// js/main.js

// ─────────── Web-Worker Pathfinding Setup (POOL) ───────────
let _pfMsgId = 0;
// id -> resolve() for results coming back from any worker
const _pfPending = new Map();

/**
 * Minimal worker pool that feeds jobs to idle workers.
 * We keep your existing "message id" pattern so the rest of the code stays unchanged.
 */
class PathWorkerPool {
  constructor(url, size) {
    this.url = url;
    this.queue = [];             // pending jobs: {payload, transfer}
    this.workers = [];
    this._makeWorkers(size);
  }

  _makeWorkers(size) {
    for (let i = 0; i < size; i++) {
      const w = new Worker(this.url);
      w._busy = false;

      // When a worker finishes a job, resolve the pending promise by id,
      // mark the worker idle, and dispatch the next queued job.
      w.onmessage = (e) => {
        const { id } = e.data || {};
        const resolve = _pfPending.get(id);
        if (resolve) {
          resolve(e.data);
          _pfPending.delete(id);
        }
        w._busy = false;
        this._drain();
      };

      w.onerror = (err) => {
        console.error('[path worker] error:', err);
        // Mark idle and keep going so one worker crash doesn’t stall everything
        w._busy = false;
        this._drain();
      };

      this.workers.push(w);
    }
  }

  /**
   * Enqueue a job for the pool. The job MUST already contain a unique "id".
   * Optionally pass Transferable objects via `transfer`.
   */
  postMessage(payload, transfer) {
    this.queue.push({ payload, transfer });
    this._drain();
  }

  _drain() {
    for (const w of this.workers) {
      if (this.queue.length === 0) return;
      if (w._busy) continue;
      const job = this.queue.shift();
      w._busy = true;
      if (job.transfer && job.transfer.length) {
        w.postMessage(job.payload, job.transfer);
      } else {
        w.postMessage(job.payload);
      }
    }
  }
}

// Create a pool sized to your CPU (cap a bit so we don’t spawn dozens)
const _POOL_SIZE = Math.max(
  1,
  Math.min((navigator.hardwareConcurrency || 4), 4) // e.g., 6 cores => 6 workers (capped at 8)
);


const pathPool = new PathWorkerPool('js/pathfindingWorker.js', _POOL_SIZE);


// Map the engine's terrain strings to compact byte codes.
// Keep this in sync with the worker's mapping.
const TERRAIN_CODE = {
  PLAIN: 0, DESERT: 1, WATER: 2, MOUNTAIN: 3, FOREST: 4, SHRUB: 5, RIVER: 6, ICE: 7
};

function encodeTerrainsToU8(grid) {
  const { rows, cols } = grid;
  const A = new Uint8Array(rows * cols);
  let k = 0;
  for (let y = 0; y < rows; y++) {
    const row = grid.cells[y];
    for (let x = 0; x < cols; x++) {
      const t = row[x].terrain;
      A[k++] = TERRAIN_CODE[t] || 0; // 0 for unknown (shouldn't happen)
    }
  }
  return A;
}


// offload one empire’s cost-map job to the pool
function computeCostMapOffload(emp, grid, ownerIdFlat, penalty) {
  const id = ++_pfMsgId;

  return new Promise(resolve => {
    _pfPending.set(id, resolve);

    // NEW: pack terrains into bytes and transfer the buffer
    const terrainCodeFlat = encodeTerrainsToU8(grid);

    const payload = {
      id,
      empireId: emp.id,
      rows: grid.rows,
      cols: grid.cols,
      terrainCodeFlat,                 // <— bytes instead of string matrix
      travelSpeeds: emp.travelSpeeds,
      capital: emp.capital,

      // territory-aware penalty inputs (unchanged)
      ownerIdFlat,
      penaltyScale: penalty?.penaltyScale ?? 1.0,
      penaltyGamma: penalty?.penaltyGamma ?? 1.0
    };

    // IMPORTANT: transfer the ArrayBuffer for zero-copy
    pathPool.postMessage(payload, [terrainCodeFlat.buffer]);
  });
}
 // ───────────────────────────────────────────────────────

// --- Display density cap (for memory/CPU sanity) ---
const MAX_DPR = 1.5;
function getEffectiveDPR() {
  return Math.min(window.devicePixelRatio || 1, MAX_DPR);
}


window.currentMode = null;
window.currentHeatEmpire = null;

window.currentRouteEmpire = null;
window.currentRouteTarget = null;

window.isRecalibrating    = false;
window.recalibrateCancel  = false;

window.addEventListener('load', () => {
  // --- Cache DOM elements ---
  const canvas               = document.getElementById('mapCanvas');
  const ctx                  = canvas.getContext('2d');

  const canvasSizeSlider  = document.getElementById('canvas-size-slider');
const canvasSizeDisplay = document.getElementById('canvas-size-display');
const resizer           = document.getElementById('canvas-resizer');


// ← insert offscreen setup here:
const offscreen = document.createElement('canvas');
const offCtx    = offscreen.getContext('2d');

// View toggle buttons
const viewTerrainBtn = document.getElementById('view-terrain-btn');
const viewValueBtn   = document.getElementById('view-value-btn');

function setViewMode(mode) {
  window.viewMode = mode;
    window.renderMode = mode; // add this
  // simple visual state
  if (mode === 'value') {
    viewValueBtn.classList.add('btn-secondary');
    viewTerrainBtn.classList.remove('btn-secondary');
  } else {
    viewTerrainBtn.classList.add('btn-secondary');
    viewValueBtn.classList.remove('btn-secondary');
  }
  renderBackground();
  //drawCurrent();
}

viewTerrainBtn?.addEventListener('click', () => setViewMode('terrain'));
viewValueBtn?.addEventListener('click',   () => setViewMode('value'));



// Recalibrate button (keep icon; only change the label text)
const recBtn   = document.getElementById('recalibrate-btn');
const recLabel = recBtn?.querySelector('.label');

recBtn.addEventListener('click', () => {
  if (!window.isRecalibrating) {
    // start recalibration
    window.isRecalibrating   = true;
    window.recalibrateCancel = false;

    if (recLabel) recLabel.textContent = 'Stop Recalibrate';
    recBtn.classList.add('is-spinning');             // optional: spin animation
    recBtn.setAttribute('aria-pressed', 'true');     // optional a11y

    recalibrateTerritory()
      .catch(() => {}) // ignore cancellation “errors”
      .finally(() => {
        // reset button when done or cancelled
        window.isRecalibrating   = false;
        window.recalibrateCancel = false;

        if (recLabel) recLabel.textContent = 'Recalibrate';
        recBtn.classList.remove('is-spinning');
        recBtn.removeAttribute('aria-pressed');
      });
  } else {
    // request cancellation
    window.recalibrateCancel = true;
    if (recLabel) recLabel.textContent = 'Stopping…';
  }
});

// --- Auto-Grow amount wiring (cells per tick) ---
window.autoGrowAmount = 5;

const autoGrowAmtSlider = document.getElementById('auto-grow-speed');
const autoGrowAmtVal    = document.getElementById('auto-grow-speed-val');

if (autoGrowAmtSlider && autoGrowAmtVal) {
  window.autoGrowAmount = parseInt(autoGrowAmtSlider.value, 10);
  autoGrowAmtVal.textContent = String(window.autoGrowAmount);

  autoGrowAmtSlider.addEventListener('input', () => {
    window.autoGrowAmount = parseInt(autoGrowAmtSlider.value, 10);
    autoGrowAmtVal.textContent = String(window.autoGrowAmount);
    // No need to restart the timer; next tick uses the new amount automatically
  });
}

// Include-enemy-territory-in-heatmap checkbox
(function wireIncludeEnemyHeatmap(){
  const cb = document.getElementById('include-enemy-heatmap');
  if (!cb) return;
  // default off
  window.includeEnemyHeatmap = cb.checked;

  cb.addEventListener('change', () => {
    window.includeEnemyHeatmap = cb.checked;
    // Recompute the heatmap’s ranks & redraw (cost maps don’t need recompute)
    simulateAndDraw();
  });
})();






function resizeCanvases() {
  offscreen.width  = canvas.width;
  offscreen.height = canvas.height;
}



  
  const paintSelect          = document.getElementById('paint-select');
  const slidersDiv           = document.getElementById('sliders');
  const rndBtn               = document.getElementById('randomize-btn');
  const exportTerrainBtn     = document.getElementById('export-terrain-btn');
  const importTerrainInput   = document.getElementById('import-terrain-input');
  const toggleGridBtn        = document.getElementById('toggle-grid-btn');

  const gridWidthSlider  = document.getElementById('grid-width-slider');
const gridWidthDisplay = document.getElementById('grid-width-display');
const gridHeightSlider  = document.getElementById('grid-height-slider');
const gridHeightDisplay = document.getElementById('grid-height-display');

let gridWidth  = parseInt(gridWidthSlider.value, 10);
let gridHeight = parseInt(gridHeightSlider.value, 10);

gridWidthSlider.addEventListener('input', () => {
  gridWidth = parseInt(gridWidthSlider.value, 10);
if (gridWidthDisplay)  gridWidthDisplay.textContent  = cols;
  rebuildGrid();
});

gridHeightSlider.addEventListener('input', () => {
  gridHeight = parseInt(gridHeightSlider.value, 10);
if (gridHeightDisplay) gridHeightDisplay.textContent = rows;
  rebuildGrid();
});


// Mouse hover info

// grab the checkbox (now in the HTML)
const infoCheckbox = document.getElementById('info-mode-checkbox');
window.infoMode = false;

// toggle the global flag when clicked
infoCheckbox.addEventListener('change', e => {
  window.infoMode = e.target.checked;
  tooltip.style.opacity = '0';
});


// ————— Tooltip setup —————
const tooltip = document.createElement('div');
tooltip.style.position        = 'absolute';
tooltip.style.pointerEvents   = 'none';
tooltip.style.padding         = '4px 8px';
tooltip.style.background      = 'rgba(0,0,0,0.7)';
tooltip.style.color           = '#fff';
tooltip.style.borderRadius    = '4px';
tooltip.style.fontSize        = '12px';
tooltip.style.transition      = 'opacity 0.1s';
tooltip.style.opacity         = '0';
tooltip.style.whiteSpace      = 'nowrap';
document.body.appendChild(tooltip);


// Helper for scheduling terrain shading
let shadeQueued = false;
function scheduleTerrainShading() {
  if (shadeQueued) return;
  shadeQueued = true;
  requestAnimationFrame(() => {
    // only recompute what’s needed
    if (window.precomputeWaterShading) window.precomputeWaterShading(grid, canvas.width, canvas.height);
    if (window.computeMountainDepth)   window.computeMountainDepth(grid);
    renderBackground();
    //drawCurrent();
    shadeQueued = false;
  });
}

// Helper: rebuild the offscreen background once next frame (used by Value sliders)
let valueRebuildQueued = false;
function scheduleValueRebuild() {
  if (valueRebuildQueued) return;
  valueRebuildQueued = true;
  requestAnimationFrame(() => {
    renderBackground();          // rebuilds terrain OR value, depending on renderMode
    valueRebuildQueued = false;
  });
}


// helper to find owner by flat index
function findOwner(idx) {
  for (const emp of EmpireManager.empires) {
    if (emp.territory.has(idx)) return emp.name;
  }
  return 'None';
}

// Returns 0..100 (percentile) for the currently shown heatmap, or null if not applicable
function heatPercentAt(col, row) {
  const info = window.__heatRank;          // { empId, rank: Float32Array }
  const emp  = window.currentHeatEmpire;   // set by drawHeatmap(emp)
  if (!info || !emp || info.empId !== emp.id) return null;

  const idx = row * grid.cols + col;

  // Hostile cells are black; don't show a percent for those
  const owner = findOwner(idx);
  if (owner !== 'None' && owner !== emp.name) return null;

  const t = info.rank[idx];                // 0..1 or -1 if not in the ranked set
  if (t == null || t < 0) return null;

  return Math.round(t * 100);
}

// View: Terrain vs Value
(function wireViewButtons(){
  const btnTerr = document.getElementById('view-terrain-btn');
  const btnVal  = document.getElementById('view-value-btn');

  function reflectButtons() {
    if (!btnTerr || !btnVal) return;
    if (window.renderMode === 'value') {
      btnVal.classList.remove('btn-secondary');
      btnTerr.classList.add('btn-secondary');
    } else {
      btnTerr.classList.remove('btn-secondary');
      btnVal.classList.add('btn-secondary');
    }
  }

  btnTerr?.addEventListener('click', () => {
    window.renderMode = 'terrain';
    // rebuild offscreen terrain layer (value mode doesn't use it)
    renderBackground();
    //drawCurrent();
    reflectButtons();
  });
  btnVal?.addEventListener('click', () => {
    window.renderMode = 'value';
    // value view draws directly; no offscreen needed
    //drawCurrent();
    reflectButtons();
  });

  reflectButtons();
})();

// ——— Help popovers ———
(function helpPopovers(){
  const HELP = {
    canvas:       { title: 'Canvas', text: 'Resize the pixel canvas and toggle grid/labels. This does not change the simulation grid (rows × cols).' },
    terrain:      { title: 'Terrain', text: 'Load or randomize terrains, paint cells, and place an overlay guide. Rows × cols set the simulation grid.' },
    empires:      { title: 'Empires', text: 'Add empires, set sizes and travel costs, and tweak hostile-territory penalty and heatmap options.' },
    value:        { title: 'Value', text: 'Land-value layer: view, paint numeric values, import/export, or set terrain-wide defaults.' },
    hostile:        { title: 'Hostile penalty', text: 'How much pathing through enemy territory should be penalized.' },
    empiresload:        { title: 'Load / export empires', text: 'Load empire capitals and characteristics from file, or export. Can also export ownership grid.' },
    valuesliders:        { title: 'Value sliders', text: 'Change the land value of all terrain of a certain type.' },
    terrainmodification:        { title: 'Terrain modification', text: 'Paint terrain type or randomize a completely new map. Warning: Randomizing or changing grid size erases current map.' },
    optimizationdetail:        { title: 'Empire optimization', text: 'Vary capital locations or sliders to maximize territory or land value. Slider step determine how much sliders should change each attempt.' },
    growthdetail:        { title: 'Set threshold for land value that causes growth. Empire size becomes land value divided by growth threshold.' },
    optimization: { title: 'Optimization', text: 'Recalibrate recomputes territories fully. Optimization varies capital locations or empire characteristics to improve territory or land value. ' }
  };

  let pop;
  function closePop(){ if(pop){ pop.remove(); pop=null; } }

  document.addEventListener('click', (e) => {
    const btn = e.target.closest('.help-icon');
    if (!btn) return closePop();

    e.stopPropagation();
    const key = btn.getAttribute('data-help');
    const data = HELP[key];
    if (!data) return;

    closePop();
    pop = document.createElement('div');
    pop.className = 'help-pop';
    pop.innerHTML = `<h4>${data.title}</h4><p>${data.text}</p>`;
    document.body.appendChild(pop);

    const r = btn.getBoundingClientRect();
    const x = r.left + window.scrollX + r.width + 10;
    const y = r.top  + window.scrollY - 4;
    pop.style.left = x + 'px';
    pop.style.top  = y + 'px';
  });

  window.addEventListener('scroll', closePop, true);
})();

// —— Value paint toggle (Start/Stop), radio-free ——
(function valuePaintToggle(){
  const btn   = document.getElementById('start-value-paint-btn');
  const pmBtn = document.getElementById('paint-mode-btn'); // the global "Enable/Disable Paint Mode" button
  const isPaintingOn = () => window.currentMode === 'paint';

  if (!btn || !pmBtn) return;

  let active = false;

  function setBtn(on){
    active = !!on;
    btn.textContent = active ? 'Stop Value Painting' : 'Start Value Painting';
    btn.classList.toggle('btn-danger',    active);
    btn.classList.toggle('btn-secondary', !active);
    btn.setAttribute('aria-pressed', String(active));
  }

  function enableValuePaint(){
    // switch view to Value (optional, makes painting clearer)
    if (typeof window.renderMode !== 'undefined') {
      window.renderMode = 'value';
      renderBackground?.(); drawCurrent?.();
    }
    // ensure global paint mode is ON
    if (!isPaintingOn()) pmBtn.click();
    // force paint target to "value"
    setPaintMode('value');
    setBtn(true);
  }

  function disablePainting(){
    // turn global paint mode OFF & revert target to terrain as default
if (isPaintingOn()) pmBtn.click();
    setPaintMode('terrain');
    setBtn(false);
  }

  // Main toggle
  btn.addEventListener('click', () => (active ? disablePainting() : enableValuePaint()));

  // Keep the button in sync if user clicks the global Paint Mode button directly
  function syncFromUI(){
const paintingOn  = isPaintingOn();
    setBtn(paintingOn && window.paintMode === 'value');
    // if painting turned off externally, reset target to terrain
    if (!paintingOn && window.paintMode === 'value') setPaintMode('terrain');
  }
  pmBtn.addEventListener('click', () => setTimeout(syncFromUI, 0));

  // initial state
  syncFromUI();
})();



// ————— Mouse handlers —————
canvas.addEventListener('mousemove', e => {
  const rect = canvas.getBoundingClientRect();
  // convert to canvas‐space
  const x = ((e.clientX - rect.left) * (canvas.width  / rect.width));
  const y = ((e.clientY - rect.top ) * (canvas.height / rect.height));
  const col = Math.floor(x / (canvas.width  / grid.cols));
  const row = Math.floor(y / (canvas.height / grid.rows));
  if (col < 0 || col >= grid.cols || row < 0 || row >= grid.rows) {
    tooltip.style.opacity = '0';
    return;
  }

    if (!window.infoMode) {
    tooltip.style.opacity = '0';
    return;
  }

  const idx     = row * grid.cols + col;
  const terr    = grid.cells[row][col].terrain;
  const owner   = findOwner(idx);
 const pct = heatPercentAt(col, row);
tooltip.innerHTML = `
  <strong>Owner:</strong> ${owner}<br/>
  <strong>Terrain:</strong> ${terr}${
    (pct == null ? '' : `<br/><strong>Heatmap:</strong> ${pct.toFixed(0)}%`)
  }
`;
  // position tooltip slightly offset from mouse
  tooltip.style.left    = e.pageX + 10 + 'px';
  tooltip.style.top     = e.pageY + 10 + 'px';
  tooltip.style.opacity = '1';
});

canvas.addEventListener('mouseleave', () => {
  tooltip.style.opacity = '0';
});

function autoGrowTick() {
  const emps = EmpireManager.empires;
  if (emps.length === 0) return;

  const emp = emps[autoGrowIndex % emps.length];
  emp.size += (window.autoGrowAmount || 5);

  // keep UI in sync
  if (emp._sizeSlider) emp._sizeSlider.value = emp.size;
  if (emp._sizeInput)  emp._sizeInput.value  = emp.size;

  simulateAndDraw();
  autoGrowIndex++;
}


const panelsContainer = document.createElement('div');
panelsContainer.id = 'empire-panels';
// insert this just below your static Add Empire button in the #controls div:
document
  .getElementById('add-empire-btn')
  .parentNode
  .insertAdjacentElement('afterend', panelsContainer);

// Add Empire
document
  .getElementById('add-empire-btn')
  .addEventListener('click', () => {
    const emp = EmpireManager.addEmpire();
    createEmpirePanel(emp);


  // NEW: default travel costs from global settings + sync the panel UI
  if (window.globalTravelSpeeds) emp.travelSpeeds = deepClone(window.globalTravelSpeeds);
  for (const k of TERRAIN_KEYS) applySliderToUI(emp, k);

    window.currentMode   = 'placeCapital';
    window.currentEmpire = emp;
    //alert(`Click on the map to place the capital for '${emp.name}'`);
  });

// Auto-Grow start/stop
let autoGrowInterval = null;
let autoGrowIndex = 0;

document.getElementById('auto-grow-btn').addEventListener('click', function() {
  const btn = this;
  if (!autoGrowInterval) {
    autoGrowInterval = setInterval(autoGrowTick, 50); // fixed cadence
    btn.textContent = 'Stop Auto-Grow';
  } else {
    clearInterval(autoGrowInterval);
    autoGrowInterval = null;
    btn.textContent = 'Auto-Grow Empires';
  }
});





// ─────────── Optimization (Placement & Sliders) ───────────

// Globals you can tweak later or via the UI below
window.optimizeDelta = 0.20;   // slider change per round (cost units)
window.optimizeDelayMs = 0;    // pause between rounds

let optimizingPlacement = false;
let optimizingSliders   = false;

// --- Adjust Size ←→ Land Value state ---
window.growthThreshold = 10;        // default; synced to the slider
let adjustingSizes = false;         // loop flag for "Adjust size to land value"
let comboAdjustOptimize = false;    // loop flag for the combo mode

// Which value-optimizers to run inside the combo loop
let runPlacementValue = true;
let runSlidersValue   = true;

let runAdjustSize     = true;       // ← new: toggle “Adjust size ↔ value”




const dirs8 = [
  {dx:  1, dy:  0}, {dx: -1, dy:  0}, {dx: 0, dy:  1}, {dx: 0, dy: -1},
  {dx:  1, dy:  1}, {dx:  1, dy: -1}, {dx: -1, dy: 1}, {dx: -1, dy: -1},
];

function sleep(ms){ return new Promise(r => setTimeout(r, ms)); }
function clamp(v, lo, hi){ return Math.max(lo, Math.min(hi, v)); }
function deepClone(obj){ return JSON.parse(JSON.stringify(obj)); }


function updateComboButtonLabel() {
  const btn = document.getElementById('combo-adjust-optimize-btn');
  if (!btn) return;
  const parts = [];
  if (runAdjustSize)     parts.push('Adjust');
  if (runPlacementValue) parts.push('Placement');
  if (runSlidersValue)   parts.push('Sliders');
  btn.textContent = parts.length ? `Run: ${parts.join(' + ')}` : 'Run: (choose options)';
}


// UI sliders for optimization step & delay
(function wireOptimizeControls(){
  const step = document.getElementById('opt-step');
  const stepVal = document.getElementById('opt-step-val');
  const dly = document.getElementById('opt-delay');
  const dlyVal = document.getElementById('opt-delay-val');

  if (step && stepVal) {
    window.optimizeDelta = parseFloat(step.value);
    stepVal.textContent = window.optimizeDelta.toFixed(2);
    step.addEventListener('input', () => {
      window.optimizeDelta = parseFloat(step.value);
      stepVal.textContent = window.optimizeDelta.toFixed(2);
    });
  }
  if (dly && dlyVal) {
    window.optimizeDelayMs = parseInt(dly.value, 10) || 0;
    dlyVal.textContent = String(window.optimizeDelayMs);
    dly.addEventListener('input', () => {
      window.optimizeDelayMs = parseInt(dly.value, 10) || 0;
      dlyVal.textContent = String(window.optimizeDelayMs);
    });
  }
})();

// Growth-threshold slider
(function wireGrowthThreshold(){
  const s = document.getElementById('growth-threshold');
  const out = document.getElementById('growth-threshold-val');
  if (!s || !out) return;

  window.growthThreshold = parseInt(s.value || '10', 10);
  out.textContent = String(window.growthThreshold);

  s.addEventListener('input', () => {
    window.growthThreshold = Math.max(1, Math.min(61, parseInt(s.value || '10', 10)));
    out.textContent = String(window.growthThreshold);
  });
})();

// Helper: update one empire's capital label if available
function updateCapitalLabel(emp) {
  if (emp._capitalDisplay && emp.capital) {
    emp._capitalDisplay.textContent = `Capital: (${emp.capital.x},${emp.capital.y})`;
  }
}

// ---- Placement ROUND ----
async function placementRound() {
  const emps = EmpireManager.empires;
  if (!emps.length) return;

  // Snapshot previous capitals and territory sizes
  const prevCaps  = emps.map(e => e.capital ? {x: e.capital.x, y: e.capital.y} : null);
  const prevSizes = emps.map(e => e.territory ? e.territory.size : 0);

  // Propose moves for everyone (simultaneous proposals)
  for (let i = 0; i < emps.length; i++) {
    const e = emps[i];
    if (!e.capital) continue;

    // Try up to 8 random directions to find a valid non-water cell
    let tried = 0, moved = false;
    const order = dirs8.slice().sort(() => Math.random() - 0.5);
    while (tried < order.length && !moved) {
      const {dx, dy} = order[tried++];
      const nx = clamp(e.capital.x + dx, 0, grid.cols - 1);
      const ny = clamp(e.capital.y + dy, 0, grid.rows - 1);
      if (grid.cells[ny][nx].terrain === 'WATER') continue; // cannot place on water
      // accept proposal (for now)
      e.capital = { x: nx, y: ny };
      updateCapitalLabel(e);
      moved = true;
    }
    // If we couldn't find a legal neighbor, we keep the old capital
  }

  // Recalculate with all proposals applied
  await simulateAndDraw();

  // Accept/reject per-empire
  let anyAccepted = false;
  for (let i = 0; i < emps.length; i++) {
    const e = emps[i];
    const before = prevSizes[i];
    const after  = e.territory ? e.territory.size : 0;

// If equal or better accept
    if (after < before) {
      // revert this capital
      if (prevCaps[i]) {
        e.capital = { x: prevCaps[i].x, y: prevCaps[i].y };
        updateCapitalLabel(e);
      }
    } else {
      anyAccepted = true;
    }
  }

  // Final recompute to reflect the mixed accepted/reverted state
  await simulateAndDraw();

  return anyAccepted;
}


// Optimization of placement for value
async function placementRoundValue() {
  const emps = EmpireManager.empires;
  if (!emps.length) return;

  // Baseline values before proposals
  const prevCaps  = emps.map(e => e.capital ? {x:e.capital.x, y:e.capital.y} : null);
  const prevVals  = emps.map(e => e._value || 0);

  // Propose random 8-neighbor step (no WATER)
  for (const e of emps) {
    if (!e.capital) continue;
    const order = dirs8.slice().sort(() => Math.random() - 0.5);
    for (const {dx,dy} of order) {
      const nx = clamp(e.capital.x + dx, 0, grid.cols - 1);
      const ny = clamp(e.capital.y + dy, 0, grid.rows - 1);
      if (grid.cells[ny][nx].terrain !== 'WATER') { e.capital = {x:nx,y:ny}; break; }
    }
    updateCapitalLabel(e);
  }

  // Recompute → this refreshes e._value via computeEmpireTotals()
  await simulateAndDraw();

  // Accept if value strictly increased, otherwise revert
  let anyAccepted = false;
  for (let i = 0; i < emps.length; i++) {
    const e = emps[i];
    const after = e._value || 0;
    if (after < prevVals[i]) {
      if (prevCaps[i]) e.capital = prevCaps[i];
      updateCapitalLabel(e);
    } else {
      anyAccepted = true;
    }
  }

  // Show mixed state
  await simulateAndDraw();
  return anyAccepted;
}


// Terrain-specific default travel costs (lower = faster)
const DEFAULT_TRAVEL_SPEEDS = {
  PLAIN:    3,
  DESERT:   10,
  WATER:    2,
  MOUNTAIN: 8,
  FOREST:   5,
  SHRUB:    4,
  RIVER:    1,
  ICE:      10
};

// ---- Sliders ROUND ----
const TERRAIN_KEYS = ['PLAIN','DESERT','WATER','MOUNTAIN','FOREST', 'SHRUB', 'RIVER','ICE'];

function applySliderToUI(emp, key) {
  const s = emp._speedSliders?.[key];
  const v = emp._speedValues?.[key];
  const formatted = Number(emp.travelSpeeds[key]).toFixed(1);
  if (s) s.value = emp.travelSpeeds[key];
  if (v) {
    if (v.tagName === 'INPUT') v.value = formatted;
    else v.textContent = formatted;
  }
}

async function slidersRound() {
  const emps = EmpireManager.empires;
  if (!emps.length) return;

  const delta = window.optimizeDelta || 0.2;

  // Snapshot old speeds and sizes
  const prevSpeeds = emps.map(e => deepClone(e.travelSpeeds));
  const prevSizes  = emps.map(e => e.territory ? e.territory.size : 0);

  // Propose one up + one down for each empire
  for (let i = 0; i < emps.length; i++) {
    const e = emps[i];
    // pick two distinct terrain keys
    const keys = TERRAIN_KEYS.slice().sort(() => Math.random() - 0.5);
    const downKey = keys[0]; // lower cost (i.e., better/faster)
    let upKey = keys[1];
    if (upKey === downKey) upKey = keys[2];

    // Note: travelSpeeds are "costs": lower is faster.
    e.travelSpeeds[downKey] = clamp(+e.travelSpeeds[downKey] - delta, 0.1, 10);
    e.travelSpeeds[upKey]   = clamp(+e.travelSpeeds[upKey]   + delta, 0.1, 10);

    // keep UI in sync
    applySliderToUI(e, downKey);
    applySliderToUI(e, upKey);
  }

  // Recompute with all proposals
  await simulateAndDraw();

  // Accept/reject per-empire
  let anyAccepted = false;
  for (let i = 0; i < emps.length; i++) {
    const e = emps[i];
    const before = prevSizes[i];
    const after  = e.territory ? e.territory.size : 0;
    if (after < before) {
      // revert all speeds for this empire
      e.travelSpeeds = prevSpeeds[i];
      // sync entire UI row for this empire
      for (const k of TERRAIN_KEYS) applySliderToUI(e, k);
    } else {
      anyAccepted = true;
    }
  }

  // Final recompute to reflect accepted/reverted settings
  await simulateAndDraw();

  return anyAccepted;
}


// Optimize sliders for value
async function slidersRoundValue() {
  const emps = EmpireManager.empires;
  if (!emps.length) return;

  const delta = window.optimizeDelta || 0.2;
  const prevSpeeds = emps.map(e => deepClone(e.travelSpeeds));
  const prevVals   = emps.map(e => e._value || 0);

  for (const e of emps) {
    const keys = TERRAIN_KEYS.slice().sort(() => Math.random() - 0.5);
    const downKey = keys[0];
    const upKey   = keys.find(k => k !== downKey) || keys[1];
    e.travelSpeeds[downKey] = clamp(+e.travelSpeeds[downKey] - delta, 0.1, 10);
    e.travelSpeeds[upKey]   = clamp(+e.travelSpeeds[upKey]   + delta, 0.1, 10);
    applySliderToUI(e, downKey);
    applySliderToUI(e, upKey);
  }

  await simulateAndDraw();          // refresh e._value

  let anyAccepted = false;
  for (let i = 0; i < emps.length; i++) {
    const e = emps[i];
    const after = e._value || 0;
    if (after < prevVals[i]) {
      e.travelSpeeds = prevSpeeds[i];            // revert
      for (const k of TERRAIN_KEYS) applySliderToUI(e, k);
    } else {
      anyAccepted = true;
    }
  }

  await simulateAndDraw();
  return anyAccepted;
}



// Global default speeds used for "apply to all" + new empires."
// Initialize from the first empire if present; otherwise from terrain-specific defaults.
window.globalTravelSpeeds = (function seedGlobals(){
  const first = (window.EmpireManager && EmpireManager.empires && EmpireManager.empires[0]) || null;
  if (first && first.travelSpeeds) return { ...first.travelSpeeds };
  return { ...DEFAULT_TRAVEL_SPEEDS };
})();

function setGlobalSpeed(key, v) {
  v = Math.max(0.1, Math.min(10, Math.round(parseFloat(v||0)*10)/10));
  window.globalTravelSpeeds[key] = v;

  // Push to all existing empires + keep their UI in sync
  if (window.EmpireManager && EmpireManager.empires) {
    for (const e of EmpireManager.empires) {
      e.travelSpeeds[key] = v;
      applySliderToUI(e, key);
    }
  }

  // Recompute territories so users see the effect
  if (typeof simulateAndDraw === 'function') simulateAndDraw();
}

function buildGlobalSpeedSliders() {
  const wrap = document.getElementById('global-speed-sliders');
  if (!wrap) return;
  wrap.innerHTML = '';

  for (const t of TERRAIN_KEYS) {
    // layout: label | slider | number (same as empire panel)
    const row = document.createElement('label');
    row.style.display = 'grid';
    row.style.gridTemplateColumns = '90px 1fr 56px';
    row.style.alignItems = 'center';
    row.style.columnGap  = '8px';
    row.style.margin     = '4px 0';

    const name = document.createElement('span');
    name.textContent = t[0] + t.slice(1).toLowerCase();

    const slider = document.createElement('input');
    slider.type  = 'range';
    slider.min   = '0.1';
    slider.max   = '10';
    slider.step  = '0.1';
    slider.value = String(window.globalTravelSpeeds[t]);
    slider.style.width  = '100%';
    slider.style.margin = '0';

    const num = document.createElement('input');
    num.type   = 'number';
    num.min    = '0.1';
    num.max    = '10';
    num.step   = '0.1';
    num.value  = Number(window.globalTravelSpeeds[t]).toFixed(1);
    num.style.width = '56px';
    num.style.textAlign = 'right';

    function apply(v) {
      setGlobalSpeed(t, v);
      slider.value = String(window.globalTravelSpeeds[t]);
      num.value    = Number(window.globalTravelSpeeds[t]).toFixed(1);
    }

    slider.addEventListener('input', () => apply(slider.value));
    num.addEventListener('change',   () => apply(num.value));

    row.append(name, slider, num);
    wrap.appendChild(row);
  }
}

// Build once when the DOM is ready (call after your constants exist)
buildGlobalSpeedSliders();



// ── Global Target Size (logarithmic) ──────────────────────────────
(function wireGlobalTargetSize(){
  const s   = document.getElementById('global-size-slider');
  const out = document.getElementById('global-size-display');
  if (!s || !out || !window.sliderToSize || !window.sizeToSlider) return;

  // Init to geometric-mean-ish: use first empire if present, else 50
  const init = (window.EmpireManager?.empires?.[0]?.size) || 50;
  s.value = String(sizeToSlider(init));
  out.textContent = String(init);

  function applyAll(size) {
    out.textContent = String(size);
    for (const e of EmpireManager.empires) {
      e.size = size;
      // keep each panel in sync (log slider!)
      if (e._sizeInput)  e._sizeInput.value  = String(size);
      if (e._sizeSlider) e._sizeSlider.value = String(sizeToSlider(size));
    }
    simulateAndDraw(); drawCurrent();
  }

  s.addEventListener('input', () => applyAll(sliderToSize(s.value)));
})();


// Random empires
function pickRandomLandCells(n) {
  const land = [];
  for (let y = 0; y < grid.rows; y++) {
    for (let x = 0; x < grid.cols; x++) {
      if (grid.cells[y][x].terrain !== 'WATER') land.push({x,y});
    }
  }
  // exclude already-occupied capitals
  const occ = new Set();
  if (window.EmpireManager) {
    for (const e of EmpireManager.empires) {
      if (e.capital) occ.add(e.capital.x + ',' + e.capital.y);
    }
  }
  // Fisher–Yates shuffle
  for (let i = land.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [land[i], land[j]] = [land[j], land[i]];
  }
  const out = [];
  for (const c of land) {
    const key = c.x + ',' + c.y;
    if (!occ.has(key)) {
      out.push(c);
      if (out.length >= n) break;
    }
  }
  return out;
}

async function addRandomEmpires(n) {
  n = Math.max(1, Math.min(20, n|0));
  const cells = pickRandomLandCells(n);
  if (!cells.length) return;

  for (let i = 0; i < cells.length; i++) {
    const emp = EmpireManager.addEmpire();
    createEmpirePanel(emp);

    // default travel costs from global settings + sync the panel UI
    if (window.globalTravelSpeeds) emp.travelSpeeds = deepClone(window.globalTravelSpeeds);
    for (const k of TERRAIN_KEYS) applySliderToUI(emp, k);

    // place capital at the picked land cell
    emp.capital = { x: cells[i].x, y: cells[i].y };
    updateCapitalLabel(emp);
  }

  // compute territories for the new set
  await simulateAndDraw();
}

// Wire the button
(function wireRandomEmpireAdder(){
  const btn = document.getElementById('add-random-empires-btn');
  const num = document.getElementById('random-empires-count');
  if (!btn || !num) return;
  btn.addEventListener('click', () => {
    const n = parseInt(num.value || '1', 10);
    addRandomEmpires(n);
  });
})();


// ---- Loop drivers & buttons ----
async function runPlacementOptimizeLoop(btn) {
  optimizingPlacement = true;
  btn.textContent = 'Stop Optimize Placement';
  try {
    while (optimizingPlacement) {
      await placementRound();
      if (window.optimizeDelayMs) await sleep(window.optimizeDelayMs);
    }
  } finally {
    optimizingPlacement = false;
    btn.textContent = 'Optimize Placement';
  }
}

async function runSlidersOptimizeLoop(btn) {
  optimizingSliders = true;
  btn.textContent = 'Stop Optimize Sliders';
  try {
    while (optimizingSliders) {
      await slidersRound();
      if (window.optimizeDelayMs) await sleep(window.optimizeDelayMs);
    }
  } finally {
    optimizingSliders = false;
    btn.textContent = 'Optimize Sliders';
  }
}

async function runAdjustSizesLoop(btn) {
  adjustingSizes = true;
  btn.textContent = 'Stop Adjust (Value ↔ Size)';
  try {
    let iter = 0;
    const MAX_ITERS = 200;

    while (adjustingSizes && iter++ < MAX_ITERS) {
      // Ensure e._value is up-to-date
      await simulateAndDraw();

      // One pass of size = round(value / threshold)
      const changed = adjustSizesOnceFromValue(window.growthThreshold);

      // If nothing changed, we’ve converged
      if (!changed) break;

      // Reflect the new target sizes on the map
      await simulateAndDraw();

      if (window.optimizeDelayMs) await sleep(window.optimizeDelayMs);
    }
  } finally {
    adjustingSizes = false;
    btn.textContent = 'Adjust size to land value';
  }
}

async function runComboAdjustOptimizeLoop(btn) {
  // Don’t start if nothing is selected
  if (!runAdjustSize && !runPlacementValue && !runSlidersValue) {
    alert('Choose at least one option: Adjust, Placement, or Sliders.');
    return;
  }

  comboAdjustOptimize = true;
  btn.textContent = 'Stop';
  try {
    while (comboAdjustOptimize) {
      // Always work off current state
      await simulateAndDraw();

      // 1) Optional: size step from land value
      if (runAdjustSize) {
        adjustSizesOnceFromValue(window.growthThreshold);
        await simulateAndDraw();
      }

      // 2) Optional: run value-based optimizers
      if (runPlacementValue) await placementRoundValue();
      if (runSlidersValue)   await slidersRoundValue();

      if (window.optimizeDelayMs) await sleep(window.optimizeDelayMs);
    }
  } finally {
    comboAdjustOptimize = false;
    updateComboButtonLabel(); // restore label to “Run: …”
  }
}

// Wire buttons
(function wireOptimizeButtons(){
  const pBtn = document.getElementById('opt-placement-btn');
  const sBtn = document.getElementById('opt-sliders-btn');
  if (pBtn) {
    pBtn.addEventListener('click', async () => {
      if (!optimizingPlacement) {
        // If slider optimization is running, stop it first
        optimizingSliders = false;
        await runPlacementOptimizeLoop(pBtn);
      } else {
        optimizingPlacement = false;
      }
    });
  }
  if (sBtn) {
    sBtn.addEventListener('click', async () => {
      if (!optimizingSliders) {
        // If placement optimization is running, stop it first
        optimizingPlacement = false;
        await runSlidersOptimizeLoop(sBtn);
      } else {
        optimizingSliders = false;
      }
    });
  }
})();


// Wire "Adjust size to land value" and "Adjust ↔ Optimize (Value)"
(function wireAdjustButtons(){
  const adjustBtn = document.getElementById('adjust-size-to-value-btn');
  const comboBtn  = document.getElementById('combo-adjust-optimize-btn');
  if (!adjustBtn && !comboBtn) return;

  // Convenience: if any other loops are running, stop them before starting ours
  function stopOthers() {
    optimizingPlacement = false;
    optimizingSliders = false;
    optimizingPlacementValue = false;
    optimizingSlidersValue = false;
  }

  adjustBtn?.addEventListener('click', async () => {
    if (!adjustingSizes) {
      stopOthers();
      await runAdjustSizesLoop(adjustBtn);
    } else {
      adjustingSizes = false;
    }
  });

  comboBtn?.addEventListener('click', async () => {
    if (!comboAdjustOptimize) {
      stopOthers();
      await runComboAdjustOptimizeLoop(comboBtn);
    } else {
      comboAdjustOptimize = false;
    }
  });
})();


// Combo-loop toggles
(function wireComboToggles(){
  const cAdjust    = document.getElementById('run-adjust-size');
  const cPlacement = document.getElementById('run-placement-value');
  const cSliders   = document.getElementById('run-sliders-value');
  if (!cPlacement || !cSliders || !cAdjust) return;

  // initial sync
  runAdjustSize     = !!cAdjust.checked;
  runPlacementValue = !!cPlacement.checked;
  runSlidersValue   = !!cSliders.checked;
  updateComboButtonLabel();

  const sync = () => {
    runAdjustSize     = !!cAdjust.checked;
    runPlacementValue = !!cPlacement.checked;
    runSlidersValue   = !!cSliders.checked;
    updateComboButtonLabel();
  };

  cAdjust.addEventListener('change', sync);
  cPlacement.addEventListener('change', sync);
  cSliders.addEventListener('change',   sync);
})();


// Import Empires (button opens the hidden file input)
const importEmpiresBtn   = document.getElementById('import-empires-btn');
const importEmpiresInput = document.getElementById('import-empires-input');
if (importEmpiresBtn && importEmpiresInput) {
  importEmpiresBtn.addEventListener('click', () => importEmpiresInput.click());
}



let optimizingPlacementValue = false;
let optimizingSlidersValue   = false;

async function runPlacementOptimizeLoopValue(btn) {
  optimizingPlacementValue = true;
  btn.textContent = 'Stop Optimize Placement (Value)';
  try {
    while (optimizingPlacementValue) {
      await placementRoundValue();
      if (window.optimizeDelayMs) await sleep(window.optimizeDelayMs);
    }
  } finally {
    optimizingPlacementValue = false;
    btn.textContent = 'Optimize Placement (Value)';
  }
}

async function runSlidersOptimizeLoopValue(btn) {
  optimizingSlidersValue = true;
  btn.textContent = 'Stop Optimize Sliders (Value)';
  try {
    while (optimizingSlidersValue) {
      await slidersRoundValue();
      if (window.optimizeDelayMs) await sleep(window.optimizeDelayMs);
    }
  } finally {
    optimizingSlidersValue = false;
    btn.textContent = 'Optimize Sliders (Value)';
  }
}

(function wireValueOptimizeButtons(){
  const pBtn = document.getElementById('opt-placement-value-btn');
  const sBtn = document.getElementById('opt-sliders-value-btn');
  if (pBtn) {
    pBtn.addEventListener('click', async () => {
      if (!optimizingPlacementValue) {
        optimizingSlidersValue = false;   // stop the other if running
        await runPlacementOptimizeLoopValue(pBtn);
      } else {
        optimizingPlacementValue = false;
      }
    });
  }
  if (sBtn) {
    sBtn.addEventListener('click', async () => {
      if (!optimizingSlidersValue) {
        optimizingPlacementValue = false;
        await runSlidersOptimizeLoopValue(sBtn);
      } else {
        optimizingSlidersValue = false;
      }
    });
  }
})();




// Hook up penalty sliders (now defined in index.html)
(function wirePenaltySliders(){
  const s = document.getElementById('penalty-scale');
  const g = document.getElementById('penalty-gamma');
  const sv = document.getElementById('penalty-scale-val');
  const gv = document.getElementById('penalty-gamma-val');

  if (!s || !g) return; // if the HTML isn't present, bail

  // Initialize globals from slider defaults
  window.penaltyScale = parseFloat(s.value);
  window.penaltyGamma = parseFloat(g.value);
  sv.textContent = window.penaltyScale.toFixed(1);
  gv.textContent = window.penaltyGamma.toFixed(1);

  const onChange = () => {
    window.penaltyScale = parseFloat(s.value);
    window.penaltyGamma = parseFloat(g.value);
    sv.textContent = window.penaltyScale.toFixed(1);
    gv.textContent = window.penaltyGamma.toFixed(1);
    simulateAndDraw();
  };

  s.addEventListener('input', onChange);
  g.addEventListener('input', onChange);
})();


  // Collapse All Empires button
document
  .getElementById('collapse-all-empires-btn')
  .addEventListener('click', () => {
    // find every <details> in the empire panels and close it
    document
      .querySelectorAll('#empire-panels details')
      .forEach(detail => detail.open = false);
  });

/// LOAD GUIDE
//  Overlay state
let overlayImage    = null;   // will hold an Image object
let overlayOpacity  = 0.5;    // default 50%

const overlayInput    = document.getElementById('overlay-input');
const opacitySlider   = document.getElementById('overlay-opacity');
const opacityDisplay  = document.getElementById('overlay-opacity-display');
const clearOverlayBtn = document.getElementById('clear-overlay-btn');

// When user picks a file, load it into an Image
overlayInput.addEventListener('change', () => {
  const file = overlayInput.files[0];
  if (!file) return;
  const reader = new FileReader();
  reader.onload = () => {
    overlayImage = new Image();
    overlayImage.onload = () => drawCurrent();
    overlayImage.src = reader.result;
  };
  reader.readAsDataURL(file);
  // reset input so they can re-import the same file if needed
  overlayInput.value = '';
});

// Adjust opacity
opacitySlider.addEventListener('input', () => {
  overlayOpacity = parseInt(opacitySlider.value, 10) / 100;
  opacityDisplay.textContent = opacitySlider.value + '%';
  drawCurrent();
});

// Clear the guide
clearOverlayBtn.addEventListener('click', () => {
  overlayImage = null;
  drawCurrent();
});

// END LOAD GUIDE


      // --- Hide Empire Names Checkbox ---
window.hideEmpireNames = false;

const hideNamesCheckbox = document.getElementById('hide-names-checkbox');
hideNamesCheckbox.addEventListener('change', e => {
  window.hideEmpireNames = e.target.checked;
  // force a redraw to immediately reflect the change
  drawCurrent();
});


// Rebuild gruid
function rebuildGrid() {
  // create new grid with width × height
  grid = new Grid(gridWidth, gridHeight);
  window.grid = grid;
  grid.initValueLayer(1);   // ← add this right after the Grid is created
window.applyAllTerrainValues();   // apply sliders to the new grid

// Keep canvas height locked to the new aspect, using current wrapper width

if (window.__resizerSnapToCurrent) { window.__resizerSnapToCurrent(); }



  // regenerate variants, shading, and redraw
  generateVariantGrid();
  computeMountainDepth(grid);
  precomputeWaterShading(grid, canvas.width, canvas.height);
  resizeCanvases();
  renderBackground();
  simulateAndDraw();
}

  // --- State ---
  let grid     = new Grid(gridWidth, gridHeight);
 window.grid  = grid;
 // add this line:
grid.initValueLayer(1);



// === Resizable wrapper → resize canvas bitmap only (no grid/rows/cols changes) ===
(function initCanvasResizer(){
  if (!resizer) return;

  // Keep the wrapper's height matching current grid aspect (rows/cols)
  function heightFromWidth(w){
    const aspect = grid.rows / grid.cols;          // fixed from current grid
    return Math.max(1, Math.round(w * aspect));
  }

  // Sync canvas bitmap to wrapper width; do NOT change grid rows/cols
  function applyCanvasSizeFromWrapper(cssW){
    const w = Math.max(100, Math.round(cssW));
    const h = heightFromWidth(w);

    // Snap and lock the wrapper’s height so it cannot be dragged independently
    resizer.style.height     = h + 'px';
    resizer.style.minHeight  = h + 'px';
    resizer.style.maxHeight  = h + 'px';

    // NEW — cap DPR and size bitmap accordingly (keep CSS size unchanged)
const dpr = getEffectiveDPR();
canvas.style.width  = w + 'px';
canvas.style.height = h + 'px';
canvas.width  = Math.round(w * dpr);
canvas.height = Math.round(h * dpr);

    // Offscreen + redraw
    resizeCanvases();
    renderBackground();
    drawCurrent();

    // Optional UI mirrors (safe even if removed in HTML)
    if (typeof canvasSizeDisplay !== 'undefined' && canvasSizeDisplay) {
      canvasSizeDisplay.textContent = String(w);
    }
    if (typeof canvasSizeSlider !== 'undefined' && canvasSizeSlider) {
      canvasSizeSlider.value = String(w);
    }
  }

  // Expose helpers so other code (terrain load / grid resize) can re-lock height
  window.__resizerApplyFromWidth = function(w){
    try { applyCanvasSizeFromWrapper(Math.round(w)); } catch (e) {}
  };
  window.__resizerSnapToCurrent = function(){
    try {
      const w = resizer.getBoundingClientRect().width || resizer.clientWidth || canvas.width;
      applyCanvasSizeFromWrapper(Math.round(w));
    } catch (e) {}
  };

  // Observe width changes (dragging the wrapper)
  const ro = new ResizeObserver(entries => {
    for (const entry of entries) {
      if (entry.target !== resizer) continue;
      applyCanvasSizeFromWrapper(Math.round(entry.contentRect.width));
    }
  });

  // Initial snap from the wrapper's current width, then observe
if (window.__resizerSnapToCurrent) { window.__resizerSnapToCurrent(); }
ro.observe(resizer);


})();




  let cellSize = canvas.width / grid.cols;

  // --- Paint Mode Toggle ---
  const paintModeBtn = document.getElementById('paint-mode-btn');
  let paintMode = false;
  paintModeBtn.addEventListener('click', () => {
    paintMode = !paintMode;
    paintModeBtn.textContent = paintMode
      ? 'Stop Terrain Painting'
      : 'Start Terrain Painting';
    window.currentMode = paintMode
      ? 'paint'
      : null;
  });

  // --- Paint Tool State ---
  let painting  = false;
  let paintType = paintSelect.value;
  paintSelect.addEventListener('change', () => {
    paintType = paintSelect.value;
  });

  // brush‐size state
const brushSizeSlider  = document.getElementById('brush-size-slider');
const brushSizeDisplay = document.getElementById('brush-size-display');
let brushSize = parseInt(brushSizeSlider.value, 10);

brushSizeSlider.addEventListener('input', () => {
  brushSize = parseInt(brushSizeSlider.value, 10);
  brushSizeDisplay.textContent = brushSize;
});

  // --- Terrain % Sliders ---
  const defaults = { PLAIN:40, DESERT:10, WATER:40, MOUNTAIN:10, FOREST:10, SHRUB:10, RIVER:5 , ICE:5};
  Object.keys(TERRAIN).forEach(type => {
    const label = document.createElement('label');
    label.textContent = `${type.charAt(0)+type.slice(1).toLowerCase()}: `;
    const input = document.createElement('input');
    input.type  = 'range'; input.min = 0; input.max = 100;
    input.value = defaults[type]; input.id = `slider-${type}`;
    const span  = document.createElement('span');
    span.textContent = input.value+'%';
    input.oninput = () => { span.textContent = input.value+'%'; };
    label.append(input, span);
    slidersDiv.append(label);
  });

  // Paint terrain
const paintHeader   = document.getElementById('paint-header');
const paintControls = document.getElementById('paint-controls');
paintHeader.addEventListener('click', () => {
  const hidden = paintControls.classList.toggle('hidden');
  paintHeader.textContent = hidden 
    ? 'Paint terrain ▶' 
    : 'Paint terrain ▼';
});



// Value paint UI
const paintModeRadios = document.querySelectorAll('input[name="paint-mode"]');
const valuePaintControls = document.getElementById('value-paint-controls');
const paintValueInput = document.getElementById('paint-value');
const paintValueChar  = document.getElementById('paint-value-char');


// Global paint mode (no radios anymore)
window.paintMode = window.paintMode || 'terrain';  // 'terrain' | 'value'
function setPaintMode(mode) {
  window.paintMode = mode === 'value' ? 'value' : 'terrain';
  const controls = document.getElementById('value-paint-controls');
  if (controls) controls.style.display = (window.paintMode === 'value') ? 'flex' : 'none';
}
function currentPaintTarget() {
  // Backward-compat with any older code that still calls this:
  return window.paintMode || 'terrain';
}
// show/hide at boot
setPaintMode(window.paintMode);

paintModeRadios.forEach(r => {
  r.addEventListener('change', () => {
    const mode = currentPaintTarget();
    valuePaintControls.style.display = (mode === 'value') ? 'flex' : 'none';
  });
});

// show "(a)" / "(b)" etc next to the numeric value
function valToCharLocal(v) {
  v = Math.max(0, Math.min(61, Math.floor(v)));
  if (v <= 9)  return String.fromCharCode(48 + v);
  if (v <= 35) return String.fromCharCode(97 + (v - 10));
  return String.fromCharCode(65 + (v - 36));
}
paintValueInput.addEventListener('input', () => {
  let v = parseInt(paintValueInput.value || '0', 10);
  v = Math.max(0, Math.min(61, v));
  paintValueInput.value = v;
  paintValueChar.textContent = `(${valToCharLocal(v)})`;
});


// Value-paint brush size (independent of terrain brush)
window.valueBrushSize = window.valueBrushSize || 1;

(function wireValueBrush(){
  const s   = document.getElementById('value-brush-size');
  const out = document.getElementById('value-brush-size-display');
  if (!s || !out) return;

  // init
  s.value = String(window.valueBrushSize);
  out.textContent = String(window.valueBrushSize);

  s.addEventListener('input', () => {
    window.valueBrushSize = Math.max(1, Math.min(25, parseInt(s.value || '1', 10)));
    out.textContent = String(window.valueBrushSize);
  });
})();



// ===== Value-by-terrain sliders =====
const terrainValueSliders = document.getElementById('terrain-value-sliders');
const resetTerrainValuesBtn = document.getElementById('reset-terrain-values');

// global map: terrain -> numeric value (0..61); default 1 for all
window.terrainValueMap = Object.fromEntries(
  Object.keys(TERRAIN).map(k => [k, 1])
);

// we already have valToCharLocal(v). Reuse it to show the symbol.
function buildTerrainValueSliders() {
  if (!terrainValueSliders) return;
  terrainValueSliders.innerHTML = '';

  for (const type of Object.keys(TERRAIN)) {
    const row = document.createElement('div');
    row.className = 'row';
    row.style.gap = '8px';
    row.style.alignItems = 'center';

    const label = document.createElement('span');
    label.style.width = '90px';
    label.textContent = type.charAt(0) + type.slice(1).toLowerCase();

    // NEW: manual number input (like "Target size")
    const num = document.createElement('input');
    num.type = 'number';
    num.min = '0';
    num.max = '61';
    num.step = '1';
    num.value = String(window.terrainValueMap[type]);
    num.style.width = '40px';

    const slider = document.createElement('input');
    slider.type = 'range';
    slider.min = '0';
    slider.max = '61';
    slider.step = '1';
    slider.value = String(window.terrainValueMap[type]);
    slider.id = `val-slider-${type}`;
    slider.style.flex = '1';

    const readout = document.createElement('span');
    const setReadout = (n) => { readout.textContent = `(${valToCharLocal(n)})`; };
    setReadout(parseInt(slider.value, 10));

    function apply(n) {
      n = Math.max(0, Math.min(61, Math.floor(n)));
      window.terrainValueMap[type] = n;
      slider.value = String(n);
      num.value = String(n);
      setReadout(n);
      applyTerrainValueToGrid(type, n);  // update map immediately
      
      scheduleValueRebuild();           // <-- rebuild offscreen on drag

      if (window.TVM_KEY) {
        try { localStorage.setItem(window.TVM_KEY, JSON.stringify(window.terrainValueMap)); } catch {}
      }
    }

    // slider behaves like your Target size slider → updates immediately
    slider.addEventListener('input', (e) => apply(e.target.value));
    // number behaves like your Target size number → commit on change
    num.addEventListener('change', (e) => apply(e.target.value));

    row.append(label, slider, num);
    terrainValueSliders.appendChild(row);
  }
}

// set the value layer for every cell that matches `type`
function applyTerrainValueToGrid(type, v) {
  const vv = Math.max(0, Math.min(61, v|0));
  for (let y = 0; y < grid.rows; y++) {
    const row = grid.cells[y];
    for (let x = 0; x < grid.cols; x++) {
      if (row[x].terrain === type) {
        grid.setValueAt(x, y, vv);
      }
    }
  }
  //renderBackground();
  //drawCurrent();
}

// helper to apply today’s settings to the entire map
window.applyAllTerrainValues = function() {
  for (const [type, v] of Object.entries(window.terrainValueMap)) {
    applyTerrainValueToGrid(type, v);
  }
};

// reset button
resetTerrainValuesBtn?.addEventListener('click', () => {
  Object.keys(window.terrainValueMap).forEach(k => (window.terrainValueMap[k] = 1));
  buildTerrainValueSliders();
  window.applyAllTerrainValues();
  scheduleValueRebuild();  

});

// build once on load
buildTerrainValueSliders();





// Randomize terrain  (null-safe)
const randHeader   = document.getElementById('randomize-header');
const randControls = document.getElementById('randomize-controls');
if (randHeader && randControls) {
  randHeader.addEventListener('click', () => {
    const hidden = randControls.classList.toggle('hidden');
    randHeader.textContent = hidden 
      ? 'Randomize terrain ▶' 
      : 'Randomize terrain ▼';
  });
}



// turn grid off by default
let showGrid = false;

// update the toggle‐button label to match (null-safe)
if (toggleGridBtn) {
  toggleGridBtn.textContent = showGrid ? 'Hide Grid Lines' : 'Show Grid Lines';
}

  // --- Variant Grid for color variation ---
  window.variantGrid = [];
  function generateVariantGrid() {
    window.variantGrid = Array.from({ length: grid.rows }, () =>
      Array.from({ length: grid.cols }, () => 0)
    );
    for (let y = 0; y < grid.rows; y++) {
      for (let x = 0; x < grid.cols; x++) {
        const t = grid.cells[y][x].terrain;
        const variants = TERRAIN_VARIANTS[t] || [TERRAIN[t].color];
        variantGrid[y][x] = Math.floor(Math.random() * variants.length);
      }
    }
  }
  // initial variant assignment
  generateVariantGrid();

  // once, at startup, for the initial terrain:
window.precomputeWaterShading(grid, canvas.width, canvas.height);

  // --- Draw Helper ---
  // render static terrain+shading into our offscreen buffer
// set a global-ish view mode
window.viewMode = window.viewMode || 'terrain';

// render static background into offscreen
function renderBackground() {
  offCtx.clearRect(0, 0, offscreen.width, offscreen.height);
  const cellSize = offscreen.width / grid.cols;
  if (window.renderMode === 'value' && window.drawValueGrid) {
    drawValueGrid(offCtx, grid, cellSize, showGrid);
  } else {
    drawGrid(offCtx, grid, cellSize, showGrid);
  }
}

function drawCurrent() {
  // 1) recompute cellSize to match current canvas & grid
  const cellSize = canvas.width / grid.cols;

  // 2) draw the cached background (terrain OR value) from offscreen
  ctx.drawImage(offscreen, 0, 0);

  if (overlayImage) {
    ctx.save();
    ctx.globalAlpha = overlayOpacity;
    // stretch guide to full canvas size
    ctx.drawImage(overlayImage, 0, 0, canvas.width, canvas.height);
    ctx.restore();
  }

  // draw territories (fills) only when *not* showing a heatmap
  if (!window.currentHeatEmpire) {
    EmpireManager.empires.forEach(emp => {
      ctx.fillStyle = emp.color;
      emp.territory.forEach(idx => {
        const x = idx % grid.cols;
        const y = Math.floor(idx / grid.cols);
        ctx.fillRect(x * cellSize, y * cellSize, cellSize, cellSize);
      });
    });
  }

  // draw thick border around each empire's territory (Path2D per empire)
  EmpireManager.empires.forEach(emp => {
    const terr = emp.territory;
    if (!terr || terr.size === 0) return;

    const path = new Path2D();
    const cols = grid.cols;

    terr.forEach(idx => {
      const x  = idx % cols;
      const y  = Math.floor(idx / cols);
      const px = x * cellSize, py = y * cellSize;

      // if neighbor is NOT owned by this empire, add that edge segment
      // top
      if (!terr.has((y - 1) * cols + x)) {
        path.moveTo(px, py);
        path.lineTo(px + cellSize, py);
      }
      // right
      if (!terr.has(y * cols + (x + 1))) {
        path.moveTo(px + cellSize, py);
        path.lineTo(px + cellSize, py + cellSize);
      }
      // bottom
      if (!terr.has((y + 1) * cols + x)) {
        path.moveTo(px + cellSize, py + cellSize);
        path.lineTo(px, py + cellSize);
      }
      // left
      if (!terr.has(y * cols + (x - 1))) {
        path.moveTo(px, py + cellSize);
        path.lineTo(px, py);
      }
    });

    // stroke once per empire
    ctx.save();
    ctx.strokeStyle = '#000';
    ctx.lineWidth   = Math.max(2, cellSize * 0.1);
    ctx.lineJoin    = 'round';
    ctx.lineCap     = 'round';
    ctx.stroke(path);
    ctx.restore();
  });

  // draw capitals
  EmpireManager.empires.forEach(emp => {
    if (!emp.capital) return;
    const { x, y } = emp.capital;
    const cx = x * cellSize + cellSize / 2;
    const cy = y * cellSize + cellSize / 2;
    const radius = cellSize * 1;

    // capital marker
    ctx.beginPath();
    ctx.arc(cx, cy, radius, 0, Math.PI * 2);
    ctx.fillStyle = '#fff';
    ctx.fill();
    ctx.lineWidth   = Math.max(2, Math.ceil(cellSize * 0.05));
    ctx.strokeStyle = '#000';
    ctx.stroke();

    // name label (outlined)
    if (!window.hideEmpireNames) {
      const fontPx = Math.max(12, cellSize * 3.5);
      const labelX = cx + radius + 6;
      if (window.drawOutlinedLabel) {
        window.drawOutlinedLabel(ctx, emp.name, labelX, cy, fontPx, 'left');
      } else {
        ctx.fillStyle = '#FFF';
        ctx.font = `${Math.ceil(fontPx)}px sans-serif`;
        ctx.textBaseline = 'middle';
        ctx.textAlign = 'left';
        ctx.fillText(emp.name, labelX, cy);
      }
    }
  }); // ← this ); was missing

  // finally, if there's a saved route, re-draw it (only once, not per empire)
  if (window.currentRouteEmpire && window.currentRouteTarget) {
    const { x, y } = window.currentRouteTarget;
    drawRoute(window.currentRouteEmpire, x, y);
  }
}


  window.drawCurrent = drawCurrent;

  // --- Download Helper ---
  function downloadTextFile(name, text) {
    const blob = new Blob([text], { type: 'text/plain' });
    const a    = document.createElement('a');
    a.href     = URL.createObjectURL(blob);
    a.download = name;
    a.click();
  }

  // --- Import/Export Mappings ---
  const TERRAIN_CHAR = {
    PLAIN:  'P',
    DESERT: 'D',
    WATER:  'W',
    MOUNTAIN:'M',
    FOREST: 'F',
    SHRUB: 'S',
    RIVER:  'R',
    ICE:  'I'
  };
  const CHAR_TERRAIN = Object.fromEntries(
    Object.entries(TERRAIN_CHAR).map(([k,v]) => [v,k])
  );


// Does the best-known path from this empire's capital to idx pass through enemy-owned cells?
function pathUsesHostileTransit(emp, idx, hostileSet, grid) {
  const cols = grid.cols;

  // Get parent index function (works with typed or object parent map)
  let getParentIdx;
  if (emp.parentIdx instanceof Int32Array) {
    getParentIdx = (i) => emp.parentIdx[i];
  } else if (emp.parentMap) {
    getParentIdx = (i) => {
      const x = i % cols, y = (i / cols) | 0;
      const p = emp.parentMap[y]?.[x];
      return p ? (p.y * cols + p.x) : -1;
    };
  } else {
    // No path info -> can't verify, treat as "requires transit" to be safe
    return true;
  }

  const start = emp.capital ? emp.capital.y * cols + emp.capital.x : -1;
  if (start < 0) return true;

  // Walk back along the parent chain; if we ever step through a hostile-owned cell, return true.
  // Important: we check the *parent* nodes along the way, not the destination cell itself.
  let i = idx;
  // hard guard against loops
  let hops = 0, N = grid.rows * grid.cols;
  while (i !== start && i >= 0 && hops++ < N) {
    const p = getParentIdx(i);
    if (p < 0) break;            // unreachable or root
    if (hostileSet.has(p)) return true;  // found hostile transit
    i = p;
  }
  return false;
}


// Memoized hostile-transit check: "does the best-known path to idx pass through enemy-owned cells?"
// Works with either typed parentIdx (Int32Array) or legacy parentMap.
function pathUsesHostileTransitMemo(emp, idx, hostileSet, grid) {
  // Per-empire memo map (key: flat idx, value: boolean)
  if (!emp._transitCache) emp._transitCache = new Map();
  const cache = emp._transitCache;
  if (cache.has(idx)) return cache.get(idx);

  const cols = grid.cols;

  // parent accessor (typed or legacy)
  let getParentIdx;
  if (emp.parentIdx instanceof Int32Array) {
    getParentIdx = (i) => emp.parentIdx[i];
  } else if (emp.parentMap) {
    getParentIdx = (i) => {
      const x = i % cols, y = (i / cols) | 0;
      const p = emp.parentMap[y]?.[x];
      return p ? (p.y * cols + p.x) : -1;
    };
  } else {
    // No parent info => don’t block (be permissive)
    cache.set(idx, false);
    return false;
  }

  const start = emp.capital ? emp.capital.y * cols + emp.capital.x : -1;
  if (start < 0) { cache.set(idx, false); return false; }

  // Walk back the parent chain; remember the trail to bulk-fill the cache at the end
  let i = idx;
  const trail = [];
  let hops = 0, N = grid.rows * grid.cols;
  let result; // undefined until decided

  while (i !== start && i >= 0 && hops++ < N) {
    if (cache.has(i)) { result = cache.get(i); break; }
    const p = getParentIdx(i);
    if (p < 0) { result = false; break; }
    if (hostileSet.has(p)) { result = true; break; } // would pass through enemy land
    trail.push(i);
    i = p;
  }
  if (result === undefined) result = false; // reached start or no evidence of hostile transit

  // Cache the whole trail with the same result
  cache.set(idx, result);
  for (const t of trail) cache.set(t, result);

  return result;
}


  // --- Randomize Terrain ---
  rndBtn.addEventListener('click', () => {
    const raw = {}, keys = Object.keys(TERRAIN);
    let total = 0;
    keys.forEach(t => {
      const v = parseInt(document.getElementById(`slider-${t}`).value, 10);
      raw[t] = v; total += v;
    });
    if (total === 0) return alert('Set at least one terrain above 0%.');
    const weights = {};
    keys.forEach(t => weights[t] = raw[t] / total);
    grid.randomize(weights);
    window.applyAllTerrainValues();   // <— add this

    generateVariantGrid();
    computeMountainDepth(grid);

    window.precomputeWaterShading(grid, canvas.width, canvas.height);
    resizeCanvases();
    renderBackground();
      simulateAndDraw();

    


    // drawCurrent();
  });

  // --- Painting ---
  function paintAtEvent(e) {
  const rect = canvas.getBoundingClientRect();
  const sx   = (e.clientX - rect.left) * (canvas.width/rect.width);
  const sy   = (e.clientY - rect.top ) * (canvas.height/rect.height);

  // recompute after any resizes
  const cellW = canvas.width  / grid.cols;
  const cellH = canvas.height / grid.rows;
  const x     = Math.floor(sx / cellW);
  const y     = Math.floor(sy / cellH);

  if (x>=0 && x<grid.cols && y>=0 && y<grid.rows) {
    const paintTarget  = currentPaintTarget(); // "terrain" | "value"
    const half = Math.floor(((paintTarget === 'value' ? (window.valueBrushSize || 1) : brushSize)) / 2);
    const valueToPaint = parseInt(paintValueInput?.value || '0', 10);

    for (let dy = -half; dy <= half; dy++) {
      for (let dx = -half; dx <= half; dx++) {
        const xx = x + dx, yy = y + dy;
        if (xx < 0 || xx >= grid.cols || yy < 0 || yy >= grid.rows) continue;

        if (paintTarget === 'value') {
          grid.setValueAt(xx, yy, valueToPaint);
        } else {
          grid.cells[yy][xx].terrain = paintType;
          window.variantGrid[yy][xx] = Math.floor(
            Math.random() * (TERRAIN_VARIANTS[paintType] || [TERRAIN[paintType].color]).length
          );
          // keep value layer aligned with terrain type
          grid.setValueAt(xx, yy, (window.terrainValueMap?.[paintType] ?? 1));
        }
      }
    }

    if (paintTarget !== 'value') {
    scheduleTerrainShading();
}

    renderBackground();
    drawCurrent();
  }
}





  canvas.addEventListener('mousedown', e => {
    if (window.currentMode==='paint') { painting=true; paintAtEvent(e); }
  });
  canvas.addEventListener('mousemove', e => {
    if (window.currentMode==='paint' && painting) paintAtEvent(e);
  });
  document.addEventListener('mouseup', () => { painting = false; });

// —————————————————————————————
// Canvas-click: handle Place-Capital & (optional) Find-Route
canvas.addEventListener('click', e => {
  const rect   = canvas.getBoundingClientRect();
  const scaleX = canvas.width  / rect.width;
  const scaleY = canvas.height / rect.height;
  const cx     = (e.clientX - rect.left) * scaleX;
  const cy     = (e.clientY - rect.top ) * scaleY;
const cellW = canvas.width  / grid.cols;
const cellH = canvas.height / grid.rows;
const x     = Math.floor(cx / cellW);
const y     = Math.floor(cy / cellH);

  // (1) Route-finding mode?  [if you’ve wired that up]
  if (window.currentMode === 'findRoute' && window.pendingRouteEmpire) {
    window.currentRouteEmpire = window.pendingRouteEmpire;
    window.currentRouteTarget = { x, y };
    window.pendingRouteEmpire = null;
    window.currentMode        = null;
    return;
  }

  // (2) Capital-placement mode
  if (window.currentMode === 'placeCapital' && window.currentEmpire) {
    const emp = window.currentEmpire;
    emp.capital = { x, y };
    emp._capitalDisplay.textContent = `Capital: (${x},${y})`;

    window.currentEmpire = null;
    window.currentMode   = null;
    window.simulateAndDraw();
    window.drawCurrent();
    return;
  }
});

// EXPORT OWNERSHIP
// Ownership export (0 for neutral/water; 1..9 then A..Z for empires)
const exportOwnershipBtn = document.getElementById('export-ownership-btn');
if (exportOwnershipBtn) {
  exportOwnershipBtn.addEventListener('click', () => {
    const rows = grid.rows, cols = grid.cols;

    // 1) Build a 2D char buffer, default '0'
    const buf = Array.from({ length: rows }, () => Array(cols).fill('0'));

    // 2) Map each *current* empire to a symbol by its panel/order position
// Map 1..9 -> '1'..'9', 10..35 -> 'A'..'Z', 36..61 -> 'a'..'z'
function ordinalToSymbol(ord) {
  if (ord <= 9) return String(ord);                 // 1..9
  ord -= 9;
  if (ord <= 26) return String.fromCharCode(64 + ord); // 10..35 => 'A'..'Z'
  ord -= 26;
  if (ord <= 26) return String.fromCharCode(96 + ord); // 36..61 => 'a'..'z'
  return '?'; // beyond 61 empires
}

const symbolByEmpireId = new Map();
EmpireManager.empires.forEach((emp, idx) => {
  const ord = idx + 1; // 1-based order in the panel
  symbolByEmpireId.set(emp.id, ordinalToSymbol(ord));
});

    // 3) Fill owned cells
    EmpireManager.empires.forEach(emp => {
      const sym = symbolByEmpireId.get(emp.id);
      if (!sym) return;
      emp.territory.forEach(idx => {
        const y = Math.floor(idx / cols);
        const x = idx % cols;
        buf[y][x] = sym;
      });
    });

    // 4) Emit text
    const text = buf.map(row => row.join('')).join('\n');
    downloadTextFile('ownership.txt', text);
  });
}

  // --- Export Terrain ---
  exportTerrainBtn.addEventListener('click', () => {
    let out = '';
    for (let y=0; y<grid.rows; y++) {
      for (let x=0; x<grid.cols; x++) {
        out += TERRAIN_CHAR[grid.cells[y][x].terrain];
      }
      out += '\n';
    }
    downloadTextFile('terrain.txt', out);
  });

  

// LOAD TERRAIN

  function loadTerrainFromText(text) {
  // parse lines → token matrix (handles space-separated or single-char files)
  const rawLines = text.trim().split(/\r?\n/).filter(l => l.trim());
  const mapTokens = rawLines.map(l => {
    const parts = l.trim().split(/\s+/);
    return (parts.length > 1) ? parts : l.trim().split('');
  });

  const rows = mapTokens.length;
  const cols = mapTokens[0].length;

  // update grid-slider & UI state
  gridWidthSlider.value     = cols;
gridWidthDisplay.textContent  = cols;
gridHeightSlider.value    = rows;
gridHeightDisplay.textContent = rows;

  grid = new Grid(cols, rows);
  window.grid     = grid;
  grid.initValueLayer(1);   // ← add this right after the Grid is created
  window.applyAllTerrainValues();   // apply the terrain->value sliders


  window.gridWidth  = cols;
  window.gridHeight = rows;

  // Re-snap height to the new aspect (keeping current wrapper width)
if (window.__resizerSnapToCurrent) { window.__resizerSnapToCurrent(); }

  // populate terrains
  mapTokens.forEach((rowTokens, y) => {
    rowTokens.forEach((tok, x) => {
      grid.cells[y][x].terrain = CHAR_TERRAIN[tok] || 'PLAIN';
    });
  });

  // regenerate variants + shading + redraw
  generateVariantGrid();
  computeMountainDepth(grid);
  window.precomputeWaterShading(grid, canvas.width, canvas.height);
  resizeCanvases();
  renderBackground();
  simulateAndDraw();
  drawCurrent();
}

// Import terrain from files
importTerrainInput.addEventListener('change', function() {
  const file = this.files[0];
  if (!file) return;
  const reader = new FileReader();
  reader.onload = () => loadTerrainFromText(reader.result);
  reader.readAsText(file);
  this.value = '';
});

// Terrain menu
function initTerrainMenu() {
  const menu = document.getElementById('terrain-menu');
  menu.addEventListener('change', async () => {
    const fname = menu.value;
    if (!fname) return;

    try {
      // point at your actual folder name:
      const res = await fetch(`./terrain/${fname}`);
      if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
      const text = await res.text();
      loadTerrainFromText(text);
    } catch (err) {
      alert('Failed to load terrain: ' + err.message);
    } finally {
      menu.value = '';
    }
  });
}

function initValueMenu() {
  const menu = document.getElementById('value-menu');
  if (!menu) return;

  menu.addEventListener('change', async () => {
    const fname = menu.value;
    if (!fname) return;

    try {
      // Expect files under ./landvalue/ with the same filenames as terrain (minus Middle-earth)
      const res = await fetch(`./${fname}`);
      if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
      const text = await res.text();

      // Try to import into current grid dimensions
      const ok = grid.importValueLayerFromText(text);
      if (!ok) {
        alert('Value map size does not match the current grid. Load the matching terrain first (same rows × cols), then try again.');
      } else {
        // switch to Value view so the user immediately sees it
        window.renderMode = 'value';
        renderBackground();
        drawCurrent();
      }
    } catch (err) {
      alert('Failed to load value map: ' + err.message);
    } finally {
      menu.value = '';
    }
  });
}

// Overlay menu (needs to live after the DOM is ready)
function initOverlayMenu() {
  const overlayMenu = document.getElementById('overlay-menu');
  overlayMenu.addEventListener('change', async (e) => {
    const fname = e.target.value;
    if (!fname) return;

    // load the selected image
    const img = new Image();
    img.onload = () => {
      overlayImage = img;
      drawCurrent();
    };
    img.onerror = () => {
      alert('Failed to load overlay: ' + fname);
    };
    img.src = fname;  // your <option> values already include "guide/… .png"

    // reset the dropdown
    overlayMenu.value = '';
  });
}

// call it once on startup (after your initTerrainMenu call)
initOverlayMenu();

//// EXPORTING AND IMPORTING EMPIRES
  // helper to kick off a download
function downloadJSON(name, text) {
  const blob = new Blob([text], { type: 'application/json' });
  const a    = document.createElement('a');
  a.href     = URL.createObjectURL(blob);
  a.download = name;
  a.click();
}

// EXPORT: dump empire state to JSON
document
  .getElementById('export-empires-btn')
  .addEventListener('click', () => {
    const data = EmpireManager.empires.map(emp => ({
      name:         emp.name,
      color:        emp.color,
      size:         emp.size,
      travelSpeeds: emp.travelSpeeds,
      capital:      emp.capital
    }));
    downloadJSON('empires.json', JSON.stringify(data, null, 2));
  });

// IMPORT: read JSON, rebuild empires & their UI
document
  .getElementById('import-empires-input')
  .addEventListener('change', function() {
    const file = this.files[0];
    if (!file) return;
    const reader = new FileReader();

    reader.onload = () => {
      try {
        const configs = JSON.parse(reader.result);

        // 1) Clear existing
        EmpireManager.empires = [];
        EmpireManager.nextId  = 1;
        document.getElementById('empire-panels').innerHTML = '';


        // 2) For each saved empire, recreate data + UI
        configs.forEach(cfg => {
          // a) Data
          const emp = EmpireManager.addEmpire(cfg.name, cfg.color);
          emp.size         = cfg.size;
          emp.travelSpeeds = cfg.travelSpeeds;
          emp.capital      = cfg.capital;

          // b) Build its panel & wire up all controls
          EmpireManager.createEmpirePanel(emp);

          // c) Sync sliders & labels to imported values
          emp._sizeSlider.value = cfg.size;
          if (emp._sizeInput) {
            emp._sizeInput.value = cfg.size;
          }



 // c) Travel speeds: merge defaults with the imported ones
const defaults = window.globalTravelSpeeds || { ...DEFAULT_TRAVEL_SPEEDS };
emp.travelSpeeds = Object.assign({}, defaults, cfg.travelSpeeds || {});


// d) Sync ALL terrain rows in the UI (even ones missing in the file)
for (const t of TERRAIN_KEYS) {
  const s = emp._speedSliders?.[t];
  const v = emp._speedValues?.[t];
  const val = emp.travelSpeeds[t];
  if (s) s.value = val;
  if (v) {
    const formatted = Number(val).toFixed(1);
    if (v.tagName === 'INPUT') v.value = formatted;
    else v.textContent = formatted;
  }
}
          // d) Only show capital if it was set
          if (cfg.capital) {
            emp._capitalDisplay.textContent =
              `Capital: (${cfg.capital.x},${cfg.capital.y})`;
          }
        });

        // ) Rerun the sim & redraw
        simulateAndDraw();
        drawCurrent();
      } catch (err) {
        alert('Error loading empires.json: ' + err.message);
      }
    };
    reader.readAsText(file);
    this.value = '';
  });







// EXPORTING AND IMPORTING VALUE LAYER
const importValueBtn = document.getElementById('import-value-btn');
const importValueFile = document.getElementById('import-value-file');
const exportValueBtn = document.getElementById('export-value-btn');

importValueBtn?.addEventListener('click', () => importValueFile.click());
importValueFile?.addEventListener('change', () => {
  const file = importValueFile.files?.[0];
  if (!file) return;
  const reader = new FileReader();
  reader.onload = () => {
    const ok = grid.importValueLayerFromText(String(reader.result));
    if (ok) {
      renderBackground();
      drawCurrent();
    }
  };
  reader.readAsText(file);
  importValueFile.value = '';
});

exportValueBtn?.addEventListener('click', () => {
  const txt = grid.exportValueLayerToText();
  const name = `landvalue_${grid.cols}x${grid.rows}.txt`;
  downloadTextFile(name, txt);
});




if (toggleGridBtn) {
  toggleGridBtn.addEventListener('click', () => {
    showGrid = !showGrid;
    toggleGridBtn.textContent = showGrid ? 'Hide Grid Lines' : 'Show Grid Lines';
    drawCurrent();
  });
}


// Function for computing total land value
 function computeEmpireTotals() {
  const cols = grid.cols;
  for (const e of EmpireManager.empires) {
    let val = 0;
    if (e.territory && e.territory.size) {
      for (const idx of e.territory) {
        const x = idx % cols, y = (idx / cols) | 0;
        val += grid.getValueAt(x, y);       // 0..61 per cell
      }
    }
    e._area  = e.territory ? e.territory.size : 0;
    e._value = val;

    // Optional: show in panel if you’ve created a slot for it
    if (e._valueDisplay) e._valueDisplay.textContent = `Land value: ${e._value}`;

  }
}


// One pass: set each empire.size = round(value / threshold). Returns true if any size changed.
function adjustSizesOnceFromValue(threshold) {
  threshold = Math.max(1, Math.min(61, Math.floor(threshold || 1)));
  let changed = false;

  for (const e of EmpireManager.empires) {
    const val = Math.max(0, Math.floor(e._value || 0));
    let newSize = Math.round(val / threshold);
    // keep sane bounds (match your size UI)
    newSize = Math.max(1, Math.min(30000, newSize));

    if (newSize !== e.size) {
      e.size = newSize;
      changed = true;
      // keep the panel UI in sync
      if (e._sizeSlider) e._sizeSlider.value = String(newSize);
      if (e._sizeInput)  e._sizeInput.value  = String(newSize);
    }
  }
  return changed;
}


// --- 1) Single-ring simulation step (same logic, less churn) ---
async function simulateOneRing() {
  // 1a) Recompute cost maps (hostile-cell barrier + penalty)
  await EmpireManager.updateAllCostMaps(grid);

  const rows = grid.rows, cols = grid.cols, N = rows * cols;

  // Build a one-time list of all non-water cell indices (avoid repeating the water check per-empire)
  const nonWaterIdx = [];
  for (let y = 0; y < rows; y++) {
    const row = grid.cells[y];
    for (let x = 0; x < cols; x++) {
      if (row[x].terrain !== 'WATER') nonWaterIdx.push(y * cols + x);
    }
  }

  // 1b) Collect all reachable, non-water cells
  // (Keep the same global “cheapest-first” behavior.)
  const all = [];
  for (const emp of EmpireManager.empires) {
    // Accept either typed (Float64/32) flat buffer or legacy 2D array
    const cm = (emp.costMapFlat != null) ? emp.costMapFlat : emp.costMap;
    if (!cm) continue;                 // no map yet for this empire
    const isFlat = ArrayBuffer.isView(cm);

    for (let k = 0; k < nonWaterIdx.length; k++) {
      const idx = nonWaterIdx[k];
      const cost = isFlat ? cm[idx] : cm[(idx / cols | 0)][idx % cols];
      if (!Number.isFinite(cost)) continue;
      all.push({ emp, idx, cost });
    }
  }

  // 1c) Global sort, clear, assign exactly `size` cells per empire, one owner per cell
  all.sort((a, b) => a.cost - b.cost);

  // Important: we only clear after 1b, so any earlier checks would have seen last ring's territory
  // (We don’t do owner-crossing checks here, so this is just the right place to clear.)
  for (const emp of EmpireManager.empires) emp.territory.clear();

  const taken  = new Uint8Array(N);    // faster & smaller than Set()
  const counts = Object.create(null);
  for (const emp of EmpireManager.empires) counts[emp.id] = 0;

  for (let i = 0; i < all.length; i++) {
    const { emp, idx } = all[i];
    if (counts[emp.id] >= emp.size) continue;
    if (taken[idx]) continue;
    emp.territory.add(idx);
    taken[idx] = 1;
    counts[emp.id]++;
  }

  // Update the per-empire "Size: N" label in each panel (only if changed)
  for (const emp of EmpireManager.empires) {
    if (emp._sizeDisplay) {
      const txt = `Size: ${emp.territory.size}`;
      if (emp._sizeDisplay.textContent !== txt) emp._sizeDisplay.textContent = txt;
    }
  }

  // Compute empire value (unchanged)
  computeEmpireTotals();
}

// --- 2) Full “recalibration” loop, now checking actual territory changes ---
// 2) Full “recalibration” loop with live updates
  async function recalibrateTerritory() {
  if (window.disablePenalty || window.recalibrateCancel) {
    // one‐shot, no penalty
    await simulateOneRing();
    drawCurrent();
    return;
  }
  let changed;
  do {
    if (window.recalibrateCancel) break;

    // snapshot each empire’s territory
    const prev = EmpireManager.empires.map(e => new Set(e.territory));

// do one ring of capture (now waits for workers)
     await simulateOneRing();

    // redraw so the user sees that ring land
    //drawCurrent();

    // small pause so the frame can render (20ms = ~50fps)
    await new Promise(r => setTimeout(r, 0));

    // detect any empire whose territory actually changed
    changed = EmpireManager.empires.some((e,i) => {
      const before = prev[i];
      if (e.territory.size !== before.size) return true;
      for (const cell of e.territory) if (!before.has(cell)) return true;
      for (const cell of before)    if (!e.territory.has(cell)) return true;
      return false;
    });
  } while (changed);
}

// --- 3) Swap your old simulateAndDraw and add the Recalibrate button ---
// NEW:
async function simulateAndDraw() {
  await simulateOneRing();
  //drawCurrent();
}



  window.simulateAndDraw = simulateAndDraw;

  // --- Initial Render & UI Boot ---
  generateVariantGrid();
  window.precomputeWaterShading(grid, canvas.width, canvas.height);
  resizeCanvases();
  renderBackground();
  

  simulateAndDraw();

initTerrainMenu();
initValueMenu();



  // ↓↓↓ render loop starts here ↓↓↓
// ↓↓↓ render loop starts here ↓↓↓
;(function renderLoop() {
  // 1) always redraw the base map (terrain + grid + territory + capitals)
  drawCurrent();

  // 2) now overlay the semi‑transparent heatmap if toggled
  if (window.currentHeatEmpire) {
    ctx.save();
    ctx.globalAlpha = 0.5;
    drawHeatmap(window.currentHeatEmpire);
    ctx.restore();
  }

  // 3) finally overlay any route
  if (window.currentRouteEmpire && window.currentRouteTarget) {
    drawRoute(
      window.currentRouteEmpire,
      window.currentRouteTarget.x,
      window.currentRouteTarget.y
    );
  }

  requestAnimationFrame(renderLoop);
})();
});

