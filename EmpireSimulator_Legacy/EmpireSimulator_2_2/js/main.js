// js/main.js

const USE_REACH = false;  // When false, bidding ignores reach maps; eligibility = (finite cost && cap>0)


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


const pathPool = new PathWorkerPool('js/pathfindingWorker.js?v=2', _POOL_SIZE);


// Map the engine's terrain strings to compact byte codes.
// Keep this in sync with the worker's mapping.
const TERRAIN_CODE = {
  PLAIN: 0, DESERT: 1, WATER: 2, MOUNTAIN: 3, FOREST: 4, SHRUB: 5, RIVER: 6, ICE: 7
};



// ---- Cached terrain bytes (0..7 per cell) ----
window._terrainCodeFlatCache = null;

function rebuildTerrainByteCache(grid) {
  const { rows, cols } = grid;
  const A = new Uint8Array(rows * cols);
  let k = 0;
  for (let y = 0; y < rows; y++) {
    const row = grid.cells[y];
    for (let x = 0; x < cols; x++) {
      A[k++] = (TERRAIN_CODE[row[x].terrain] || 0);
    }
  }
  window._terrainCodeFlatCache = A;
}

function getTerrainBytes(grid) {
  const N = grid.rows * grid.cols;
  if (!(window._terrainCodeFlatCache instanceof Uint8Array) ||
      window._terrainCodeFlatCache.length !== N) {
    rebuildTerrainByteCache(grid);
  }
  return window._terrainCodeFlatCache;
}





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
function computeCostMapOffload(emp, grid) {
  const id = ++_pfMsgId;

  return new Promise(resolve => {
    _pfPending.set(id, resolve);

const terrainCodeFlat = getTerrainBytes(grid);

const payload = {
  id,
  empireId: emp.id,
  rows: grid.rows,
  cols: grid.cols,
  terrainCodeFlat,     // structured clone will copy, but we avoid re-encoding
  travelSpeeds: emp.travelSpeeds,
  capital: emp.capital,
};

// Do NOT transfer the cache buffer (it would detach it). Just post payload.
pathPool.postMessage(payload);




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


(function initCitiesMenu(){
  const menu = document.getElementById('cities-menu');
  if (!menu) return;

  menu.addEventListener('change', async () => {
    const fname = menu.value;
    if (!fname) return;

    try {
      const res = await fetch(`./${fname}`);
      if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
      const text = await res.text();

      importEmpiresFromText(text, fname);
    } catch (err) {
      alert('Failed to load preset: ' + fname + '\n' + err.message);
    } finally {
      // allow picking the same option again later
      menu.value = '';
    }
  });
})();


// --- Auto-Grow amount wiring (cells per tick) ---
window.autoGrowAmount = 50;

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
  if (gridWidthDisplay)  gridWidthDisplay.textContent  = String(gridWidth);
  rebuildGrid();
});

gridHeightSlider.addEventListener('input', () => {
  gridHeight = parseInt(gridHeightSlider.value, 10);
  if (gridHeightDisplay) gridHeightDisplay.textContent = String(gridHeight);
  rebuildGrid();
});


// Average land value helper
function computeGlobalAvgLandValue() {
  const rows = grid.rows, cols = grid.cols;
  if (!grid.valueLayer) return 0; // or return 1 if you prefer a safe default

  let sum = 0, count = 0;
  for (let y = 0; y < rows; y++) {
    const row = grid.cells[y];
    for (let x = 0; x < cols; x++) {
      if (row[x].terrain === 'WATER') continue;
      const v = grid.getValueAt(x, y);  // 0..61 from valueLayer
      sum += v;
      count++;
    }
  }
  return count ? (sum / count) : 0;
}

// Mouse hover info

// grab the checkbox (now in the HTML)
const infoCheckbox = document.getElementById('info-mode-checkbox');
window.infoMode = false;

// toggle the global flag when clicked
infoCheckbox.addEventListener('change', e => {
  window.infoMode = e.target.checked;
  tooltip.style.opacity = '0';
    _lastInfoIdx = -1;                   // ← reset the cache when toggling
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
    optimizationdetail:        { title: 'Empire simulation', text: 'Growth threshold: Above which land value empires grow. Discrimination: Threshold for which cells that should be ignored. Power weight: How much weight given to power relative to travel cost.' },
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

// --- Info-mode helpers: cheapest empires to a cell (uses precomputed cost maps) ---
function topThreeCheapestAt(idx) {
  const out = [];
  for (const e of EmpireManager.empires) {
    const A = e.costMapFlat;
    if (!A || A.length === 0) continue;
    const d = A[idx];
    if (!(d < Infinity)) continue;
    out.push({ id: e.id, name: e.name || `Empire ${e.id}`, cost: d });
  }
  out.sort((a, b) => (a.cost - b.cost) || (a.id - b.id)); // stable tie-break by id
  return out.slice(0, 3);
}

// cache so we only compute when the hovered cell actually changes
let _lastInfoIdx  = -1;
let _lastTop3HTML = '';


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


// Land value (0..61); show char if helper exists
const landVal = (typeof grid.getValueAt === 'function')
  ? grid.getValueAt(col, row)
  : Number(grid.cells[row][col].value ?? grid.cells[row][col].landValue ?? 0);
const valChar = (typeof valToCharLocal === 'function') ? ` (${valToCharLocal(landVal)})` : '';

// Top 3 cheapest empires to this cell (only recompute when cell changes)
let topHTML = '';
//const idx = row * grid.cols + col;
if (idx !== _lastInfoIdx) {
  const top3 = topThreeCheapestAt(idx);
  topHTML = top3.map((t, i) => `${i + 1}) ${t.name} (${t.cost.toFixed(2)})`).join('<br/>');
  _lastInfoIdx  = idx;
  _lastTop3HTML = topHTML;
} else {
  topHTML = _lastTop3HTML;
}

tooltip.innerHTML = `
  <strong>Owner:</strong> ${owner}<br/>
  <strong>Terrain:</strong> ${terr}<br/>
  <strong>Land value:</strong> ${landVal}${valChar}${
    (pct == null ? '' : `<br/><strong>Heatmap:</strong> ${pct.toFixed(0)}%`)
  }${
    (topHTML ? `<br/><strong>Cheapest:</strong><br/>${topHTML}` : '')
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





// ─────────── Optimization (Placement & Sliders) ───────────

// Globals you can tweak later or via the UI below
window.optimizeDelta = 0.20;   // slider change per round (cost units)
window.optimizeDelayMs = 0;    // pause between rounds

let optimizingPlacement = false;
let optimizingSliders   = false;

// --- Adjust Size ←→ Land Value state ---
window.growthThreshold = 1;        // default; synced to the slider


const dirs8 = [
  {dx:  1, dy:  0}, {dx: -1, dy:  0}, {dx: 0, dy:  1}, {dx: 0, dy: -1},
  {dx:  1, dy:  1}, {dx:  1, dy: -1}, {dx: -1, dy: 1}, {dx: -1, dy: -1},
];

function sleep(ms){ return new Promise(r => setTimeout(r, ms)); }
function clamp(v, lo, hi){ return Math.max(lo, Math.min(hi, v)); }
function deepClone(obj){ return JSON.parse(JSON.stringify(obj)); }

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


// Discrimination δ: empires only bid on cells with value >= δ (pure distance ranking)
(function wireDiscrimination(){
  const s   = document.getElementById('discrimination-threshold');
  const out = document.getElementById('discrimination-threshold-val');

  // default: if missing, use growthThreshold (keeps things sensible)
  window.discriminationThreshold = Math.round(
    Number(s?.value ?? window.growthThreshold ?? 1)
  );

  if (out) out.textContent = String(window.discriminationThreshold);

  s?.addEventListener('input', () => {
    const v = Math.max(0, Math.min(61, parseInt(s.value || '1', 10)));
    window.discriminationThreshold = v;
    if (out) out.textContent = String(v);
    // optional: live recompute
    //simulateAndDraw?.();
  });
})();

// Power weight μ: how strongly power helps in contested cells (0 disables power)
(function wirePowerWeight(){
  const s   = document.getElementById('power-weight');
  const out = document.getElementById('power-weight-val');

  window.powerWeight = Number(s?.value ?? 0.15); // default 0.15
  if (out) out.textContent = String(window.powerWeight);

  s?.addEventListener('input', () => {
    const v = Math.max(0, Math.min(5, Number(s.value || 0)));
    window.powerWeight = v;
    if (out) out.textContent = String(v);
    //simulateAndDraw?.();
  });
})();






// Helper: update one empire's capital label if available
function updateCapitalLabel(emp) {
  if (emp._capitalDisplay && emp.capital) {
    emp._capitalDisplay.textContent = `Capital: (${emp.capital.x},${emp.capital.y})`;
  }
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
  ICE:      10,
  SWITCH:   0   // NEW: default global switching cost (0 = off)
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


// Global default speeds used for "apply to all" + new empires."
// Initialize from the first empire if present; otherwise from terrain-specific defaults.
window.globalTravelSpeeds = (function seedGlobals(){
  const first = (window.EmpireManager && EmpireManager.empires && EmpireManager.empires[0]) || null;
  if (first && first.travelSpeeds) return { ...first.travelSpeeds };
  return { ...DEFAULT_TRAVEL_SPEEDS };
})();

// Ensure SWITCH exists in the global bag (in case of older saves)
if (window.globalTravelSpeeds.SWITCH == null) window.globalTravelSpeeds.SWITCH = 0;

function setGlobalSpeed(key, v) {
  let raw = parseFloat(v || 0);

  // SWITCH: allow 0 (to disable) and allow values > 10 if typed
  if (key === 'SWITCH') {
    raw = Math.max(0, Math.round(raw * 10) / 10);
  } else {
    // Other terrains keep the 0.1..10 slider range
    raw = Math.max(0.1, Math.min(10, Math.round(raw * 10) / 10));
  }

  window.globalTravelSpeeds[key] = raw;

  // Push to all existing empires + keep their UI in sync
  if (window.EmpireManager && EmpireManager.empires) {
    for (const e of EmpireManager.empires) {
      e.travelSpeeds[key] = raw;
      applySliderToUI(e, key);
    }
  }


  // Cheap redraw of basic map
  window.drawCurrent?.();

  // Only heavy recompute if a heatmap is active
  window.requestRecomputeFromSliders?.();

  

}

function buildGlobalSpeedSliders() {
  const wrap = document.getElementById('global-speed-sliders');
  if (!wrap) return;
  wrap.innerHTML = '';

  // Make sure SWITCH exists
  if (window.globalTravelSpeeds.SWITCH == null) window.globalTravelSpeeds.SWITCH = 0;

  // 1) One row per terrain (unchanged)
  for (const t of TERRAIN_KEYS) {
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

  // 2) EXTRA ROW: global SWITCH (0..10 on slider; number box can exceed 10)
  {
    const row = document.createElement('label');
    row.style.display = 'grid';
    row.style.gridTemplateColumns = '90px 1fr 56px';
    row.style.alignItems = 'center';
    row.style.columnGap  = '8px';
    row.style.margin     = '4px 0';

    const name = document.createElement('span');
    name.textContent = 'Switching';

    const slider = document.createElement('input');
    slider.type  = 'range';
    slider.min   = '0';     // allow 0 (off)
    slider.max   = '10';
    slider.step  = '0.1';
    slider.value = String(window.globalTravelSpeeds.SWITCH);
    slider.style.width  = '100%';
    slider.style.margin = '0';

    const num = document.createElement('input');
    num.type   = 'number';
    num.min    = '0';
    num.step   = '0.1';
    num.value  = Number(window.globalTravelSpeeds.SWITCH).toFixed(1);
    num.style.width = '56px';
    num.style.textAlign = 'right';
    num.removeAttribute('max'); // ← allow higher than 10 if typed

    function applyFromSlider(v) {
      setGlobalSpeed('SWITCH', v);
      slider.value = String(window.globalTravelSpeeds.SWITCH);
      num.value    = Number(window.globalTravelSpeeds.SWITCH).toFixed(1);
    }

    function applyFromNumber(v) {
      setGlobalSpeed('SWITCH', v);
      // Slider parks at 10 if the typed value is > 10
      slider.value = String(Math.min(10, window.globalTravelSpeeds.SWITCH));
      num.value    = Number(window.globalTravelSpeeds.SWITCH).toFixed(1);
    }

    slider.addEventListener('input', () => applyFromSlider(slider.value));
    num.addEventListener('change',   () => applyFromNumber(num.value));

    row.append(name, slider, num);
    wrap.appendChild(row);
  }
}

// Build once when the DOM is ready (call after your constants exist)
buildGlobalSpeedSliders();


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
  n = Math.max(1, Math.min(100, n|0));
  const cells = pickRandomLandCells(n);
  if (!cells.length) return;

  for (let i = 0; i < cells.length; i++) {
    const emp = EmpireManager.addEmpire();
    createEmpirePanel(emp);

    // default travel costs from global settings + sync the panel UI
    if (window.globalTravelSpeeds) emp.travelSpeeds = deepClone(window.globalTravelSpeeds);
    for (const k of TERRAIN_KEYS) applySliderToUI(emp, k);
    applySliderToUI(emp, 'SWITCH');


    // place capital at the picked land cell
    emp.capital = { x: cells[i].x, y: cells[i].y };
    updateCapitalLabel(emp);
  }

  // compute territories for the new set
  //await simulateAndDraw();
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


// Wire "Adjust size to land value" to the new recalibrate+sizes loop.
// Also wire the "Adjust size and optimize (Value)" button to the same behavior.
(function wireAdjustButtons(){
  let comboBtn = document.getElementById('combo-adjust-optimize-btn');

  // If neither exists, nothing to do
  if (!comboBtn) return;

  function stopOthers() {
    optimizingPlacement = false;
    optimizingSliders = false;
    optimizingPlacementValue = false;
    optimizingSlidersValue = false;
    // also stop the older loops if they were mid-flight
    window.recalibrateCancel = false;
  }

  // helper to toggle a button into/out of running state and run the loop
  async function toggleRun(btn) {
    if (btn.dataset.running === '1') { // request stop
      btn.dataset.running = '0';
      return;
    }
    stopOthers();
    await runRecalibrateWithDynamicSizes(btn);
  }

  // Bind whichever buttons exist
  comboBtn ?.addEventListener('click', () => toggleRun(comboBtn));
})();


// Import Empires (button opens the hidden file input)
const importEmpiresBtn   = document.getElementById('import-empires-btn');
const importEmpiresInput = document.getElementById('import-empires-input');
if (importEmpiresBtn && importEmpiresInput) {
  importEmpiresBtn.addEventListener('click', () => importEmpiresInput.click());
}



let optimizingPlacementValue = false;
let optimizingSlidersValue   = false;


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
  grid.initValueLayer(11);   // ← add this right after the Grid is created
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
grid.initValueLayer(11);



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
  //computeEmpireTotals();  // keep land value & average live in the panel at all times
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



// Is cell `idx` eligible by a WATER route from *any currently-owned* cell?
// We follow the parent chain from idx towards the capital and accept iff
// we hit one of our owned cells and *all steps after that are WATER*.
// (We allow the last leg over water; no crossing enemy/neutral land.)
function reachableByWaterFromOwned(emp, idx, ownerPrev, grid) {
  const cols = grid.cols;

  // Access parent index for this empire (typed or legacy)
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
    return false; // no path info; be conservative
  }

  let i = idx;
  let hops = 0, N = grid.rows * grid.cols;

  // skip the destination cell itself; we examine the *intermediate* steps
  i = getParentIdx(i);
  let sawWater = false;

  while (i >= 0 && hops++ < N) {
    // If we reached one of our owned cells: true iff we actually sailed at least one water step
    if (ownerPrev[i] === emp.id) return sawWater;

    const x = i % cols, y = (i / cols) | 0;
    const terr = grid.cells[y][x].terrain;

    if (terr === 'WATER') {
      sawWater = true;
      i = getParentIdx(i);
      continue;
    }

    // we hit land that we don't own before touching our coast ⇒ not eligible by water
    return false;
  }
  return false;
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

// NEW:
rebuildTerrainByteCache(grid);

    generateVariantGrid();
    computeMountainDepth(grid);

    window.precomputeWaterShading(grid, canvas.width, canvas.height);
    resizeCanvases();
    renderBackground();
      //simulateAndDraw();

    


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

// NEW: keep bytes cache in sync (if it exists)
if (window._terrainCodeFlatCache) {
  const idx = yy * grid.cols + xx;
  window._terrainCodeFlatCache[idx] = (TERRAIN_CODE[paintType] || 0);
}
          
        }
      }
    }

    if (paintTarget !== 'value') {
    scheduleTerrainShading();
}

    renderBackground();
    drawCurrent();
    window.requestRecomputeFromSliders?.();   // ← recompute when painting while route/heatmap is on
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

  // Ensure cost maps exist before drawing the route
  setTimeout(async () => {
    if (typeof window.recomputeCostMapsOnly === 'function') {
      await window.recomputeCostMapsOnly();
    }
    window.drawCurrent?.();
  }, 0);

  return;
}

  // (2) Capital-placement mode
  if (window.currentMode === 'placeCapital' && window.currentEmpire) {
    const emp = window.currentEmpire;
    emp.capital = { x, y };
    emp._capitalDisplay.textContent = `Capital: (${x},${y})`;

    window.currentEmpire = null;
    window.currentMode   = null;
    //window.simulateAndDraw();
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
  grid.initValueLayer(11);   // ← add this right after the Grid is created
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

  // NEW: rebuild terrain bytes cache once
rebuildTerrainByteCache(grid);

  // regenerate variants + shading + redraw
  generateVariantGrid();
  computeMountainDepth(grid);
  window.precomputeWaterShading(grid, canvas.width, canvas.height);
  resizeCanvases();
  renderBackground();
  //simulateAndDraw();
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



// Automatically load value layer
      const res_value = await fetch(`./landvalue/landvalue_${fname}`);
      if (!res_value.ok) throw new Error(res_value.status + ' ' + res_value.statusText);
      const text_value = await res_value.text();

      // Try to import into current grid dimensions
      const ok = grid.importValueLayerFromText(text_value);


      const avgValue = computeGlobalAvgLandValue();
window.growthThreshold = avgValue;     // default = map’s average value

// After you compute avgValue for the loaded map:
const slider = document.getElementById('growth-threshold');      // <input id="growth-threshold">
const label  = document.getElementById('growth-threshold-val');  // <span id="growth-threshold-val">
if (slider) {
  const min = Number(slider.min) || 1;
  const max = Number(slider.max) || 61;
  //const v = Math.max(min, Math.min(max, Math.round(avgValue)));
  const v = 1;


  slider.value = String(v);
  if (label) label.textContent = String(v);

  // If you rely on this elsewhere (power calc), keep this; otherwise remove:
  window.growthThreshold = v;

  // Optional: trigger a recompute if you want it to apply immediately
  // simulateAndDraw();
}




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


function importEmpiresFromText(jsonText, sourceLabel = 'JSON') {
  const configs = JSON.parse(jsonText);
  if (!Array.isArray(configs)) {
    throw new Error('Expected a JSON array of empire configs.');
  }

  // Clear existing (keep the same array object if possible)
  if (Array.isArray(EmpireManager.empires)) EmpireManager.empires.length = 0;
  else EmpireManager.empires = [];

  EmpireManager.nextId = 1;
  EmpireManager.nextColorIdx = 0;
  document.getElementById('empire-panels').innerHTML = '';

  const defaults = window.globalTravelSpeeds || { ...DEFAULT_TRAVEL_SPEEDS };

  configs.forEach(cfg => {
    const emp = EmpireManager.addEmpire(cfg.name, cfg.color);

    // Build UI first so sliders exist
    EmpireManager.createEmpirePanel(emp);

    // Restore properties (with sane fallbacks)
    emp.size = (cfg.size != null) ? cfg.size : emp.size;
    emp.travelSpeeds = Object.assign({}, defaults, cfg.travelSpeeds || {});
    emp.capital = cfg.capital || null;

    // Sync UI values (terrain + switch)
    for (const k of TERRAIN_KEYS) applySliderToUI(emp, k);
    applySliderToUI(emp, 'SWITCH');

    // Capital label
    updateCapitalLabel(emp);
  });

  drawCurrent();
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
    importEmpiresFromText(String(reader.result), file.name || 'file');
  } catch (err) {
    alert('Error loading ' + (file.name || 'empires.json') + ': ' + err.message);
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



// Recompute per-empire totals from current territory and set display power.
// _value: sum of cell values; _area: number of cells; _avg: average value
// power = sqrt( max(0, _avg - growthThreshold) * _area )
function computeEmpireTotals() {
  const rows = grid.rows, cols = grid.cols;
  const thr  = Number(window.growthThreshold ?? 0) || 0;

  for (const e of EmpireManager.empires) {
    let sum = 0, area = 0;

    // territory is a Set of flat indices (create if missing)
    if (!e.territory) e.territory = new Set();

    for (const idx of e.territory) {
      const y = (idx / cols) | 0;
      const x = idx % cols;
      const v = (typeof grid.getValueAt === 'function')
        ? grid.getValueAt(x, y)
        : Number(grid.cells[y][x].value ?? grid.cells[y][x].landValue ?? 0);
      sum  += v;
      area += 1;
    }

    e._value = sum;
    e._area  = area;
    e._avg   = area > 0 ? (sum / area) : 0;

    const surplus = Math.max(0, e._avg - thr);
    let P = Math.sqrt(surplus * area) || 0;
    if (window.powerScale != null) {
      const s = Number(window.powerScale) || 1;
      P *= s;
    }
    e.power = P;   // for UI/debug; the auction uses P_byId it computes itself

    // --- NEW: keep the panel meta-row in sync ---
    if (e._sizeDisplay) {
      // use area = number of cells currently in territory
      e._sizeDisplay.textContent = `Size: ${area}`;
    }
    if (e._valueDisplay) {
      // round for readability; you can use sum directly if you prefer
      e._valueDisplay.textContent = `Land value: ${Math.round(sum)}`;
    }

  }
}



// Build and draw the Leaderboard (size = cells; power already computed in computeEmpireTotals)
window.renderLeaderboard = function renderLeaderboard() {
  const table = document.getElementById('leaderboard-table');
  if (!table) return;

  // Make sure totals (including power) are up to date
  computeEmpireTotals();

  // Collect data
  const rows = EmpireManager.empires.map(e => ({
    name: e.name || `Empire ${e.id}`,
    cells: e.territory ? e.territory.size : 0,
    power: Number.isFinite(e.power) ? e.power : 0
  }));

  // Sort by size (cells), then power as tie-breaker
  rows.sort((a, b) => (b.cells - a.cells) || (b.power - a.power));

  // Fill table body
  const tbody = table.querySelector('tbody');
  tbody.innerHTML = '';
  rows.forEach((r, i) => {
    const tr = document.createElement('tr');
    tr.innerHTML = `
      <td style="padding:3px 6px;">${i + 1}</td>
      <td style="padding:3px 6px;">${r.name}</td>
      <td style="text-align:right; padding:3px 6px;">${r.cells}</td>
      <td style="text-align:right; padding:3px 6px;">${r.power.toFixed(3)}</td>
    `;
    tbody.appendChild(tr);
  });

// Wire the CSV export button once (build fresh data on each click)
const btn = document.getElementById('export-leaderboard-btn');
if (btn && !btn.dataset.wired) {
  btn.dataset.wired = '1';
  btn.addEventListener('click', () => {
    // 1) Make sure totals are up to date
    computeEmpireTotals();

    // 2) Collect fresh data, and include each empire’s travel costs
    const rowsNow = EmpireManager.empires.map(e => ({
      name:  e.name || `Empire ${e.id}`,
      cells: e.territory ? e.territory.size : 0,
      power: Number.isFinite(e.power) ? e.power : 0,
      speeds: e.travelSpeeds || {}   // ← per-terrain travel cost settings
    }))
    .sort((a, b) => (b.cells - a.cells) || (b.power - a.power));

    // 3) Decide the order of the terrain columns (keep it simple + consistent)
const TERRAIN_ORDER = ['PLAIN','DESERT','WATER','MOUNTAIN','FOREST','SHRUB','RIVER','ICE'];

// 4) CSV header: rank, empire, cells, power, switch, then one column per terrain
const header =
  ['rank','empire','cells','power','switch', ...TERRAIN_ORDER.map(t => t.toLowerCase())]
  .join(',') + '\n';

// 5) Build CSV rows (numbers are formatted; names are JSON-escaped)
const lines = rowsNow.map((r, i) => {
  const terrainVals = TERRAIN_ORDER.map(k => {
    const v = r.speeds[k];
    return (v == null) ? '' : Number(v).toFixed(3);
  });
  const switchVal = (r.speeds && r.speeds.SWITCH != null)
    ? Number(r.speeds.SWITCH).toFixed(3)
    : '0.000';

  return [
    i + 1,
    JSON.stringify(r.name),
    r.cells,
    r.power.toFixed(6),
    switchVal,
    ...terrainVals
  ].join(',');
});

    // 6) Download the file
    const csv = header + lines.join('\n');
    downloadTextFile('leaderboard.csv', csv);
  });
}
};



// Compute target size per empire from total land value and threshold.
// Writes e._targetSize but DOES NOT change e.size.
function computeTargetSizesFromValue(threshold) {
  threshold = Math.max(1, Math.min(61, Math.floor(threshold || 1)));
  let changed = false;

  for (const e of EmpireManager.empires) {
    const val = Math.max(0, Math.floor(e._value || 0)); // total land value
    let t = Math.round(val / threshold);
    t = Math.max(0, Math.min(100000, t));                // same sane bounds as your UI

    if ((e._targetSize | 0) !== t) {
      e._targetSize = t;
      changed = true;
    }
  }
  return changed;
}

// Nudge actual caps (e.size) TOWARD e._targetSize by at most `step`.
// By default we only GROW toward the target (no shrinking). Set `bidirectional=true` if you want both.
function nudgeSizesTowardTarget(step, bidirectional = false) {
  step = Math.max(1, Math.floor(step || 1));
  let changed = false;

  for (const e of EmpireManager.empires) {
    const target = (e._targetSize != null) ? (e._targetSize | 0) : (e.size | 0);
    let s = e.size | 0;

    if (s < target) {
      const inc = Math.min(step, target - s);
      s += inc;
    } else if (bidirectional && s > target) {
      const dec = Math.min(step, s - target);
      s -= dec;
    }

    if (s !== (e.size | 0)) {
      e.size = s;
      changed = true;
    }
  }
  return changed;
}


// Recalibrate loop using the new global auction (no heap).
// Each iteration does:
//   (1) computeEmpireTotals()  -> updates e._value, e._avg, e.power
//   (2) adjustSizesOnceFromValue(threshold) -> sets e.size cap
//   (3) updateAllCostMaps() + recomputeOwnershipAuctionOptionA() -> assign cells
//   (4) repeat until the user stops, or break early if nothing changes
async function runRecalibrateWithDynamicSizes(btn) {
  // mark running and set button label
  btn.dataset.running = '1';
  const originalLabel = btn.textContent;
  btn.textContent = 'Stop';

  try {
    let iter = 0;

    // Prime base travel-cost maps once (terrain/capitals/speeds stable most of the time)
    await EmpireManager.updateAllCostMaps(grid);

    while (btn.dataset.running === '1') {
      const prevOwnerVersion = window._ownerVersion | 0;

      // 1) Totals + power from current territory (value + avg)
      computeEmpireTotals();

      // 2) Compute TARGET sizes, then nudge actual sizes up toward target
      computeTargetSizesFromValue(window.growthThreshold);
      const sizeChanged = nudgeSizesTowardTarget(window.autoGrowAmount, /*bidirectional=*/true);


      // 3) Full, atomic global auction assignment
      await recomputeOwnershipAuctionGlobal();

      // 4) Draw occasionally to keep UI snappy
      if ((iter++ % 2) === 0) drawCurrent();

      // Exit early if nothing changed (ownership and sizes stable)
      const ownerUnchanged = (window._ownerVersion | 0) === prevOwnerVersion;
      if (!sizeChanged && ownerUnchanged) break;

      // Yield to UI
      await new Promise(r => setTimeout(r, 0));
    }
  } finally {
    btn.dataset.running = '0';
    btn.textContent = originalLabel || 'Adjust size to land value';
    computeEmpireTotals();
    drawCurrent();
  }
}




// === Global auction-based reassignment (Option A++) ===
// Recomputes the entire ownership map in 3 passes using precomputed base costs.
// - Never assigns WATER
// - Deterministic tie-breaks (lower empire id, then lower cell idx)
// - Respects per-empire caps (emp.size)
// - Gives runner-up a chance, then a final fallback so small empires can still fill
// GLOBAL AUCTION (single-pass): assigns cells to the lowest-cost empires,
// respecting per-tick quotas, with no defense/reach special cases.
async function recomputeOwnershipAuctionGlobal() {
  // --- Config (deterministic) ---
  const EPS = 1e-6;   // tiny hysteresis for float ties
  const K   = 8;      // keep top-K empires per cell (4–8 is enough)


// 8-neighbour offsets (needed by eligibleFrontier)
const DX8 = [ 1, -1,  0,  0,  1,  1, -1, -1 ];
const DY8 = [ 0,  0,  1, -1,  1, -1,  1, -1 ];

  // --- Inputs & basics ---
  const emps = EmpireManager.empires;           // <— your project’s list
  const rows = grid.rows, cols = grid.cols, N = rows * cols;

  // Land mask (WATER never assigned)
  const isLand = new Uint8Array(N);
  for (let y = 0, i = 0; y < rows; y++) {
    const row = grid.cells[y];
    for (let x = 0; x < cols; x++, i++) {
      isLand[i] = (row[x].terrain === 'WATER') ? 0 : 1;
    }
  }


  // Bidding gates
const delta = Number(window.discriminationThreshold ?? window.growthThreshold ?? 1); // δ
const mu    = Math.max(0, Number(window.powerWeight ?? 0)); // μ in [0..1] (or 0.3), 0 disables power





  // Max id & power hook (power = 1 for now; keep the knob)
  let maxId = 0;
  for (const e of emps) if (e.id > maxId) maxId = e.id;


/// POWER EPSILON
  const EPSD = 1e-6;  // small distance epsilon for d=0 at capitals

// Flattened value map for quick access (and build isLand at the same time)
const valFlat = new Float32Array(N);
{
  let i = 0;
  for (let y = 0; y < rows; y++) {
    const row = grid.cells[y];
    for (let x = 0; x < cols; x++, i++) {
      const cell = row[x];
      // Use your actual value source; grid.getValueAt(x,y) if that’s your accessor
      const v = (typeof grid.getValueAt === 'function') ? grid.getValueAt(x, y)
                                                       : Number(cell.value ?? cell.landValue ?? 0);
      valFlat[i] = v;
      // (your isLand code is already present above; keep it as-is)
    }
  }
}




  // --- Previous ownership (only for measuring curr size; no incumbency) ---
  const ownerPrev = new Int16Array(N);
  for (const e of emps) {
    if (!e.territory) continue;
    for (const idx of e.territory) ownerPrev[idx] = e.id;
  }



  // --- Per-tick quotas (your "good cap"): total cells each empire may end with this tick ---
  const curr = new Int32Array(maxId + 1);
  for (let i = 0; i < N; i++) { const id = ownerPrev[i]; if (id > 0) curr[id]++; }

 const target = new Int32Array(maxId + 1);
for (const e of emps) {
  // use target size if present; else use UI size slider
  target[e.id] = (e._targetSize != null) ? (e._targetSize | 0) : (e.size | 0);
}




// S_e = max(1, (avg - threshold)+ * size)
// We'll derive avg from current ownership to avoid relying on other places.
const thr = Number(window.growthThreshold ?? 0) || 0;

// From ownerPrev we already have curr sizes; accumulate value sums per empire
const sumVal = new Float64Array(maxId + 1);
for (let i = 0; i < N; i++) {
  const id = ownerPrev[i];
  if (id > 0) sumVal[id] += valFlat[i];
}

// Compute per-empire power P = sqrt( max(0, avg - thr) * area )
const P_byId = new Float32Array(maxId + 1);
{
  const thr = Number(window.growthThreshold ?? 0) || 0;

  for (const e of emps) {
    const id = e.id;
    const n  = curr[id] | 0;
    const avg = (n > 0) ? (sumVal[id] / n) : 0;
    const surplus = Math.max(0, avg - thr);
    let P = Math.sqrt(surplus * n) || 0;

    if (window.powerScale != null) {
      const s = Number(window.powerScale) || 1;
      P *= s;
    }
    P_byId[id] = P;
  }
}


  const G = Math.max(1, (Number(window.assignGrowthStep ?? window.autoGrowAmount ?? 5) | 0));
  const quota = new Int32Array(maxId + 1);
  let sumQuota = 0;
  for (const e of emps) {
    const q = Math.max(0, Math.min(target[e.id], curr[e.id] + G));
    quota[e.id] = q;
    sumQuota += q;
  }

  // Ensure all active empires have a ready cost map with finite cost at their capital
let mapsReady = true;
for (const e of emps) {
  if (!e.capital) continue;
  const base = e.costMapFlat;
  if (!base || base.length !== N) { mapsReady = false; break; }
  const ci = e.capital.y * cols + e.capital.x;
  if (!(base[ci] < Infinity)) { mapsReady = false; break; } // capital must be reachable
}
if (!mapsReady) {
  // Compute cost maps before assigning, then proceed once ready
  await EmpireManager.updateAllCostMaps(grid);
  // (Optional) re-check here; in practice the awaited call is sufficient
}




// Map empire id -> flat capital index (or -1 if none)
const capIdxById = new Int32Array(maxId + 1);
capIdxById.fill(-1);
for (const e of emps) {
  if (e.capital) {
    capIdxById[e.id] = e.capital.y * cols + e.capital.x;
  }
}

// Quick access to cost maps by id (for defender distance on hostile tiles)
const costById = new Array(maxId + 1);
for (const e of emps) costById[e.id] = e.costMapFlat || null;

// Frontier adjacency: cell i is eligible for empire id if
// - it was owned by id last tick, OR
// - it is the capital tile itself (seed), OR
// - any of its 8-neighbors was owned by id last tick, OR
// - it is adjacent to the capital tile (for the very first tick)
function eligibleFrontier(id, i) {
  if (id <= 0) return false;

  //// 1) Always allow defending/retaining your own previous tiles
  //if (ownerPrev[i] === id) return true;

  const capIdx = capIdxById[id];

  // 2) Full border from last tick: any of 8 neighbors owned by id last tick
  const x = i % cols, y = (i / cols) | 0;
  for (let k = 0; k < 8; k++) {
    const nx = x + DX8[k], ny = y + DY8[k];
    if (nx < 0 || nx >= cols || ny < 0 || ny >= rows) continue;
    const ni = ny * cols + nx;
    if (ownerPrev[ni] === id) return true;
  }

  // 3) First-seed convenience: ONLY if empire has no territory yet,
  // allow the capital tile and its 8-neighbors as eligible.
  if ((curr[id] | 0) === 0 && capIdx >= 0) {
    if (i === capIdx) return true;
    const cx = capIdx % cols, cy = (capIdx / cols) | 0;
    for (let k = 0; k < 8; k++) {
      const nx = cx + DX8[k], ny = cy + DY8[k];
      if (nx < 0 || nx >= cols || ny < 0 || ny >= rows) continue;
      const ni = ny * cols + nx;
      if (ni === i) return true;
    }
  }

  return false;
}






  // --- Top-K candidates per cell (sorted by ascending cost) ---
  // Store as structure-of-arrays for speed/determinism.
  const topKId   = new Int16Array(N * K);
  const topKCost = new Float32Array(N * K);
  const topKLen  = new Uint8Array(N);
  for (let i = 0; i < topKCost.length; i++) topKCost[i] = Infinity;

  // Scan by empire (cache-friendly over costMapFlat)
  for (const e of emps) {
    const id = e.id;
    const base = e.costMapFlat;
    if (!base || base.length !== N) continue;
    //const inv = invPow[id];

    for (let i = 0; i < N; i++) {
      if (!isLand[i]) continue;

const d = base[i];
if (!(d < Infinity)) continue;

    const v = valFlat[i];

    // --- Eligibility: frontier OR reachable by WATER from any owned cell
    const eligible =
      eligibleFrontier(id, i) ||
      reachableByWaterFromOwned(e, i, ownerPrev, grid);
    if (!eligible) continue;

    // --- Discrimination: only consider cells with value >= δ
    if (!(v >= delta)) continue;

    // --- Pure distance bidding (no benefit division, no hostile bias, no thinning)
    const c = d + EPSD;

      const off = i * K;
      let len = topKLen[i];

      // Fast drop if full and new cost not better than worst
      if (len === K) {
        const worstC  = topKCost[off + (K - 1)];
        const worstId = topKId  [off + (K - 1)];
        if (!((c < worstC - EPS) || (Math.abs(c - worstC) <= EPS && id < worstId))) continue;
      }

      // Insert in sorted position (stable tie: id)
      let pos = Math.min(len, K - 1);
      while (pos > 0) {
        const prevC  = topKCost[off + (pos - 1)];
        const prevId = topKId  [off + (pos - 1)];
        if ((c < prevC - EPS) || (Math.abs(c - prevC) <= EPS && id < prevId)) {
          topKCost[off + pos] = prevC;
          topKId  [off + pos] = prevId;
          pos--;
        } else break;
      }
      topKCost[off + pos] = c;
      topKId  [off + pos] = id;
      if (len < K) len++;
      topKLen[i] = len;
    }
  }

  // --- Min-heap over (cost, cellIdx, empireId, ptrIntoTopK) ---
  const heapCost = []; const heapCell = []; const heapEmp = []; const heapPtr = []; let H = 0;
  function less(a, b) {
    const da = heapCost[a], db = heapCost[b];
    if (Math.abs(da - db) > EPS) return da < db;
    if (heapCell[a] !== heapCell[b]) return heapCell[a] < heapCell[b];
    return heapEmp[a] < heapEmp[b];
  }
  function swap(a, b) {
    [heapCost[a], heapCost[b]] = [heapCost[b], heapCost[a]];
    [heapCell[a], heapCell[b]] = [heapCell[b], heapCell[a]];
    [heapEmp [a], heapEmp [b]] = [heapEmp [b], heapEmp [a]];
    [heapPtr [a], heapPtr [b]] = [heapPtr [b], heapPtr [a]];
  }
  function push(cost, cell, emp, ptr) {
    heapCost[H] = cost; heapCell[H] = cell; heapEmp[H] = emp; heapPtr[H] = ptr;
    let i = H++; while (i > 0) { const p = (i - 1) >> 1; if (less(i, p)) { swap(i, p); i = p; } else break; }
  }
  function pop() {
    if (H <= 0) return null;
    const out = { cost: heapCost[0], cell: heapCell[0], emp: heapEmp[0], ptr: heapPtr[0] };
    const last = --H;
    if (last >= 0) {
      heapCost[0] = heapCost[last]; heapCell[0] = heapCell[last];
      heapEmp [0] = heapEmp [last]; heapPtr [0] = heapPtr [last];
    }
    let i = 0; for (;;) {
      const l = i*2+1, r = l+1; if (l >= H) break;
      const m = (r < H && less(r, l)) ? r : l;
      if (less(m, i)) { swap(m, i); i = m; } else break;
    }
    return out;
  }

  // Track which per-cell candidates we've already pushed
const topKUsed = new Uint8Array(N * K);

// Helper: for a given cell i, pick the unused candidate j that minimizes
// effective cost = distance / (1 + mu * P_byId[id])
function pickBestByEffective(i) {
  const off = i * K;
  const len = topKLen[i];
  let bestJ = -1, bestEff = Infinity;

  for (let j = 0; j < len; j++) {
    if (topKUsed[off + j]) continue;
    const id2 = topKId  [off + j];
    const d2  = topKCost[off + j];     // stored as PURE distance
    const eff = d2 / (1 + mu * (P_byId[id2] || 0));
    if (eff < bestEff) { bestEff = eff; bestJ = j; }
  }
  return (bestJ >= 0) ? { j: bestJ, eff: bestEff } : null;
}

// Seed heap: best (distance+power) candidate per cell (topKCost holds raw distance)
for (let i = 0; i < N; i++) {
  if (!isLand[i]) continue;
  if (topKLen[i] === 0) continue;

  const pick = pickBestByEffective(i);
  if (!pick) continue;

  const j   = pick.j;
  const id  = topKId  [i*K + j];
  const eff = pick.eff;

  topKUsed[i*K + j] = 1;
  push(eff, i, id, j);  // heap key is effective cost (distance adjusted by power)
}


  // --- Single global auction ---
  const owner = new Int16Array(N); // 0 = neutral/water initially
  while (H > 0 && sumQuota > 0) {
    const it = pop(); if (!it) break;
    const i = it.cell, e = it.emp, p = it.ptr;

    if (owner[i] !== 0) continue;        // cell already assigned

    if (quota[e] > 0) {
      owner[i] = e;                      // assign to this empire
      quota[e]--; sumQuota--;
      continue;
    }

// No quota: advance this cell to its NEXT best by effective cost
const pick = pickBestByEffective(i);
if (pick) {
  const j2  = pick.j;
  const id2 = topKId  [i*K + j2];
  const eff = pick.eff;
  topKUsed[i*K + j2] = 1;
  push(eff, i, id2, j2);
}

  }

// --- Rebuild territories from owner[] deterministically ---

// 0) Build O(1) lookup from id -> empire (avoid O(E) .find per cell)
const idToEmp = new Array(maxId + 1);
for (const e of emps) idToEmp[e.id] = e;

// 1) Clear (or create) existing sets once
for (const e of emps) {
  if (e.territory instanceof Set) e.territory.clear();
  else e.territory = new Set();
}

// 2) Single linear pass filling sets via O(1) lookup
for (let i = 0; i < N; i++) {
  const id = owner[i];
  if (id > 0) {
    const e = idToEmp[id];
    if (e) e.territory.add(i);
  }
}

// --- Elimination & cleanup: (1) capital taken by others  (2) zero-size empires ---
{
  const toRemove = [];

  // (1) Capital taken by someone else
  for (const e of emps) {
    if (!e.capital) continue;
    const capIdx   = e.capital.y * cols + e.capital.x;
    const capOwner = owner[capIdx] | 0;
    if (capOwner > 0 && capOwner !== e.id) {
      toRemove.push(e);
    }
  }

  // (2) Zero-size empires (no cells at all → remove, even if not overrun)
  for (const e of emps) {
    const area = e && e.territory ? e.territory.size : 0;
    if (area === 0) {
      toRemove.push(e);
    }
  }

  // Deduplicate (in case an empire matches both rules)
  const seen = new Set();
  const unique = [];
  for (const e of toRemove) {
    if (!e || seen.has(e.id)) continue;
    seen.add(e.id);
    unique.push(e);
  }

  // Remove from the board + from the UI + from the model
  for (const e of unique) {
    // Clear any leftover tiles on the board
    for (let i = 0; i < N; i++) {
      if (owner[i] === e.id) owner[i] = 0;
    }

    // Mark and remove from your model
    e._dead = true;
    EmpireManager.removeEmpire(e.id);

    // Remove the control panel on the left, if it exists
    const panel = document.getElementById(`empire-panel-${e.id}`);
    if (panel && panel.parentNode) panel.parentNode.removeChild(panel);
  }
}

  // Notify visuals that ownership changed
  window._ownerVersion = (window._ownerVersion | 0) + 1;

  // Done
  return owner;
}

////////////
//////////// END OF GLOBAL ASSIGNMENT FUNCTION
////////////


// Recompute only the travel-cost maps for each empire (no territory changes)
async function recomputeCostMapsOnly() {
  // If you want land totals to affect only simulation, we can skip computeEmpireTotals() here.
  await EmpireManager.updateAllCostMaps(grid);
}

// Expose for other files / UI helpers
window.recomputeCostMapsOnly = recomputeCostMapsOnly;



// Recompute full assignment using worker cost maps + auction
async function simulateAndDraw() {

  // 1) Refresh land totals so power reflects current average value
  computeEmpireTotals();

  // 2) Ensure all empires have up-to-date base travel-cost maps
  //    (Terrain/capital/speed changes → rerun Dijkstra in the worker)
  await EmpireManager.updateAllCostMaps(grid);   // returns when all worker jobs finish
  //      ^ This uses the pool + transfers terrain bytes, sets emp.costMapFlat. :contentReference[oaicite:5]{index=5} :contentReference[oaicite:6]{index=6}

  // 3) Global auction recompute (winner + runner-up + fallback)
  await recomputeOwnershipAuctionGlobal();

  // 4) Keep “Size: N” labels in sync
  for (const emp of EmpireManager.empires) {
    if (emp._sizeDisplay) emp._sizeDisplay.textContent = `Size: ${emp.territory.size}`;

    window.renderLeaderboard?.();
  }
}



// Debounced + non-overlapping recompute for UI changes.
// Triggers if a heatmap is active OR a route is currently displayed.
window._recomputeFromSlidersRunning = false;
window._recomputeFromSlidersQueued  = false;

window.requestRecomputeFromSliders = function () {
  const need =
    !!window.currentHeatEmpire ||
    (!!window.currentRouteEmpire && !!window.currentRouteTarget);
  if (!need) return;

  window._recomputeFromSlidersQueued = true;
  scheduleRecomputeFromSliders();
};

function scheduleRecomputeFromSliders() {
  if (window._recomputeFromSlidersRunning) return;
  if (!window._recomputeFromSlidersQueued) return;

  window._recomputeFromSlidersQueued  = false;
  window._recomputeFromSlidersRunning = true;

  (async () => {
    try {
      if (typeof window.recomputeCostMapsOnly === 'function') {
        await window.recomputeCostMapsOnly();
      }
      // drawCurrent() will redraw heatmap and/or the route using fresh parents
      window.drawCurrent?.();
    } finally {
      window._recomputeFromSlidersRunning = false;
      if (window._recomputeFromSlidersQueued) scheduleRecomputeFromSliders();
    }
  })();
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

      window.renderLeaderboard?.();


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

