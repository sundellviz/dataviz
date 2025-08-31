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
  Math.min((navigator.hardwareConcurrency || 4), 8) // e.g., 6 cores => 6 workers (capped at 8)
);

const pathPool = new PathWorkerPool('js/pathfindingWorker.js', _POOL_SIZE);



// offload one empire’s cost-map job to the pool
function computeCostMapOffload(emp, grid, ownerIdFlat, penalty) {
  const id = ++_pfMsgId;

  return new Promise(resolve => {
    _pfPending.set(id, resolve);

    const payload = {
      id,
      empireId: emp.id,
      rows: grid.rows,
      cols: grid.cols,
      terrains: grid.cells.map(r => r.map(c => c.terrain)),
      travelSpeeds: emp.travelSpeeds,
      capital: emp.capital,

      // NEW: territory-aware penalty inputs
      ownerIdFlat,
      penaltyScale: penalty?.penaltyScale ?? 1.0,
      penaltyGamma: penalty?.penaltyGamma ?? 1.0
    };

    // No transfer list needed; simple clone is fine for these sizes.
    pathPool.postMessage(payload);
  });
}
 // ───────────────────────────────────────────────────────



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

// ← insert offscreen setup here:
const offscreen = document.createElement('canvas');
const offCtx    = offscreen.getContext('2d');

// Recalibrate button
const recBtn = document.getElementById('recalibrate-btn');
recBtn.addEventListener('click', () => {
  if (!window.isRecalibrating) {
    // start recalibration
    window.isRecalibrating   = true;
    window.recalibrateCancel = false;
    recBtn.textContent       = 'Stop Recalibrate';
    recalibrateTerritory()
      .catch(() => {})      // ignore cancellation “errors”
      .finally(() => {
        // reset button when done or cancelled
        window.isRecalibrating   = false;
        window.recalibrateCancel = false;
        recBtn.textContent       = 'Recalibrate';
      });
  } else {
    // request cancellation
    window.recalibrateCancel = true;
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



  const canvasSizeSlider     = document.getElementById('canvas-size-slider');
  const canvasSizeDisplay    = document.getElementById('canvas-size-display');

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
  gridWidthDisplay.textContent = gridWidth;
  rebuildGrid();
});

gridHeightSlider.addEventListener('input', () => {
  gridHeight = parseInt(gridHeightSlider.value, 10);
  gridHeightDisplay.textContent = gridHeight;
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
    window.currentMode   = 'placeCapital';
    window.currentEmpire = emp;
    alert(`Click on the map to place the capital for '${emp.name}'`);
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

  // resize canvas height so cells stay square
  const cellSize = canvas.width / gridWidth;
  canvas.height = cellSize * gridHeight;
  canvas.style.height = canvas.height + 'px';

  // regenerate variants, shading, and redraw
  generateVariantGrid();
  precomputeWaterShading(grid);
  resizeCanvases();
  renderBackground();
  simulateAndDraw();
}

  // --- State ---
  let grid     = new Grid(gridWidth, gridHeight);
 window.grid  = grid;
  let cellSize = canvas.width / grid.cols;

  // --- Paint Mode Toggle ---
  const paintModeBtn = document.getElementById('paint-mode-btn');
  let paintMode = false;
  paintModeBtn.addEventListener('click', () => {
    paintMode = !paintMode;
    paintModeBtn.textContent = paintMode
      ? 'Disable Paint Mode'
      : 'Enable Paint Mode';
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
  const defaults = { PLAIN:40, DESERT:10, WATER:40, MOUNTAIN:10, FOREST:10, RIVER:5 , ICE:5};
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

// Randomize terrain
const randHeader   = document.getElementById('randomize-header');
const randControls = document.getElementById('randomize-controls');
randHeader.addEventListener('click', () => {
  const hidden = randControls.classList.toggle('hidden');
  randHeader.textContent = hidden 
    ? 'Randomize terrain ▶' 
    : 'Randomize terrain ▼';
});



 // turn grid off by default
 let showGrid = false;

 // and update the toggle‐button label to match
 toggleGridBtn.textContent = showGrid
   ? 'Hide Grid Lines'
   : 'Show Grid Lines';



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
window.precomputeWaterShading(grid);

  // --- Draw Helper ---
  // render static terrain+shading into our offscreen buffer
  function renderBackground() {
    offCtx.clearRect(0, 0, offscreen.width, offscreen.height);
    // compute identical cellSize to what drawCurrent will use:
    const cellSize = offscreen.width / grid.cols; 
    drawGrid(offCtx, grid, cellSize, showGrid);
  }

  function drawCurrent() {
  // 1) recompute cellSize to match current canvas & grid
  const cellSize = canvas.width / grid.cols;

  // 2) draw the cached terrain+shading
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
    RIVER:  'R',
    ICE:  'I'
  };
  const CHAR_TERRAIN = Object.fromEntries(
    Object.entries(TERRAIN_CHAR).map(([k,v]) => [v,k])
  );

// --- Canvas Size Slider (maintain rows×cols aspect) ---
canvasSizeSlider.addEventListener('input', () => {
  const w = parseInt(canvasSizeSlider.value, 10);
  canvasSizeDisplay.textContent = w;

  // set new width
  canvas.width = w;

  // compute height so cells stay square
  // (grid.rows / grid.cols) is the desired aspect
  const h = Math.round(w * (grid.rows / grid.cols));
  canvas.height = h;

  // update CSS size
  canvas.style.width  = w + 'px';
  canvas.style.height = h + 'px';

    // keep our offscreen canvas in sync and redraw terrain layer
    window.precomputeWaterShading(grid);
    resizeCanvases();
    renderBackground();
    


  // drawCurrent();
});

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
    generateVariantGrid();

    window.precomputeWaterShading(grid);
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

    // ↪ recompute cellSize *after* any resizes
  const cellW = canvas.width  / grid.cols;
  const cellH = canvas.height / grid.rows;
  const x     = Math.floor(sx / cellW);
  const y     = Math.floor(sy / cellH);



    if (x>=0&&x<grid.cols&&y>=0&&y<grid.rows) {
// anchor the square so it’s centered on the click
const half = Math.floor(brushSize / 2);
for (let dy = -half; dy <= half; dy++) {
  for (let dx = -half; dx <= half; dx++) {
    const xx = x + dx, yy = y + dy;
    if (xx < 0 || xx >= grid.cols || yy < 0 || yy >= grid.rows) continue;
    grid.cells[yy][xx].terrain = paintType;
    window.variantGrid[yy][xx] = Math.floor(
      Math.random() *
      (TERRAIN_VARIANTS[paintType] || [TERRAIN[paintType].color]).length
    );
  }
}




      window.precomputeWaterShading(grid);
renderBackground();
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
  
  window.gridWidth  = cols;
  window.gridHeight = rows;

  // resize canvas for new aspect ratio
  const cs = canvas.width / cols;
  canvas.height      = cs * rows;
  canvas.style.height = canvas.height + 'px';

  // populate terrains
  mapTokens.forEach((rowTokens, y) => {
    rowTokens.forEach((tok, x) => {
      grid.cells[y][x].terrain = CHAR_TERRAIN[tok] || 'PLAIN';
    });
  });

  // regenerate variants + shading + redraw
  generateVariantGrid();
  window.precomputeWaterShading(grid);
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


          for (let t in cfg.travelSpeeds) {
            const s = emp._speedSliders[t];
            const v = emp._speedValues[t];
            if (s && v) {
              s.value       = cfg.travelSpeeds[t];
              v.textContent = Number(cfg.travelSpeeds[t]).toFixed(1);
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



  // --- Toggle Grid Lines ---
 toggleGridBtn.addEventListener('click', () => {
  showGrid = !showGrid;
  toggleGridBtn.textContent = showGrid ? 'Hide Grid Lines' : 'Show Grid Lines';
  renderBackground();
});


// --- 1) Single-ring simulation step (exactly your old simulateAndDraw logic, minus the draw) ---
async function simulateOneRing() {
  // 1a) Recompute cost maps (hostile-cell barrier + penalty)
  await EmpireManager.updateAllCostMaps(grid);

  const rows = grid.rows, cols = grid.cols, N = rows * cols;

// Build ownerId[] from current territories (BEFORE we clear them in 1c)
const ownerId = new Int32Array(N);
for (const e of EmpireManager.empires) {
  if (!e.territory) continue;
  for (const idx of e.territory) ownerId[idx] = e.id;
}

// Quick id → empire lookup
const idToEmpire = new Map(EmpireManager.empires.map(e => [e.id, e]));

// Also reset the per-empire transit memo (one small map per empire)
for (const e of EmpireManager.empires) e._transitCache = new Map();

 // 1b) Collect all reachable, non-water cells, filtering out those whose best route crosses enemy land
const all = [];

EmpireManager.empires.forEach(emp => {
  const cm = (emp.costMapFlat instanceof Float32Array) ? emp.costMapFlat : emp.costMap;

  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      if (grid.cells[y][x].terrain === 'WATER') continue;

      const cost = (cm instanceof Float32Array) ? cm[y * cols + x] : cm[y][x];
      if (!isFinite(cost)) continue;

      const idx = y * cols + x;
      const owner = ownerId[idx] ? idToEmpire.get(ownerId[idx]) : null;

      // ENEMY or clean NEUTRAL/OWNED → allowed
      all.push({ emp, x, y, cost });
    }
  }
});

  // 1c) Global sort, clear, assign exactly `size` cells per empire, one owner per cell
  all.sort((a, b) => a.cost - b.cost);

  // Important: we only clear after 1b, so the owner check above sees last ring's territory
  EmpireManager.empires.forEach(emp => emp.territory.clear());

  const taken  = new Set();
  const counts = {};
  EmpireManager.empires.forEach(emp => counts[emp.id] = 0);

  for (const { emp, x, y } of all) {
    if (counts[emp.id] >= emp.size) continue;
    const idx = y * cols + x;
    if (taken.has(idx)) continue;
    emp.territory.add(idx);
    taken.add(idx);
    counts[emp.id]++;
  }

  // Update the per-empire "Size: N" label in each panel
for (const emp of EmpireManager.empires) {
  if (emp._sizeDisplay) {
    emp._sizeDisplay.textContent = `Size: ${emp.territory.size}`;
  }
}

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
    drawCurrent();

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
  drawCurrent();
}



  window.simulateAndDraw = simulateAndDraw;

  // --- Initial Render & UI Boot ---
  generateVariantGrid();
  window.precomputeWaterShading(grid);
  resizeCanvases();
  renderBackground();
  

  simulateAndDraw();

initTerrainMenu();


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

