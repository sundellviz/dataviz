// js/empires.js

class Empire {
  constructor(id, name, color) {
    this.id           = id;
    this.name         = name || `Empire ${id}`;
    this.color        = color || '#ff000080';
    this.capital      = null;      // { x, y }
    this._costMapDirty = false;
    this._borderPath = null;       // ← ADD THIS LINE
    this._borderVersion = -1;      // ← ADD THIS LINE
this.travelSpeeds = {
  PLAIN:    2.0,
  DESERT:   4.0,
  WATER:    1.5,
  OCEAN:    3.0,
  MOUNTAIN: 6.0,
  FOREST:   3.0,
  SHRUB:    3.0,
  RIVER:    1.5,
  ICE:      7.0,
  SWITCH:   0.0
};
this.size       = 50;           // target number of cells
this.power      = 1.0;          // ← NEW: influences (step_cost / power)
this.territory  = new Set();    // Set<number> of flat indices
this.costMap    = [];           // (kept for UI/heatmap compatibility; not used by global-heap)
this.parentMap  = [];
  }
}

// 10-cycle default colors (opaque #RRGGBB; we append '80' alpha when used)
const DEFAULT_EMPIRE_COLORS = [
  '#ff0000', // 1 Red
  '#0066ff', // 2 Blue
  '#ffd400', // 3 Yellow
  '#00c853', // 4 Green
  '#9c27b0', // 5 Purple
  '#ff6d00', // 6 Orange
  '#00bcd4', // 7 Cyan/Teal
  '#e91e63', // 8 Magenta/Pink
  '#795548', // 9 Brown
  '#3f51b5', // 10 Indigo
];





// EMPIRE MANAGER

const EmpireManager = {
  empires: [],
  nextId: 1,
  nextColorIdx: 0,

   // async now returns after *all* worker jobs finish
   // async now returns after *all* worker jobs finish
async updateAllCostMaps(grid, options = {}) {
  const rows = grid.rows, cols = grid.cols;

  // Option to skip ghost empires for performance during auction bidding
  // Ghost empires still need cost maps for trade route computation
  const skipGhosts = options.skipGhosts ?? false;
  const ghostCitiesEnabled = window.ghostCitiesEnabled !== false;

  // Filter empires to compute cost maps for
  const empiresToProcess = EmpireManager.empires.filter(emp => {
    // Must have a capital to compute cost map from
    if (!emp.capital) return false;

    // Always process empires with territory (active empires)
    if (emp.territory && emp.territory.size > 0) return true;

    // Ghost empire (no territory) - check if we should skip
    if (skipGhosts) {
      // Even when skipping ghosts, include if cost map is dirty or missing
      // (needed for initial computation)
      if (emp._costMapDirty) return true;
      if (!(emp.costMapFlat instanceof Float32Array) || emp.costMapFlat.length === 0) return true;
      return false;
    }

    // Include ghost if ghost cities are enabled (they participate in trade)
    // Trade routes affect power even when trade view is not active
    if (ghostCitiesEnabled) return true;

    return false;
  });

  // Kick off one worker job per empire
  const jobs = empiresToProcess.map(emp =>
    computeCostMapOffload(emp, grid).then((msg) => {
      // Accept any of the shapes we might get back:
      // - { dist, parentIdx }     // Float32 (new)
      // - { dist64, parentIdx }   // Float64 (older)
      // - { costMap, parentMap }  // legacy 2D arrays (fallback)
      const { dist, dist64, parentIdx, costMap, parentMap } = msg || {};
      const flat = dist || dist64;

      if (flat && parentIdx instanceof Int32Array) {
        // Prefer compact typed arrays
        // Normalize to Float32 once (saves RAM if worker ever sends Float64)
        emp.costMapFlat = (flat instanceof Float64Array) ? new Float32Array(flat) : flat;
        emp.parentIdx   = parentIdx;

        // Free legacy structures so downstream always prefers the flat buffers
        emp.costMap   = null;
        emp.parentMap = null;
      } else {
        // Legacy path unchanged
        emp.costMap     = costMap || null;
        emp.parentMap   = parentMap || null;
        emp.costMapFlat = null;
        emp.parentIdx   = null;
      }
    })
  );

  // Wait for all
  await Promise.all(jobs);
},

_allEmpires: [],  // tracks every empire ever added (for reset)

addEmpire(name, color) {
  // If no color provided, pick the next palette color (with 0x80 alpha)
  let chosen = color;
  if (!chosen) {
    const base = DEFAULT_EMPIRE_COLORS[this.nextColorIdx % DEFAULT_EMPIRE_COLORS.length];
    chosen = base + '80'; // keep your semi-transparent look
    this.nextColorIdx = (this.nextColorIdx + 1) % DEFAULT_EMPIRE_COLORS.length;
  }
  const e = new Empire(this.nextId++, name, chosen);
  this.empires.push(e);
  this._allEmpires.push(e);
  return e;
},

  removeEmpire(id) {
    this.empires = this.empires.filter(e => e.id !== id);
  },

  resetAll() {
    const grid = window.grid;
    const cols = grid ? grid.cols : 0;

    // Restore all empires ever added
    this.empires = this._allEmpires.slice();

    for (const e of this.empires) {
      e._dead = false;
      e.territory = new Set();
      e._costMapDirty = true;
      e._borderPath = null;
      e._borderVersion = -1;
      e.costMapFlat = null;
      e.parentIdx = null;
      e.costMap = [];
      e.parentMap = [];

      // Seed territory with just the capital cell (size 1)
      if (e.capital && grid) {
        e.territory.add(e.capital.y * cols + e.capital.x);
      }
    }

    // Rebuild all UI panels
    const container = document.getElementById('empire-panels');
    if (container) container.innerHTML = '';
    for (const e of this.empires) {
      if (typeof window.createEmpirePanel === 'function') {
        window.createEmpirePanel(e);
      }
    }

    if (typeof window.drawCurrent === 'function') window.drawCurrent();
  }
};


/**
 * Build the DOM panel & wire every UI control for an empire.
 * (name edit, color picker, size & speed sliders,
 *  place-capital, heatmap toggle, route-finding, remove)
 */
function createEmpirePanel(emp) {
  const container = document.getElementById('empire-panels');
  const panel = document.createElement('details');
  panel.id     = `empire-panel-${emp.id}`;
  panel.open   = true;
  panel.style.margin = '6px 0';

  // Inline HTML for all controls
  panel.innerHTML = `
  <summary style="
      display:flex; align-items:center; justify-content:space-between;
      padding:6px 8px; background:${emp.color}; border-radius:10px 10px 0 0; cursor:pointer;">
    <span class="empire-name" style="font-weight:700">${emp.name}</span>
    <input type="color" class="empire-color" value="${emp.color.slice(0,7)}" title="Color"/>
  </summary>

  <div class="empire-controls">
    <div class="meta-row">
      <div class="capital-display pill">Capital: (–,–)</div>
      <div class="size-display pill">Size: 0</div>
      <div class="value-display pill">Land value: 0</div>
    </div>



    <hr/>
    <div style="font-weight:600; color: var(--muted); margin-bottom:4px;">Travel costs</div>
    <div class="speed-sliders"></div>

    <div class="panel-actions">
      <button class="place-capital-btn">Place Capital</button>
      <button class="heatmap-btn btn-secondary">Show Heatmap</button>
      <button class="route-btn btn-secondary">Find Route</button>
      <button class="remove-btn btn-danger">Remove Empire</button>
    </div>
  </div>
`;
  container.append(panel);

// --- Inline rename in the header (robust: swap span <-> input) ---
const summary  = panel.querySelector('summary');
const nameSpan = summary.querySelector('.empire-name');
nameSpan.title = 'Click to rename';
nameSpan.style.cursor = 'text';

const stopToggle = (e) => { e.stopPropagation(); e.preventDefault(); };

function applyName(newText) {
  const clean = (newText || '').trim();
  emp.name = clean || `Empire ${emp.id}`;
  nameSpan.textContent = emp.name;
  if (window.drawCurrent) window.drawCurrent(); // update labels on map
}

function startNameEdit() {
  const input = document.createElement('input');
  input.type = 'text';
  input.value = emp.name;
  input.className = 'empire-name-edit';
  input.style.flex = '1 1 auto';
  input.style.minWidth = '60px';
  input.style.maxWidth = '100%';
  input.style.background = 'rgba(255,255,255,.08)';
  input.style.color = '#fff';
  input.style.border = '1px solid rgba(255,255,255,.25)';
  input.style.borderRadius = '4px';
  input.style.padding = '2px 6px';
  input.style.font = 'inherit';

  const originalName = emp.name;
  let cancelled = false;
  let closed = false; // guard against double-close

  summary.replaceChild(input, nameSpan);
  input.focus();
  input.select();

  const cleanup = () => {
    input.removeEventListener('keydown', onKey);
    input.removeEventListener('mousedown', stopToggle);
    input.removeEventListener('click', stopToggle);
    input.removeEventListener('blur', onBlur);
  };

  const onBlur = () => {
    if (closed) return;
    closed = true;

    if (cancelled) {
      nameSpan.textContent = originalName;
    } else {
      applyName(input.value);
    }
    if (input.parentNode === summary) {
      summary.replaceChild(nameSpan, input);
    } else if (!nameSpan.isConnected) {
      // safety: if something moved, put the span back
      summary.appendChild(nameSpan);
    }
    cleanup();
  };

  const onKey = (e) => {
    if (e.key === 'Enter') {
      e.preventDefault();
      cancelled = false;
      input.blur(); // let blur commit once
    } else if (e.key === 'Escape') {
      e.preventDefault();
      cancelled = true;
      input.blur(); // let blur cancel once
    }
  };

  input.addEventListener('keydown', onKey);
  input.addEventListener('mousedown', stopToggle);
  input.addEventListener('click', stopToggle);
  input.addEventListener('blur', onBlur);
}

nameSpan.addEventListener('mousedown', stopToggle);
nameSpan.addEventListener('click', (e) => { stopToggle(e); startNameEdit(); });


  // — Color Picker —
  const colorInput = panel.querySelector('.empire-color');
  colorInput.addEventListener('input', () => {
    emp.color = colorInput.value + '80';
    summary.style.background = emp.color;
    window.drawCurrent();
  });

// — Travel Speed Sliders —
const speedDiv = panel.querySelector('.speed-sliders');
speedDiv.style.display = 'grid';
speedDiv.style.rowGap  = '2px';
speedDiv.style.margin  = '2px 0';

emp._speedSliders = {};
emp._speedValues  = {};   // will hold the number <input> (not a span)

const KEYS = (window.TERRAIN_KEYS || Object.keys(TERRAIN));

// Ensure a speeds object exists
if (!emp.travelSpeeds) emp.travelSpeeds = {};

for (const t of KEYS) {
  // Backfill if the import didn’t have this terrain yet
  if (emp.travelSpeeds[t] == null) {
    const fallback = window.globalTravelSpeeds?.[t] ?? 5.0;  // default cost
    emp.travelSpeeds[t] = fallback;
  }

  // Row layout: label | slider | number (like Target size)
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
  slider.value = String(emp.travelSpeeds[t]);
  slider.style.width  = '100%';
  slider.style.margin = '0';

  const num = document.createElement('input');
num.type  = 'number';
num.min   = '0';
num.removeAttribute('max');   // ← allow any high value
num.step  = '0.1';
num.value = Number(emp.travelSpeeds[t]).toFixed(1);

// Keep the slider only for quick scrubbing (0..10)
function applyFromSlider(v) {
  let val = Math.max(0, Math.min(10, Math.round(parseFloat(v || 0) * 10) / 10));
  emp.travelSpeeds[t] = val;
  slider.value = String(val);            // slider shows its capped value
  num.value    = val.toFixed(1);         // box shows that same capped value (until user edits)
  window.drawCurrent?.();

  emp._costMapDirty = true;
  window.invalidateTradeRoutes?.();

  // Live recompute only if a heatmap is active
  window.requestRecomputeFromSliders?.();
}

// Let the number box exceed slider’s max
function applyFromNumber(v) {
  let raw = parseFloat(v || 0);
  let val = Math.max(0, Math.round(raw * 10) / 10); // no upper cap
  emp.travelSpeeds[t] = val;
  slider.value = String(Math.min(10, val));         // slider parks at top if val > 10
  num.value    = val.toFixed(1);                    // box shows full value (e.g., 25.0)
  window.drawCurrent?.();

  emp._costMapDirty = true;
  window.invalidateTradeRoutes?.();

  // Live recompute only if a heatmap is active
  window.requestRecomputeFromSliders?.();
}

slider.addEventListener('input', () => applyFromSlider(slider.value));
num.addEventListener('change',   () => applyFromNumber(num.value));

  row.append(name, slider, num);
  speedDiv.appendChild(row);

  // references for programmatic updates (optimizers, import, globals)
  emp._speedSliders[t] = slider;
  emp._speedValues[t]  = num;
}

// — Switching cost (one slider, same scale; 0 turns it off) —
{
  if (emp.travelSpeeds.SWITCH == null) emp.travelSpeeds.SWITCH = 0.0;

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
  slider.min   = '0';     // allow 0 to fully disable
  slider.max   = '10';
  slider.step  = '0.1';
  slider.value = String(emp.travelSpeeds.SWITCH);
  slider.style.width  = '100%';
  slider.style.margin = '0';

const num = document.createElement('input');
num.type  = 'number';
num.min   = '0';
num.removeAttribute('max');    // ← allow any high value
num.step  = '0.1';
num.value = Number(emp.travelSpeeds.SWITCH).toFixed(1);

// Slider: capped 0..10
function applySwitchFromSlider(v) {
  let val = Math.max(0, Math.min(10, Math.round(parseFloat(v || 0) * 10) / 10));
  emp.travelSpeeds.SWITCH = val;
  slider.value = String(val);
  num.value    = val.toFixed(1);

emp._costMapDirty = true;
window.invalidateTradeRoutes?.();

  window.drawCurrent?.();
}

// Number box: no upper cap
function applySwitchFromNumber(v) {
  let raw = parseFloat(v || 0);
  let val = Math.max(0, Math.round(raw * 10) / 10); // no upper bound
  emp.travelSpeeds.SWITCH = val;
  slider.value = String(Math.min(10, val));         // slider parks at top if val > 10
  num.value    = val.toFixed(1);

emp._costMapDirty = true;
window.invalidateTradeRoutes?.();

  window.drawCurrent?.();
}

slider.addEventListener('input', () => applySwitchFromSlider(slider.value));
num.addEventListener('change',   () => applySwitchFromNumber(num.value));

  row.append(name, slider, num);
  speedDiv.appendChild(row);

  // keep references like other sliders
  emp._speedSliders['SWITCH'] = slider;
  emp._speedValues['SWITCH']  = num;
}

  // — Place Capital —
  const placeBtn = panel.querySelector('.place-capital-btn');
  placeBtn.addEventListener('click', () => {
    window.currentRouteEmpire  = null;
    window.currentRouteTarget  = null;
    window.pendingRouteEmpire  = null;
    document.querySelectorAll('.route-btn')
      .forEach(b => b.textContent = 'Find Route');

    window.currentMode   = 'placeCapital';
    window.currentEmpire = emp;
    // alert(`Click on the map to place the capital for '${emp.name}'`);
  });


  emp._capitalDisplay = panel.querySelector('.capital-display');
  emp._sizeDisplay = panel.querySelector('.size-display');

  emp._valueDisplay = panel.querySelector('.value-display');     // NEW
if (emp._valueDisplay) {
  // If main.js has already computed totals, show it immediately; else 0 until next sim pass
  emp._valueDisplay.textContent = `Land value: ${emp._value ?? 0}`;
}

  // — Heatmap Toggle —
  const heatBtn = panel.querySelector('.heatmap-btn');
  let heatOn = false;
  heatBtn.addEventListener('click', async () => {
    heatOn = !heatOn;

    window.currentHeatEmpire = heatOn ? emp : null;
    window._heatOverlayDirty = true;
    heatBtn.textContent = heatOn ? 'Hide Heatmap' : 'Show Heatmap';

    if (heatOn && typeof window.recomputeCostMapsOnly === 'function') {
      // Ensure we have cost maps, then redraw
      await window.recomputeCostMapsOnly();
      window._heatOverlayDirty = true;
    }

    // Use requestDraw to trigger full render loop (includes heatmap overlay)
    if (typeof window.requestDraw === 'function') {
      window.requestDraw();
    } else {
      window._drawDirty = true;
    }
  });

  // — Route-Finding Toggle —
  const routeBtn = panel.querySelector('.route-btn');
  let routeOn = false;
  routeBtn.addEventListener('click', () => {
    if (!routeOn) {
      window.currentMode           = 'findRoute';
      window.pendingRouteEmpire   = emp;
      routeBtn.textContent         = 'Cancel Route';
    } else {
      window.currentMode           = null;
      window.pendingRouteEmpire   = null;
      window.currentRouteEmpire    = null;
      window.currentRouteTarget    = null;
      routeBtn.textContent         = 'Find Route';
      // Trigger full redraw
      if (typeof window.requestDraw === 'function') {
        window.requestDraw();
      } else {
        window._drawDirty = true;
      }
    }
    routeOn = !routeOn;
  });

  // — Remove Empire Button —
  const remBtn = panel.querySelector('.remove-btn');
  remBtn.addEventListener('click', () => {
    EmpireManager.removeEmpire(emp.id);
    container.removeChild(panel);
    window.drawCurrent();
  });
}

// -------------------------------------------------

// Open empire panel: switch to Empires tab and scroll to the panel
window.openEmpirePanel = function(empireId) {
  // 1. Switch to the Empires tab
  const tabBtn = document.getElementById('tab-empires');
  if (tabBtn) {
    tabBtn.click();
  }

  // 2. Find and scroll to the empire panel (with delay for tab switch)
  setTimeout(() => {
    const panel = document.getElementById(`empire-panel-${empireId}`);
    if (!panel) return;

    // Open the details if collapsed
    panel.open = true;

    // Get the sidebar container for scrolling
    const sidebar = document.getElementById('controls');
    if (sidebar) {
      // Calculate position relative to sidebar
      const panelRect = panel.getBoundingClientRect();
      const sidebarRect = sidebar.getBoundingClientRect();
      const scrollTarget = sidebar.scrollTop + (panelRect.top - sidebarRect.top) - 100;

      sidebar.scrollTo({ top: scrollTarget, behavior: 'smooth' });
    }

    // Briefly highlight the panel
    panel.style.transition = 'box-shadow 0.3s ease, outline 0.3s ease';
    panel.style.boxShadow = '0 0 0 3px var(--accent), 0 0 20px rgba(96,165,250,0.4)';
    panel.style.outline = '2px solid var(--accent)';
    setTimeout(() => {
      panel.style.boxShadow = '';
      panel.style.outline = '';
    }, 2000);
  }, 150);
};


function initEmpireUI() {
  window.pendingRouteEmpire = null;

  const resetBtn = document.getElementById('reset-empires-btn');
  if (resetBtn) {
    resetBtn.addEventListener('click', () => EmpireManager.resetAll());
  }
}

EmpireManager.createEmpirePanel = createEmpirePanel;
window.createEmpirePanel = createEmpirePanel;
window.EmpireManager = EmpireManager;
window.initEmpireUI  = initEmpireUI;
