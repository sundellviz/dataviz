// js/empires.js

class Empire {
  constructor(id, name, color) {
    this.id           = id;
    this.name         = name || `Empire ${id}`;
    this.color        = color || '#ff000080';
    this.capital      = null;      // { x, y }
    this.travelSpeeds = {
      PLAIN:    2.0,
      DESERT:   4.0,
      WATER:    1.0,
      MOUNTAIN: 6.0,
      FOREST:   3.0,
      RIVER:    1.5,
      ICE:      7.0,
    };
    this.size       = 50;
    this.territory  = new Set();   // Set<number> of flat indices
    this.costMap    = [];
    this.parentMap  = [];
  }
}

//// NEW SECTION

// Build flat ownerId array from current territories
function buildOwnerIdFlat(rows, cols, empires) {
  const N = rows * cols;
  const owner = new Int32Array(N); // 0 = neutral/water
  for (const e of empires) {
    if (!e.territory) continue;
    for (const idx of e.territory) owner[idx] = e.id;
  }
  return owner;
}



// Compute per-cell depth for the owner of each cell (0 at border, higher inward).
// We do a per-empire 4-neighbor BFS seeded from "outside" (non-owner cells at 0).
function computeOwnerDepthFlat(rows, cols, ownerIdFlat) {
  const N = rows * cols;
  const depth = new Float32Array(N); // default 0 (neutral/water/unowned)

  const dirs4 = [ [1,0], [-1,0], [0,1], [0,-1] ];

  // Get unique empire ids present
  const ids = new Set(ownerIdFlat);
  ids.delete(0);

  for (const empId of ids) {
    // dist only matters inside empId's territory
    const dist = new Int32Array(N);
    dist.fill(1e9);

    // queue seeded by all "non-owner" cells (distance 0)
    const q = new Int32Array(N);
    let head = 0, tail = 0;

    // Seed: every cell that is NOT owned by empId
    for (let i = 0; i < N; i++) {
      if (ownerIdFlat[i] !== empId) {
        dist[i] = 0;
        q[tail++] = i;
      }
    }

    // 4-neighbor BFS into empId cells only
    while (head < tail) {
      const i = q[head++];
      const x = i % cols, y = (i / cols) | 0;
      const d = dist[i] + 1;

      for (const [dx, dy] of dirs4) {
        const nx = x + dx, ny = y + dy;
        if (nx < 0 || nx >= cols || ny < 0 || ny >= rows) continue;
        const j = ny * cols + nx;
        if (ownerIdFlat[j] !== empId) continue;  // only flow inside this empire
        if (d < dist[j]) {
          dist[j] = d;
          q[tail++] = j;
        }
      }
    }

    // Convert to "depth inside": border cells become 0 (dist==1 → 0)
    for (let i = 0; i < N; i++) {
      if (ownerIdFlat[i] === empId) {
        const inner = Math.max(0, dist[i] - 1);
        // (No need to max with previous; each cell has exactly one owner here)
        depth[i] = inner;
      }
    }
  }

  return depth;
}


/// END OF NEW SECTION

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



const EmpireManager = {
  empires: [],
  nextId: 1,
  nextColorIdx: 0,

   // async now returns after *all* worker jobs finish
   // async now returns after *all* worker jobs finish
async updateAllCostMaps(grid) {
  const rows = grid.rows, cols = grid.cols;

  // 1) Build owner arrays from last ring’s territory
  const ownerIdFlat    = buildOwnerIdFlat(rows, cols, EmpireManager.empires);

  // 2) Kick off one worker job per empire with shared owner arrays
  const jobs = EmpireManager.empires.map(emp =>
    computeCostMapOffload(emp, grid, ownerIdFlat, {
      penaltyScale:  (window.penaltyScale ?? 1.0),
      penaltyGamma:  (window.penaltyGamma ?? 1.0)
    }).then(({ costMap, parentMap }) => {
      emp.costMap   = costMap;
      emp.parentMap = parentMap;
    })
  );

  // 3) Wait for all
  await Promise.all(jobs);
},

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
  return e;
},

  removeEmpire(id) {
    this.empires = this.empires.filter(e => e.id !== id);
  }
};


function makeHostileSetFor(empire, allEmpires) {
  const set = new Set();
  for (const other of allEmpires) {
    if (other === empire || !other.territory) continue;
    for (const idx of other.territory) set.add(idx);
  }
  return set;
}

window.makeHostileSetFor = makeHostileSetFor;


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
        padding:4px; background:${emp.color}; border-radius:4px;
        cursor:pointer;">
      <span class="empire-name">${emp.name}</span>
      <input type="color" class="empire-color" value="${emp.color.slice(0,7)}" title="Color"/>
    </summary>
          <div class="meta-row" style="margin:6px 0; display:flex; gap:10px; align-items:center; flex-wrap:wrap;">
  <div class="capital-display">Capital: (–,–)</div>
  <div class="size-display">Size: 0</div>
</div>


    <div class="empire-controls" style="padding:6px;">
      <div class="size-row">
        <label class="size-label">
          Target size:
          <input
            type="number"
            class="size-input"
            min="1"
            max="30000"
            value="${emp.size}"
          />
        </label>
        <input
          type="range"
          class="size-slider"
          min="1"
          max="30000"
          value="${emp.size}"
        />
      </div>
      <br/>

      Travel costs:
      <div class="speed-sliders"></div>
      <div class="panel-actions"
     style="display:grid; grid-template-columns: 1fr 1fr; gap:8px; margin-top:8px;">
  <button class="place-capital-btn">Place Capital</button>
  <button class="heatmap-btn">Show Heatmap</button>
  <button class="route-btn">Find Route</button>
  <button class="remove-btn" style="background:#e11d48;">Remove Empire</button>
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
    window.simulateAndDraw(); window.drawCurrent();
  });

  // — Size Slider + Number Input —
  const sizeSlider = panel.querySelector('.size-slider');
  const sizeInput  = panel.querySelector('.size-input');
  // const sizeValue  = panel.querySelector('.size-value');
  // keep refs if you need them later
  emp._sizeSlider = sizeSlider;
  emp._sizeInput  = sizeInput;
  // emp._sizeValue  = sizeValue;

  // when the slider moves, sync the number box and the empire size
  sizeSlider.addEventListener('input', () => {
    const v = Math.round(Number(sizeSlider.value));
    emp.size          = v;
    sizeInput.value   = v;
    // sizeValue.textContent = v;
    simulateAndDraw(); drawCurrent();
  });

  // when the user types or arrows in the number field, clamp & sync slider
  sizeInput.addEventListener('change', () => {
    let v = Number(sizeInput.value);
    // clamp between min/max
    v = Math.max(Number(sizeInput.min), Math.min(Number(sizeInput.max), v));
    v = Math.round(v);
    emp.size          = v;
    sizeSlider.value  = v;
    // sizeValue.textContent = v;
    sizeInput.value   = v;
    simulateAndDraw(); drawCurrent();
  });

// — Travel Speed Sliders —
const speedDiv = panel.querySelector('.speed-sliders');
speedDiv.style.display = 'grid';
speedDiv.style.rowGap  = '2px';
speedDiv.style.margin  = '2px 0';

emp._speedSliders = {};
emp._speedValues  = {};
for (let t in emp.travelSpeeds) {
  const lbl = document.createElement('label');
  lbl.textContent = `${t}: `;

  const inp = document.createElement('input');
  inp.type = 'range'; inp.min = 0.1; inp.max = 10; inp.step = 0.1;
  inp.value = emp.travelSpeeds[t];

  const span = document.createElement('span');
  span.textContent = emp.travelSpeeds[t].toFixed(1);

  // Compact grid for each speed row
  lbl.style.display = 'grid';
  lbl.style.gridTemplateColumns = '90px 1fr 44px'; // label | slider | value
  lbl.style.alignItems = 'center';
  lbl.style.columnGap = '8px';
  lbl.style.margin = '4px 0';

  inp.style.width = '100%';
  inp.style.margin = '0px';          // remove extra height from default margins
inp.style.height = '10px';       // optional: thinner track on WebKit/Blink

  span.style.textAlign = 'right';
  span.style.minWidth = '40px';
span.style.fontVariantNumeric = 'tabular-nums';

  inp.addEventListener('input', () => {
    emp.travelSpeeds[t] = +inp.value;
    span.textContent = Number(inp.value).toFixed(1);
    window.simulateAndDraw(); window.drawCurrent();
  });

  lbl.append(inp, span);
  speedDiv.append(lbl);
  emp._speedSliders[t] = inp;
  emp._speedValues[t]  = span;
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
    //alert(`Click on the map to place the capital for '${emp.name}'`);
  });


  emp._capitalDisplay = panel.querySelector('.capital-display');
  emp._sizeDisplay = panel.querySelector('.size-display');

  // — Heatmap Toggle —
  const heatBtn = panel.querySelector('.heatmap-btn');
  let heatOn = false;
  heatBtn.addEventListener('click', () => {
    heatOn = !heatOn;
    window.currentHeatEmpire = heatOn ? emp : null;
    heatBtn.textContent      = heatOn ? 'Hide Heatmap' : 'Show Heatmap';
    window.drawCurrent();
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
      window.currentRouteEmpire    = null;
      window.currentRouteTarget    = null;
      window.drawCurrent();
      routeBtn.textContent         = 'Find Route';
    }
    routeOn = !routeOn;
  });

  // — Remove Empire Button —
  const remBtn = panel.querySelector('.remove-btn');
  remBtn.addEventListener('click', () => {
    EmpireManager.removeEmpire(emp.id);
    container.removeChild(panel);
    window.simulateAndDraw(); window.drawCurrent();
  });
}

// -------------------------------------------------





function initEmpireUI() {
  const controls = document.getElementById('controls');
  const section  = document.createElement('div');
  section.id     = 'empire-section';
  controls.append(section);

  const panels = document.createElement('div');
  panels.id = 'empire-panels';
  section.append(panels);

const addBtn = document.createElement('button');
  addBtn.id      = 'add-empire-btn';
  addBtn.textContent = 'Add Empire';
  section.append(addBtn);

  addBtn.addEventListener('click', () => {
    // 1) create new empire data
    const emp = EmpireManager.addEmpire();
    // 2) build its full panel UI
    createEmpirePanel(emp);
    // 3) switch into capital‐placement mode
    window.currentMode   = 'placeCapital';
    window.currentEmpire = emp;
    //alert(`Click on the map to place the capital for '${emp.name}'`);
  });

  let placingEmpire = null;
  window.pendingRouteEmpire = null;

  // Canvas click: handle route‑finding or capital‑placement
  const canvas = document.getElementById('mapCanvas');
  canvas.addEventListener('click', e => {
    const rect   = canvas.getBoundingClientRect();
    const scaleX = canvas.width  / rect.width;
    const scaleY = canvas.height / rect.height;
    const cx     = (e.clientX - rect.left) * scaleX;
    const cy     = (e.clientY - rect.top ) * scaleY;
    const cellW = canvas.width  / grid.cols;
const cellH = canvas.height / grid.rows;
const x     = Math.floor((e.clientX - rect.left) * (canvas.width/rect.width)  / cellW);
const y     = Math.floor((e.clientY - rect.top ) * (canvas.height/rect.height) / cellH);

    // Route‑finding mode?
    if (window.currentMode === 'findRoute' && window.pendingRouteEmpire) {


    window.currentRouteEmpire = window.pendingRouteEmpire;
   window.currentRouteTarget = { x, y };
   window.pendingRouteEmpire = null;
   window.currentMode = null;

      return;
    }

    // Capital-placement mode?
   if (window.currentMode === 'placeCapital' && window.currentEmpire) {
  const emp = window.currentEmpire;
  emp.capital = { x, y };

  // ← use the element reference stored on the empire
  emp._capitalDisplay.textContent = `Capital: (${x},${y})`;

  window.currentEmpire = null;
  window.currentMode   = null;
  window.simulateAndDraw();
  window.drawCurrent();
  return;
}
  });

}

EmpireManager.createEmpirePanel = createEmpirePanel;
window.createEmpirePanel = createEmpirePanel;
window.EmpireManager = EmpireManager;
window.initEmpireUI  = initEmpireUI;
