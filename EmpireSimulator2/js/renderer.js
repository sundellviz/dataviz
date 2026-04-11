// js/renderer.js

// Pre-computed viridis color lookup table (256 entries)
const _viridisLUT = (function() {
  const viridisColors = [
    [68, 1, 84], [71, 44, 122], [59, 81, 139], [44, 113, 142],
    [33, 144, 141], [39, 173, 129], [92, 200, 99], [170, 220, 50],
    [253, 231, 37]
  ];
  const LUT_SIZE = 256;
  const lut = new Array(LUT_SIZE);

  for (let j = 0; j < LUT_SIZE; j++) {
    const t = j / (LUT_SIZE - 1);
    const i = t * (viridisColors.length - 1);
    const i0 = Math.floor(i);
    const i1 = Math.min(i0 + 1, viridisColors.length - 1);
    const f = i - i0;

    const [r0, g0, b0] = viridisColors[i0];
    const [r1, g1, b1] = viridisColors[i1];
    const r = Math.round(r0 + (r1 - r0) * f);
    const g = Math.round(g0 + (g1 - g0) * f);
    const b = Math.round(b0 + (b1 - b0) * f);

    lut[j] = `rgb(${r},${g},${b})`;
  }
  return lut;
})();

function viridis(t) {
  // Fast lookup using pre-computed table
  const idx = Math.max(0, Math.min(255, Math.round(t * 255)));
  return _viridisLUT[idx];
}


/**
 * Linear interpolate between two colors (hex), t in [0,1].
 */
function lerpColor(a, b, t) {
  const ah = parseInt(a.slice(1), 16),
        bh = parseInt(b.slice(1), 16);
  const ar = ah >> 16, ag = (ah >> 8) & 0xff, ab = ah & 0xff;
  const br = bh >> 16, bg = (bh >> 8) & 0xff, bb = bh & 0xff;
  const rr = Math.round(ar + (br - ar) * t);
  const rg = Math.round(ag + (bg - ag) * t);
  const rb = Math.round(ab + (bb - ab) * t);
  return '#' + ((1 << 24) + (rr << 16) + (rg << 8) + rb)
                .toString(16).slice(1);
}


// Cached raster for WATER/OCEAN shading
let waterShadeCanvas = null;

// ═══════════════════════════════════════════════════════════════════════════
// EDGE CACHE - Precomputed cell boundaries for fast lookup
// ═══════════════════════════════════════════════════════════════════════════
const _edgeCache = {
  canvasW: 0,
  canvasH: 0,
  cols: 0,
  rows: 0,
  xEdge: null,  // Int32Array
  yEdge: null   // Int32Array
};

/**
 * Get cached edge arrays for cell boundaries.
 * Recomputes only when dimensions change.
 * @returns {{ xEdge: Int32Array, yEdge: Int32Array }}
 */
function getEdgeArrays(canvasWidth, canvasHeight, cols, rows) {
  const c = _edgeCache;

  // Check if cache is valid
  if (c.xEdge && c.yEdge &&
      c.canvasW === canvasWidth && c.canvasH === canvasHeight &&
      c.cols === cols && c.rows === rows) {
    return { xEdge: c.xEdge, yEdge: c.yEdge };
  }

  // Recompute edges
  const xEdge = new Int32Array(cols + 1);
  const yEdge = new Int32Array(rows + 1);

  for (let i = 0; i <= cols; i++) {
    xEdge[i] = Math.round((i * canvasWidth) / cols);
  }
  for (let j = 0; j <= rows; j++) {
    yEdge[j] = Math.round((j * canvasHeight) / rows);
  }

  // Update cache
  c.canvasW = canvasWidth;
  c.canvasH = canvasHeight;
  c.cols = cols;
  c.rows = rows;
  c.xEdge = xEdge;
  c.yEdge = yEdge;

  return { xEdge, yEdge };
}

// Expose for use in main.js

// ═══════════════════════════════════════════════════════════════════════════
// OWNER ID CACHE - For heatmap border rendering
// ═══════════════════════════════════════════════════════════════════════════
const _ownerCache = {
  version: -1,
  ownerId: null
};

function getCachedOwnerId(N) {
  const ver = window._ownerVersion || 0;
  if (_ownerCache.ownerId && _ownerCache.ownerId.length === N && _ownerCache.version === ver) {
    return _ownerCache.ownerId;
  }

  // Rebuild
  const ownerId = new Int32Array(N);
  for (const e of EmpireManager.empires) {
    if (!e.territory) continue;
    for (const idx of e.territory) ownerId[idx] = e.id;
  }

  _ownerCache.ownerId = ownerId;
  _ownerCache.version = ver;
  return ownerId;
}

// ═══════════════════════════════════════════════════════════════════════════

function isWetTerrain(t) {
  return t === 'WATER' || t === 'OCEAN';
}

/**
 * Build a raster (canvas) of the wet shading at the desired pixel size.
 * Wet = WATER + OCEAN. Same gradient, WATER will get a light overlay later.
 */
function computeWaterShadingCanvas(grid, targetW, targetH) {
  const H = grid.rows, W = grid.cols;
  const baseColor = '#2d7efc';   // shallow (near land)
  const deepColor = '#154ca3';   // deep (far from land)

  const N = W * H;
  const idx = (x, y) => y * W + x;

  // Use -1 as "unvisited". This guarantees each cell enqueued at most once.
  const dist = new Int16Array(N);
  dist.fill(-1);

  const qx = new Int32Array(N);
  const qy = new Int32Array(N);
  let qh = 0, qt = 0;

  // Seed BFS with all NON-wet cells at distance 0
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      if (!isWetTerrain(grid.cells[y][x].terrain)) {
  dist[idx(x, y)] = 0;
  qx[qt] = x; qy[qt] = y; qt++;
}
    }
  }

// 8-neighbor with integer weights (orth=2, diag=3) for rounder gradients
const dirs = [
  [ 1,  0, 2], [-1,  0, 2], [ 0,  1, 2], [ 0, -1, 2],
  [ 1,  1, 3], [ 1, -1, 3], [-1,  1, 3], [-1, -1, 3]
];

// Weighted multi-source expansion (Dial’s algorithm with 4 buckets).
// Prevents re-enqueue explosions while supporting weighted diagonals.
const MAXD = 300; // 100 * 3, max possible distance in our weight units

const buckets = [[], [], [], []];
let cur = 0;

// Seed bucket 0 with all non-wet cells that we set to dist=0 earlier.
for (let y = 0; y < H; y++) {
  for (let x = 0; x < W; x++) {
    if (dist[idx(x, y)] === 0) buckets[0].push(idx(x, y));
  }
}

const popNext = () => {
  while (cur <= MAXD) {
    const b = buckets[cur & 3];
    if (b.length) return b.pop();
    cur++;
  }
  return -1;
};

for (;;) {
  const i = popNext();
  if (i < 0) break;

  // stale entry check
  if (dist[i] !== cur) continue;

  const x = i % W;
  const y = (i / W) | 0;

  for (const [dx, dy, w] of dirs) {
    const nx = x + dx, ny = y + dy;
    if (nx < 0 || ny < 0 || nx >= W || ny >= H) continue;

    const row = grid.cells[ny];
    if (!row) continue;
    const cell = row[nx];
    if (!cell) continue;
    if (!isWetTerrain(cell.terrain)) continue;

    const ni = idx(nx, ny);
    const nd = cur + w;
    if (nd > MAXD) continue;

    const prev = dist[ni];
    if (prev !== -1 && prev <= nd) continue; // no improvement

    dist[ni] = nd;
    buckets[nd & 3].push(ni);
  }
}

  // Find max distance over wet cells (clamped)
  let actualMax = 0;
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      if (isWetTerrain(grid.cells[y][x].terrain)) {
        const d = dist[idx(x, y)];
if (d >= 0) actualMax = Math.max(actualMax, d);
      }
    }
  }
  const maxDist = Math.min(actualMax, MAXD) || 1;

  // Rasterize into a canvas
  const c = document.createElement('canvas');
  c.width  = Math.max(1, Math.round(targetW));
  c.height = Math.max(1, Math.round(targetH));
  const ctx = c.getContext('2d', { willReadFrequently: false });
  ctx.imageSmoothingEnabled = false;

  // Integer pixel edges so every cell lines up perfectly
  const xEdge = new Int32Array(W + 1);
  const yEdge = new Int32Array(H + 1);
  for (let i = 0; i <= W; i++) xEdge[i] = Math.round((i * c.width)  / W);
  for (let j = 0; j <= H; j++) yEdge[j] = Math.round((j * c.height) / H);

  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      if (!isWetTerrain(grid.cells[y][x].terrain)) continue;

      let t = dist[idx(x, y)] / maxDist;
      t = Math.min(1, Math.max(0, Math.pow(t, 0.6))); // same bias as before

      const col = lerpColor(baseColor, deepColor, t);

      const x0 = xEdge[x], x1 = xEdge[x + 1];
      const y0 = yEdge[y], y1 = yEdge[y + 1];
      const w = x1 - x0, h = y1 - y0;
      if (w > 0 && h > 0) {
        ctx.fillStyle = col;
        ctx.fillRect(x0, y0, w, h);
      }
    }
  }

  return c;
}

function precomputeWaterShading(grid, pixelW, pixelH) {
  waterShadeCanvas = computeWaterShadingCanvas(
    grid,
    pixelW ?? (window.canvas?.width  ?? grid.cols),
    pixelH ?? (window.canvas?.height ?? grid.rows)
  );
}
window.precomputeWaterShading = precomputeWaterShading;



// ───────────────── Mountain depth tint (precomputed) ────────────────
let MOUNTAIN_DEPTH = null;   // Int16Array (rows*cols) or null
let MOUNTAIN_MAXD  = 0;

function computeMountainDepth(grid) {
  const W = grid.cols, H = grid.rows, N = W * H;
  const idx = (x, y) => y * W + x;
  const inBounds = (x, y) => (x >= 0 && y >= 0 && x < W && y < H);
  const isMountain = (x, y) => inBounds(x,y) && grid.cells[y][x].terrain === 'MOUNTAIN';

  const dist = new Int16Array(N); dist.fill(-1);
  const qx = new Int32Array(N), qy = new Int32Array(N); let qh = 0, qt = 0;

  // seed boundary cells (mountain cells touching any non-mountain or edge)
  for (let y = 0; y < H; y++) for (let x = 0; x < W; x++) {
    if (!isMountain(x, y)) continue;
    const boundary =
      !isMountain(x-1, y) || !isMountain(x+1, y) ||
      !isMountain(x, y-1) || !isMountain(x, y+1);
    if (boundary) { dist[idx(x,y)] = 0; qx[qt] = x; qy[qt] = y; qt++; }
  }

  // BFS inside mountain regions (4-neighbors)
  const dirs = [[1,0],[-1,0],[0,1],[0,-1]];
  while (qh < qt) {
    const x = qx[qh], y = qy[qh]; qh++;
    const d0 = dist[idx(x,y)];
    for (const [dx,dy] of dirs) {
      const nx = x + dx, ny = y + dy;
      if (!isMountain(nx, ny)) continue;
      const i = idx(nx, ny);
      if (dist[i] !== -1) continue;
      dist[i] = d0 + 1;
      qx[qt] = nx; qy[qt] = ny; qt++;
    }
  }

  MOUNTAIN_DEPTH = dist;
  MOUNTAIN_MAXD = 0;
  for (let i = 0; i < N; i++) if (dist[i] > MOUNTAIN_MAXD) MOUNTAIN_MAXD = dist[i];
}

// ease curve for depth → [0..1]
function depthFactor(d){
  const t = Math.min(1, d / 10);         // full effect ~6 cells inward
  return 1 - Math.pow(1 - t, 2);        // ease-out
}

// Return tinted mountain color given the base (already variant-blended)
function mountainTintColor(baseHex, x, y, cols){
  if (!MOUNTAIN_DEPTH) return baseHex;
  const i = y * cols + x;
  const d = MOUNTAIN_DEPTH[i];
  if (d <= 0) return baseHex;

  const highlight = '#d8dee9';          // subtle “snow” highlight
  const f = depthFactor(d) * 0.5;       // 0..0.6 strength
  return lerpColor(baseHex, highlight, f);
}


// ===== Value View support =====
window.renderMode = 'terrain'; // default

/**
 * Draws the land-value layer using viridis colors (0..61 -> 0..1).
 * Optionally overlays the value character when zoomed in.
 */
function drawValueGrid(ctx, grid, cellSize, showGrid = false, drawGlyphs = true) {
  const rows = window.grid.rows, cols = window.grid.cols;
  if (!grid.valueLayer) {
    // no layer yet → fall back to terrain
    return drawGrid(ctx, grid, cellSize, showGrid);
  }

  // Match your crisp rounding in drawGrid()
  ctx.imageSmoothingEnabled = false;
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);

 // Fill water background to avoid seam artifacts in value view too
ctx.fillStyle = '#1E5AA8';
ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);

  if (waterShadeCanvas) {
  ctx.drawImage(waterShadeCanvas, 0, 0, ctx.canvas.width, ctx.canvas.height);
}

  // Use cached edge arrays for cell boundaries
  const { xEdge, yEdge } = getEdgeArrays(ctx.canvas.width, ctx.canvas.height, cols, rows);

  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      const v  = grid.valueLayer?.[y]?.[x] ?? 0;
      const t  = Math.max(0, Math.min(1, v / 61));
      const x0 = xEdge[x], x1 = xEdge[x + 1];
      const y0 = yEdge[y], y1 = yEdge[y + 1];
      const w  = x1 - x0, h = y1 - y0;

      ctx.fillStyle = viridis(t);
      ctx.fillRect(x0, y0, w, h);

      // Optional glyph overlay when cells are big enough
      if (drawGlyphs && cellSize >= 14) {
        const ch = (typeof valToChar === 'function') ? valToChar(v) : String(v);
        ctx.fillStyle = 'rgba(0,0,0,0.75)';
        ctx.font = `${Math.floor(cellSize * 0.7)}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(ch, x0 + w / 2, y0 + h / 2);
      }
    }
  }

  if (showGrid) {
    ctx.strokeStyle = '#aaa';
    ctx.beginPath();
    for (let i = 0; i <= cols; i++) {
      const px = xEdge[i] + 0.5;
      ctx.moveTo(px, 0);  ctx.lineTo(px, ctx.canvas.height);
    }
    for (let j = 0; j <= rows; j++) {
      const py = yEdge[j] + 0.5;
      ctx.moveTo(0, py);  ctx.lineTo(ctx.canvas.width, py);
    }
    ctx.stroke();
  }
}

// expose for main.js
window.drawValueGrid = drawValueGrid;

// Draw crisp, readable labels (white fill with black outline), size-adaptive
function drawOutlinedLabel(ctx, text, x, y, fontPx, align = 'left') {
  ctx.font = `${fontPx}px sans-serif`;
  ctx.textBaseline = 'middle';
  ctx.textAlign = align;

  // Outline thickness scales with font size
  ctx.lineJoin = 'round';
  ctx.miterLimit = 2;
  ctx.lineWidth = Math.max(2, fontPx * 0.15);

  ctx.strokeStyle = 'black';
  ctx.strokeText(text, x, y);

  ctx.fillStyle = 'white';
  ctx.fillText(text, x, y);
}

// make available to other files loaded later
window.drawOutlinedLabel = drawOutlinedLabel;


/**
 * Draw the grid; grid lines off by default.
 * Applies on‑demand water shading from the cached map.
 */
function drawGrid(ctx, grid, cellSize, showGrid = false) {
  const Wpx = grid.cols * cellSize,
        Hpx = grid.rows * cellSize;

  ctx.imageSmoothingEnabled = false;
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);

  // Fill water background so any micro gaps show water (not black)
ctx.fillStyle = '#2d7efc';   // same baseColor used in water shading
ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);


    // Use cached edge arrays for cell boundaries
  const cols = grid.cols, rows = grid.rows;
  const { xEdge, yEdge } = getEdgeArrays(ctx.canvas.width, ctx.canvas.height, cols, rows);

// Draw cached wet shading first (scaled to the target surface)
if (waterShadeCanvas) {
  ctx.drawImage(waterShadeCanvas, 0, 0, ctx.canvas.width, ctx.canvas.height);
}

  const VARIANT_BLEND = 0.1;  // subtle mix for other terrains

  for (let y = 0; y < grid.rows; y++) {
    const y0 = yEdge[y], y1 = yEdge[y + 1];
    const h = y1 - y0;
    if (h <= 0) continue;

    const rowCells = grid.cells[y];
    const varRow = window.variantGrid?.[y];

    for (let x = 0; x < grid.cols; x++) {
      const cell = rowCells[x];
      const x0 = xEdge[x], x1 = xEdge[x + 1];
      const w = x1 - x0;
      if (w <= 0) continue;

      // Wet terrains: let the cached shading show through.
      // WATER gets an extra light overlay to distinguish it from OCEAN.
      if ((cell.terrain === 'WATER' || cell.terrain === 'OCEAN') && waterShadeCanvas) {
        if (cell.terrain === 'WATER') {
          ctx.fillStyle = 'rgba(255,255,255,0.10)';
          ctx.fillRect(x0, y0, w, h);
        }
        continue;
      }

      // Otherwise, normal terrain rendering
      const base = (TERRAIN[cell.terrain] && TERRAIN[cell.terrain].color) ? TERRAIN[cell.terrain].color : '#000000';
      const variants = TERRAIN_VARIANTS[cell.terrain] || [ base ];
      const idx = varRow?.[x] ?? 0;
      const varCol = variants[idx % variants.length];
      let color = lerpColor(base, varCol, VARIANT_BLEND);

      if (cell.terrain === 'MOUNTAIN' && MOUNTAIN_DEPTH) {
        color = mountainTintColor(color, x, y, grid.cols);
      }

      ctx.fillStyle = color;
      ctx.fillRect(x0, y0, w, h);
    }
  }

  // optional grid lines
  if (showGrid) {
    ctx.strokeStyle = '#aaa';
    ctx.beginPath();
    for (let i = 0; i <= grid.cols; i++) {
      const px = i * cellSize + 0.5;
      ctx.moveTo(px, 0);  ctx.lineTo(px, Hpx);
    }
    for (let j = 0; j <= grid.rows; j++) {
      const py = j * cellSize + 0.5;
      ctx.moveTo(0, py);  ctx.lineTo(Wpx, py);
    }
    ctx.stroke();
  }
}

/**
 * Draws a semi-transparent cost heatmap overlay for one empire.
 * Caches the result to an offscreen canvas; only rebuilds when
 * ownership, cost maps, or canvas size change.
 */
// Heatmap cache state
let _heatCacheEmpId = -1;
let _heatCacheOwnerVer = -1;
let _heatCacheW = 0;
let _heatCacheH = 0;

function drawHeatmap(emp) {
  if (!emp) return;

  const canvas = document.getElementById('mapCanvas');
  const heatLayer = window._heatLayer;
  const heatCtx = window._heatCtx;
  if (!heatLayer || !heatCtx) return;

  // Ensure heat layer matches canvas size
  if (heatLayer.width !== canvas.width || heatLayer.height !== canvas.height) {
    heatLayer.width = canvas.width;
    heatLayer.height = canvas.height;
    window._heatOverlayDirty = true;
  }

  const ownerVer = window._ownerVersion || 0;

  // Check if cache is still valid
  const dirty = window._heatOverlayDirty ||
    _heatCacheEmpId !== emp.id ||
    _heatCacheOwnerVer !== ownerVer ||
    _heatCacheW !== canvas.width ||
    _heatCacheH !== canvas.height;

  if (!dirty) return; // cached layer is still valid

  // --- Rebuild the heatmap onto the offscreen canvas ---
  const rows = window.grid.rows, cols = window.grid.cols, N = rows * cols;
  const { xEdge, yEdge } = getEdgeArrays(canvas.width, canvas.height, cols, rows);
  const ownerId = getCachedOwnerId(N);

  const flat =
    (emp.costMapFlat instanceof Float32Array && emp.costMapFlat.length === N)
      ? emp.costMapFlat
      : null;

  const has2D =
    Array.isArray(emp.costMap) &&
    emp.costMap.length === rows &&
    Array.isArray(emp.costMap[0]) &&
    emp.costMap[0].length === cols;

  if (!flat && !has2D) {
    console.warn('drawHeatmap: no cost map for empire', emp.name);
    return;
  }

  const getCost = flat
    ? (x, y) => flat[y * cols + x]
    : (x, y) => { const row = emp.costMap[y]; return row ? row[x] : Infinity; };

  // Collect reachable cells for ranking
  const vals = [];
  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      const c = getCost(x, y);
      if (isFinite(c)) vals.push([c, y * cols + x]);
    }
  }

  if (vals.length === 0) return;

  // Rank (percentile): best (lowest cost) = 1.0, worst = 0.0
  vals.sort((a, b) => a[0] - b[0]);
  const rank = new Float32Array(N); rank.fill(-1);
  if (vals.length === 1) {
    rank[vals[0][1]] = 1;
  } else {
    const denom = vals.length - 1;
    for (let i = 0; i < vals.length; i++) rank[vals[i][1]] = 1 - (i / denom);
  }

  // Expose for tooltip
  window.__heatRank = { empId: emp.id, rank };
  window.currentHeatEmpire = emp;

  // Draw onto offscreen heat layer
  heatCtx.clearRect(0, 0, heatLayer.width, heatLayer.height);
  heatCtx.imageSmoothingEnabled = false;

  for (let y = 0; y < rows; y++) {
    const y0 = yEdge[y], y1 = yEdge[y + 1];
    const rh = y1 - y0;
    if (rh <= 0) continue;

    for (let x = 0; x < cols; x++) {
      const idx = y * cols + x;
      const t = rank[idx];
      if (t < 0) continue;

      const x0 = xEdge[x], x1 = xEdge[x + 1];
      const rw = x1 - x0;
      if (rw > 0) {
        heatCtx.fillStyle = viridis(t);
        heatCtx.fillRect(x0, y0, rw, rh);
      }
    }
  }

  // Empire borders in black
  const borderWidth =
    Math.max(1, Math.min(canvas.width / cols, canvas.height / rows) * 0.08);

  heatCtx.strokeStyle = '#000';
  heatCtx.lineWidth   = borderWidth;
  heatCtx.beginPath();

  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      const idx = y * cols + x;
      const id  = ownerId[idx];

      if (x < cols - 1) {
        const idR = ownerId[idx + 1];
        if (id !== idR) {
          const xBorder = xEdge[x + 1] + 0.5;
          heatCtx.moveTo(xBorder, yEdge[y]);
          heatCtx.lineTo(xBorder, yEdge[y + 1]);
        }
      }

      if (y < rows - 1) {
        const idD = ownerId[(y + 1) * cols + x];
        if (id !== idD) {
          const yBorder = yEdge[y + 1] + 0.5;
          heatCtx.moveTo(xEdge[x], yBorder);
          heatCtx.lineTo(xEdge[x + 1], yBorder);
        }
      }
    }
  }

  heatCtx.stroke();

  // Update cache keys
  _heatCacheEmpId = emp.id;
  _heatCacheOwnerVer = ownerVer;
  _heatCacheW = canvas.width;
  _heatCacheH = canvas.height;
  window._heatOverlayDirty = false;
}


/**
 * Backtracks along Dijkstra parent pointers and draws a red route.
 */
function drawRoute(emp, tx, ty, markerStep = (window.routeMarkerStep || 50)) {
  if (!emp.capital) return alert('Place the capital first.');

  const canvas = document.getElementById('mapCanvas');
  const ctx    = canvas.getContext('2d');
  const cols   = grid.cols, rows = grid.rows;
  const cellW  = canvas.width  / cols;
  const cellH  = canvas.height / rows;

  // --- cost-map presence (create if missing) ---
  const N = rows * cols;
  let flat =
    (emp.costMapFlat instanceof Float32Array && emp.costMapFlat.length === N)
      ? emp.costMapFlat
      : null;

  const has2D =
    Array.isArray(emp.costMap) &&
    emp.costMap.length === rows &&
    Array.isArray(emp.costMap[0]) &&
    emp.costMap[0].length === cols;

  if (!flat && !has2D) {
    // Kick off a cost-map build, then redraw when done.
    if (typeof window.recomputeCostMapsOnly === 'function') {
      (async () => { 
        await window.recomputeCostMapsOnly();
        window.drawCurrent?.();   // re-renders, which calls drawRoute again
      })();
    }
    console.warn('drawRoute: no cost map yet; building one…');
    return;
  }

  // Use whichever is available (flat preferred)
  const getCost = (x, y) => {
    if (flat) return flat[y * cols + x];
    const row = emp.costMap[y];
    return row ? row[x] : Infinity;
  };

  // Build path using either typed parentIdx or legacy parentMap
  const path = []; // [{x,y}]
  if (emp.parentIdx instanceof Int32Array) {
    const start = emp.capital.y * cols + emp.capital.x;
    let i = ty * cols + tx;
    const N = rows * cols;
    let guard = 0;
    while (i !== start && i >= 0 && guard++ < N) {
      const x = i % cols, y = (i / cols) | 0;
      path.push({x, y});
      const pi = emp.parentIdx[i];
      if (pi < 0 || pi === i) break;
      i = pi;
    }
    path.push({x: emp.capital.x, y: emp.capital.y});
  } else if (emp.parentMap) {
    let cx = tx, cy = ty;
    const N = rows * cols;
    let guard = 0;
    while (!(cx === emp.capital.x && cy === emp.capital.y) && guard++ < N) {
      path.push({x: cx, y: cy});
      const p = emp.parentMap[cy]?.[cx];
      if (!p) { alert('No route found.'); return; }
      cx = p.x; cy = p.y;
    }
    path.push({x: emp.capital.x, y: emp.capital.y});
  } else {
    return alert('Parent map missing—compute territory first.');
  }

  // If target is unreachable, bail
  const totalCost = getCost(tx, ty);
  if (!isFinite(totalCost)) { alert('No route found.'); return; }

  // Convert to forward order: capital -> target
  const fwd = path.slice().reverse();

  // Screen coords for each path node, and costs at nodes
  const pts   = fwd.map(({x,y}) => ({ px: x*cellW + cellW/2, py: y*cellH + cellH/2, x, y }));
  const costs = fwd.map(({x,y}) => getCost(x, y));

  // --- draw main polyline ---
  ctx.save();
  ctx.strokeStyle = 'red';
  ctx.lineWidth   = Math.max(2, Math.min(cellW, cellH) * 0.12);
  ctx.beginPath();
  for (let i = 0; i < pts.length; i++) {
    const {px, py} = pts[i];
    if (i === 0) ctx.moveTo(px, py);
    else ctx.lineTo(px, py);
  }
  ctx.stroke();

  // --- distance markers (ticks) every markerStep of cost ---
  // We step thresholds from markerStep up to totalCost
  const tickLen = Math.max(4, Math.min(cellW, cellH) * 3);
  ctx.lineWidth = Math.max(2, Math.min(cellW, cellH) * 0.10);

  let nextThresh = markerStep;
  let segStartCost = costs[0];
  for (let i = 1; i < pts.length && nextThresh <= totalCost + 1e-6; i++) {
    const c0 = costs[i-1], c1 = costs[i];
    if (!(c1 > c0)) continue; // guard (should always increase)
    const p0 = pts[i-1], p1 = pts[i];

    // There may be multiple thresholds inside one long step
    while (nextThresh <= c1 + 1e-6) {
      if (nextThresh > totalCost + 1e-6) break;
      if (nextThresh >= c0 - 1e-6) {
        const denom = (c1 - c0);
        const t = denom > 0 ? (nextThresh - c0) / denom : 0;
        // Interpolated position along segment
        const mx = p0.px + (p1.px - p0.px) * t;
        const my = p0.py + (p1.py - p0.py) * t;

        // Perpendicular vector (normalized)
        const vx = (p1.px - p0.px), vy = (p1.py - p0.py);
        const vlen = Math.hypot(vx, vy) || 1;
        const nx = -vy / vlen, ny = vx / vlen;

        // Draw tick
        ctx.beginPath();
        ctx.moveTo(mx - nx * (tickLen/2), my - ny * (tickLen/2));
        ctx.lineTo(mx + nx * (tickLen/2), my + ny * (tickLen/2));
        ctx.strokeStyle = 'red';
        ctx.stroke();
      }
      nextThresh += markerStep;
    }
    segStartCost = c1;
  }

// --- destination dot + total cost label ---
const end = pts[pts.length - 1];
const r = Math.max(3, Math.min(cellW, cellH) * 0.22);

// Red dot
ctx.beginPath();
ctx.fillStyle = 'red';
ctx.arc(end.px, end.py, r, 0, Math.PI*2);
ctx.fill();

// Label (uses shared outlined label helper)
const fontPx = Math.max(12, Math.min(cellW, cellH) * 0.8);
const label  = `Cost: ${Math.round(totalCost)}`;
const lx = end.px + r + 6; // a small gap to the right of the dot

  window.drawOutlinedLabel(ctx, label, lx, end.py, fontPx, 'left');


  ctx.restore();
}

// expose globally
window.drawGrid    = drawGrid;
window.drawHeatmap = drawHeatmap;
window.drawRoute   = drawRoute;
window.computeMountainDepth = computeMountainDepth;
