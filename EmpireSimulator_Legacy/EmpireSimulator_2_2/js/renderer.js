// js/renderer.js

function viridis(t) {
  // Clamp t to [0, 1]
  t = Math.min(1, Math.max(0, t));

  // Viridis color map (256 values), reduced to 16 steps to keep size small
  const viridisColors = [
    [68, 1, 84], [71, 44, 122], [59, 81, 139], [44, 113, 142],
    [33, 144, 141], [39, 173, 129], [92, 200, 99], [170, 220, 50],
    [253, 231, 37]
  ];

  const i = t * (viridisColors.length - 1);
  const i0 = Math.floor(i);
  const i1 = Math.min(i0 + 1, viridisColors.length - 1);
  const f = i - i0;

  const [r0, g0, b0] = viridisColors[i0];
  const [r1, g1, b1] = viridisColors[i1];
  const r = Math.round(r0 + (r1 - r0) * f);
  const g = Math.round(g0 + (g1 - g0) * f);
  const b = Math.round(b0 + (b1 - b0) * f);

  return `rgb(${r},${g},${b})`;
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
window.RenderMode = { TERRAIN: 'terrain', VALUE: 'value' };
window.renderMode = window.RenderMode.TERRAIN; // default

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
ctx.fillStyle = '#2d7efc';
ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);

  if (waterShadeCanvas) {
  ctx.drawImage(waterShadeCanvas, 0, 0, ctx.canvas.width, ctx.canvas.height);
}

  const Wpx = cols * cellSize, Hpx = rows * cellSize;

  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      const v  = grid.valueLayer?.[y]?.[x] ?? 0;
      const t  = Math.max(0, Math.min(1, v / 61));
      const x0 = Math.round(x * cellSize);
      const y0 = Math.round(y * cellSize);
      const w  = Math.round((x + 1) * cellSize) - x0;
      const h  = Math.round((y + 1) * cellSize) - y0;

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
      const px = i * cellSize + 0.5;
      ctx.moveTo(px, 0);  ctx.lineTo(px, Hpx);
    }
    for (let j = 0; j <= rows; j++) {
      const py = j * cellSize + 0.5;
      ctx.moveTo(0, py);  ctx.lineTo(Wpx, py);
    }
    ctx.stroke();
  }
}

// expose for main.js
window.drawValueGrid = drawValueGrid;



// Cached raster for water shading (same look, less memory)
let waterShadeCanvas = null;


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
 * Build a raster (canvas) of the water shading at the desired pixel size.
 * Visuals are identical to the old array-of-strings method.
 */
function computeWaterShadingCanvas(grid, targetW, targetH) {
  const H = grid.rows, W = grid.cols;
  const baseColor  = '#2d7efc';
  const lightColor = '#154ca3';

  // 1) Distance grid + queue of land cells
  const dist  = Array.from({ length: H }, () => Array(W).fill(Infinity));
  const queue = [];
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      if (grid.cells[y][x].terrain !== 'WATER') {
        dist[y][x] = 0;
        queue.push([y, x]);
      }
    }
  }

  // 2) Multi-source BFS (8-neighbor, identical to your previous version)
  const dirs = [[ 1,  0], [-1,  0], [ 0,  1], [ 0, -1],
                [ 1,  1], [ 1, -1], [-1,  1], [-1, -1]];
  let head = 0;
  while (head < queue.length) {
    const [y, x] = queue[head++];
    for (const [dy, dx] of dirs) {
      const ny = y + dy, nx = x + dx;
      const step = (dy && dx) ? Math.SQRT2 : 1;
      if (
        ny >= 0 && ny < H && nx >= 0 && nx < W &&
        grid.cells[ny][nx].terrain === 'WATER' &&
        dist[ny][nx] > dist[y][x] + step
      ) {
        const nd = dist[y][x] + step;
        // early-exit at the visual clamp (matches the old suggestion)
        if (nd > 100) continue;
        dist[ny][nx] = nd;
        queue.push([ny, nx]);
      }
    }
  }

  // 3) Find actual max (over WATER only), then clamp
  let actualMax = 0;
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      if (grid.cells[y][x].terrain === 'WATER') {
        actualMax = Math.max(actualMax, dist[y][x]);
      }
    }
  }
  const clampMax = 100;
  const maxDist = Math.min(actualMax, clampMax) || 1;

  // 4) Rasterize into a canvas
  const c = document.createElement('canvas');
  c.width  = Math.max(1, Math.round(targetW));
  c.height = Math.max(1, Math.round(targetH));
  const ctx = c.getContext('2d', { willReadFrequently: false });
  ctx.imageSmoothingEnabled = false;

    // NEW: integer pixel edges so every cell lines up perfectly
  const xEdge = new Int32Array(W + 1);
  const yEdge = new Int32Array(H + 1);
  for (let i = 0; i <= W; i++) xEdge[i] = Math.round((i * c.width)  / W);
  for (let j = 0; j <= H; j++) yEdge[j] = Math.round((j * c.height) / H);

  const cellW = c.width  / W;
  const cellH = c.height / H;

  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      if (grid.cells[y][x].terrain !== 'WATER') continue;

      // normalized [0..1], biased a touch like before
      let t = dist[y][x] / maxDist;
      t = Math.min(1, Math.max(0, Math.pow(t, 0.6)));

      const col = lerpColor(baseColor, lightColor, t);

      const x0 = xEdge[x],     x1 = xEdge[x + 1];
      const y0 = yEdge[y],     y1 = yEdge[y + 1];
      const w  = x1 - x0,      h  = y1 - y0;

      if (w > 0 && h > 0) {
        ctx.fillStyle = col;
        ctx.fillRect(x0, y0, w, h);
      }
    }
  }

  return c;
}

/**
 * Precompute water blob shading for the current grid.
 * Call this once whenever grid.cells changes (randomize, import, resize).
 */
function precomputeWaterShading(grid, pixelW, pixelH) {
  // Build at the requested size; callers will pass canvas.width/height.
  waterShadeCanvas = computeWaterShadingCanvas(
    grid,
    pixelW ?? (window.canvas?.width  ?? grid.cols),
    pixelH ?? (window.canvas?.height ?? grid.rows)
  );
}
window.precomputeWaterShading = precomputeWaterShading;

// Optional: manual memory release hook
window.freeWaterShading = function () { waterShadeCanvas = null; };

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


    // Snap to the exact same integer edges used by the water raster
  const cols = grid.cols, rows = grid.rows;
  const xEdge = new Int32Array(cols + 1);
  const yEdge = new Int32Array(rows + 1);
  for (let i = 0; i <= cols; i++) xEdge[i] = Math.round((i * ctx.canvas.width)  / cols);
  for (let j = 0; j <= rows; j++) yEdge[j] = Math.round((j * ctx.canvas.height) / rows);

  // Draw cached water shading first (scaled to the target surface)
if (waterShadeCanvas) {
  ctx.drawImage(waterShadeCanvas, 0, 0, ctx.canvas.width, ctx.canvas.height);
}

  const VARIANT_BLEND = 0.1;  // subtle mix for other terrains

  for (let y = 0; y < grid.rows; y++) {
    for (let x = 0; x < grid.cols; x++) {
      const cell = grid.cells[y][x];
      let color;

if (cell.terrain === 'WATER') {
  if (waterShadeCanvas) {
    // Already drawn from the cached bitmap
    continue;
  } else {
    // Fallback if cache not built yet
    color = TERRAIN.WATER.color;
  }
} else {
  // all other terrains: base color blended with its variant
  const base = TERRAIN[cell.terrain].color;
  const variants = TERRAIN_VARIANTS[cell.terrain] || [ base ];
  const idx = window.variantGrid?.[y]?.[x] ?? 0;
  const varCol = variants[idx % variants.length];
  color = lerpColor(base, varCol, VARIANT_BLEND);
        // Extra tint for mountains: deeper inside looks “higher”
if (cell.terrain === 'MOUNTAIN' && MOUNTAIN_DEPTH) {
  color = mountainTintColor(color, x, y, grid.cols);
}
      }

      const x0 = xEdge[x],     x1 = xEdge[x + 1];
      const y0 = yEdge[y],     y1 = yEdge[y + 1];
      const w  = x1 - x0,      h  = y1 - y0;

      if (w > 0 && h > 0) {
        ctx.fillStyle = color;
        ctx.fillRect(x0, y0, w, h);
      }
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

// --- Value layer rendering (0..61 mapped to a color scale) ---
function valueColor(t) {
  // t ∈ [0,1] => simple purple→green→yellow ramp
  t = Math.max(0, Math.min(1, t));
  const h = 270 - 210 * t;   // 270 (purple) → 60 (yellow)
  const s = 95;
  const l = 40 + 15 * t;
  return `hsl(${h} ${s}% ${l}%)`;
}

/**
 * Draws a semi‑transparent cost heatmap overlay for one empire,
 * but *excludes* any cells that another empire controls—even for
 * computing the color scale.
 */
/**
 * Draws a semi-transparent cost heatmap overlay for one empire,
 * with hostile (other empires) cells in black. Min/max come from
 * neutral + own cells only.
 */
function drawHeatmap(emp) {
  if (!emp) return;

  const canvas = document.getElementById('mapCanvas');
  const ctx    = canvas.getContext('2d');

  const rows = window.grid.rows, cols = window.grid.cols, N = rows * cols;

  // --- snap to integer pixel edges to avoid seams ---
  const xEdge = new Int32Array(cols + 1);
  const yEdge = new Int32Array(rows + 1);
  for (let i = 0; i <= cols; i++) xEdge[i] = Math.round((i * canvas.width)  / cols);
  for (let j = 0; j <= rows; j++) yEdge[j] = Math.round((j * canvas.height) / rows);

  // Precompute per-cell integer rects
  const cellRect = (x, y) => {
    const x0 = xEdge[x],   x1 = xEdge[x + 1];
    const y0 = yEdge[y],   y1 = yEdge[y + 1];
    return [x0, y0, x1 - x0, y1 - y0];
  };

  // Optional: doesn’t affect shapes, but harmless to keep off
  if ('imageSmoothingEnabled' in ctx) ctx.imageSmoothingEnabled = false;

  // Owner lookup
  const ownerId = new Int32Array(N);
  for (const e of EmpireManager.empires) {
    if (!e.territory) continue;
    for (const idx of e.territory) ownerId[idx] = e.id;
  }

// Cost accessor (flat or 2D)
  //const N = rows * cols;

  const flat =
    (emp.costMapFlat instanceof Float32Array && emp.costMapFlat.length === N)
      ? emp.costMapFlat
      : null;

  const has2D =
    Array.isArray(emp.costMap) &&
    emp.costMap.length === rows &&
    Array.isArray(emp.costMap[0]) &&
    emp.costMap[0].length === cols;

  // If there is no usable cost map yet, don't try to draw a heatmap
  if (!flat && !has2D) {
    console.warn('drawHeatmap: no cost map for empire', emp.name);
    return;
  }

  const getCost = (x, y) => {
    if (flat) {
      return flat[y * cols + x];
    }
    const row = emp.costMap[y];
    return row ? row[x] : Infinity;
  };

// Collect reachable cells for ranking; optionally include enemy territory
const vals = [];
for (let y = 0; y < rows; y++) {
  for (let x = 0; x < cols; x++) {
    const idx = y * cols + x;

    const c = getCost(x, y);
    if (isFinite(c)) vals.push([c, idx]);
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

// Draw overlay: use viridis for ranked cells; hostile cells are left
// unfilled (terrain shows through) when includeEnemy is false.
ctx.save();
ctx.globalAlpha = 1; // solid overlay where we draw it
for (let y = 0; y < rows; y++) {
  for (let x = 0; x < cols; x++) {
    const idx = y * cols + x;
    const [rx, ry, rw, rh] = cellRect(x, y);

    const t = rank[idx];
    if (t < 0) continue; // unreachable / not in ranking

    if (rw > 0 && rh > 0) {
      ctx.fillStyle = viridis(t);
      ctx.fillRect(rx, ry, rw, rh);
    }
  }
}


  // --- Empire borders in black ---
  const borderWidth =
    Math.max(1, Math.min(canvas.width / cols, canvas.height / rows) * 0.08);

  ctx.strokeStyle = '#000';
  ctx.lineWidth   = borderWidth;
  ctx.beginPath();

  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      const idx = y * cols + x;
      const id  = ownerId[idx];

      if (x < cols - 1) {
        const idxR = idx + 1;
        const idR  = ownerId[idxR];
        if (id !== idR) {
          const xBorder = xEdge[x + 1] + 0.5;
          const y0 = yEdge[y];
          const y1 = yEdge[y + 1];
          ctx.moveTo(xBorder, y0);
          ctx.lineTo(xBorder, y1);
        }
      }

      if (y < rows - 1) {
        const idxD = (y + 1) * cols + x;
        const idD  = ownerId[idxD];
        if (id !== idD) {
          const yBorder = yEdge[y + 1] + 0.5;
          const x0 = xEdge[x];
          const x1 = xEdge[x + 1];
          ctx.moveTo(x0, yBorder);
          ctx.lineTo(x1, yBorder);
        }
      }
    }
  }

  ctx.stroke();
  ctx.restore();
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
