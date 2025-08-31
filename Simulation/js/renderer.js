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

// cache array of same shape as grid.rows×grid.cols for water shading:
let waterShadeMap = null;


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
 * Compute per‑blob water shading so that cells at the
 * very edge of a water‑blob stay the base deep blue,
 * and ones further inland get subtly lighter.
 */
function computeWaterShading(grid) {
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
  
  // 2) Multi-source BFS
  const dirs = [[ 1,  0], [-1,  0], [ 0,  1], [ 0, -1],
      [ 1,  1], [ 1, -1], [-1,  1], [-1, -1]];
  let head = 0;
  while (head < queue.length) {
    const [y, x] = queue[head++];
for (const [dy, dx] of dirs) {
  const ny = y + dy, nx = x + dx;
  const stepCost = (dy !== 0 && dx !== 0) ? Math.SQRT2 : 1;
  if (
    ny >= 0 && ny < H && nx >= 0 && nx < W &&
    grid.cells[ny][nx].terrain === 'WATER' &&
    dist[ny][nx] > dist[y][x] + stepCost
  ) {
    dist[ny][nx] = dist[y][x] + stepCost;
    queue.push([ny, nx]);
  }
}
  }
  
  // 3) Find the “deepest” distance
  let actualMax = 0;
  for (let y = 0; y < H; y++)
    for (let x = 0; x < W; x++)
      if (grid.cells[y][x].terrain === 'WATER')
        actualMax = Math.max(actualMax, dist[y][x]);
  
  // Optionally clamp to some sensible max so super-wide lakes don’t all go white
  const clampMax = 100;
  const maxDist = Math.min(actualMax, clampMax);
  
  // 4) Shade
  const shade = Array.from({ length: H }, () => Array(W).fill(baseColor));
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      if (grid.cells[y][x].terrain === 'WATER') {
        // normalized [0..1], maybe bias to keep coast darker
        let t = dist[y][x] / maxDist;
        t = Math.min(1, Math.max(0, Math.pow(t, 0.6)));
        shade[y][x] = lerpColor(baseColor, lightColor, t);
      }
    }
  }
  
  return shade;
}

/**
 * Precompute water blob shading for the current grid.
 * Call this once whenever grid.cells changes (randomize, import, resize).
 */
function precomputeWaterShading(grid) {
  waterShadeMap = computeWaterShading(grid);
}

// expose it so main.js can call it:
window.precomputeWaterShading = precomputeWaterShading;

/**
 * Draw the grid; grid lines off by default.
 * Applies on‑demand water shading from the cached map.
 */
function drawGrid(ctx, grid, cellSize, showGrid = false) {
  const Wpx = grid.cols * cellSize,
        Hpx = grid.rows * cellSize;

  ctx.imageSmoothingEnabled = false;
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);

  const VARIANT_BLEND = 0.1;  // subtle mix for other terrains

  for (let y = 0; y < grid.rows; y++) {
    for (let x = 0; x < grid.cols; x++) {
      const cell = grid.cells[y][x];
      let color;

      if (cell.terrain === 'WATER' && waterShadeMap) {
        // continuous water shading
        color = waterShadeMap[y][x];
      } else {
        // all other terrains: base color blended with its variant
        const base = TERRAIN[cell.terrain].color;
        const variants = TERRAIN_VARIANTS[cell.terrain] || [ base ];
        const idx = window.variantGrid?.[y]?.[x] ?? 0;
        const varCol = variants[idx % variants.length];
        color = lerpColor(base, varCol, VARIANT_BLEND);
      }

      // draw the cell
      const x0 = Math.round(x * cellSize),
            y0 = Math.round(y * cellSize),
            w  = Math.round((x + 1) * cellSize) - x0,
            h  = Math.round((y + 1) * cellSize) - y0;

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

  const rows = grid.rows, cols = grid.cols, N = rows * cols;

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

// Whether to include enemy territory in the heatmap calculation & display
const includeEnemy = !!window.includeEnemyHeatmap;

  // Cost accessor (flat or 2D)
  const flat = (emp.costMapFlat instanceof Float32Array) ? emp.costMapFlat : null;
  const getCost = (x, y) => flat ? flat[y * cols + x]
                                 : (emp.costMap ? emp.costMap[y][x] : Infinity);

// Collect reachable cells for ranking; optionally include enemy territory
const vals = [];
for (let y = 0; y < rows; y++) {
  for (let x = 0; x < cols; x++) {
    const idx = y * cols + x;

    // Exclude enemy cells from ranking unless checkbox is ON
    if (!includeEnemy && ownerId[idx] !== 0 && ownerId[idx] !== emp.id) continue;

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

  // Draw overlay: hostile = black, non-hostile = viridis(rank)
  ctx.save();
  ctx.globalAlpha = 1; // you mentioned removing transparency
  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      const idx = y * cols + x;
      const [rx, ry, rw, rh] = cellRect(x, y);

      // Hostile (any terrain): black only when the checkbox is OFF.
// When ON, we color them via Viridis like everything else.
if (!includeEnemy && ownerId[idx] !== 0 && ownerId[idx] !== emp.id) {
  if (rw > 0 && rh > 0) {
    ctx.fillStyle = '#000';
    ctx.fillRect(rx, ry, rw, rh);
  }
  continue;
}

      const t = rank[idx];
      if (t < 0) continue; // unreachable

      if (rw > 0 && rh > 0) {
        ctx.fillStyle = viridis(t);
        ctx.fillRect(rx, ry, rw, rh);
      }
    }
  }
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

  // --- helpers ---
  const flat = (emp.costMapFlat instanceof Float32Array) ? emp.costMapFlat : null;
  const getCost = (x, y) => flat ? flat[y * cols + x]
                                 : (emp.costMap ? emp.costMap[y][x] : Infinity);

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
