// trade.js
(() => {
  // =====================
  // TRADE (hover-focused)
  // - Hover route: popup + highlight
  // - Hover capital: popup + highlight all connected routes
  // - Leaderboard: Connections + Pass-through (sortable), no PageRank
  // =====================

  // ---- Globals (preserve if already set) ----
  window.tradeView        = window.tradeView ?? false;
  window.tradeRoutesPath  = window.tradeRoutesPath ?? null;  // Path2D: all routes (base)
  window.tradeEmpires     = window.tradeEmpires ?? null;     // empires used to build overlay
  window.tradeOutDegree   = window.tradeOutDegree ?? null;   // Uint16Array (still used for ring size)
  window.tradeRoutesStats = window.tradeRoutesStats ?? null; // { cities, routes, ms }

  window.tradeIncomingSlots = (window.tradeIncomingSlots ?? 2) | 0;

  window._tradeComputeRunning = window._tradeComputeRunning ?? false;
  window._tradeComputeQueued  = window._tradeComputeQueued  ?? false;

  // per-route “passes through”
  // key:  "srcId->dstId"  value: array of empire ids encountered in order of first appearance
  window.tradeRouteThrough = window.tradeRouteThrough ?? new Map();
  window.tradeIdToEmpire   = window.tradeIdToEmpire   ?? new Map();

  // per-route Path2D for hit-testing and highlighting
  // key: "srcId->dstId" -> Path2D
  window.tradeRoutePaths = window.tradeRoutePaths ?? new Map();

  window.tradeRouteCells = window.tradeRouteCells ?? new Map(); // key "srcId->dstId" -> Int32Array of cell indices in travel order (src->dst)

  // adjacency by empireId
  window.tradeOutgoingById = window.tradeOutgoingById ?? new Map(); // id -> [{key,srcId,dstId},...]
  window.tradeIncomingById = window.tradeIncomingById ?? new Map();

  // NEW: metrics for leaderboard
  window.tradeConnectionsById = window.tradeConnectionsById ?? new Map(); // id -> number
  window.tradePassThroughById = window.tradePassThroughById ?? new Map(); // id -> number

  // hover state
  window.tradeHoveredCapitalId = window.tradeHoveredCapitalId ?? null;
  window.tradeHoveredRouteKey  = window.tradeHoveredRouteKey  ?? null;

  // ---- Leaderboard sorting state ----
  // keys: 'conn' | 'pass'
  window.tradeLbSortKey = window.tradeLbSortKey || 'conn';
  window.tradeLbSortDir = window.tradeLbSortDir || -1; // -1 desc, +1 asc

window.invalidateTradeRoutes = function invalidateTradeRoutes() {
  // Mark that routes need recomputation, but don't null them yet
  // This prevents flickering - old routes stay visible until new ones are ready
  window._tradeRoutesInvalid = true;

  // Clear derived data that will be rebuilt
  window.tradeRouteThrough = new Map();
  window.tradeRouteCells = new Map();

  // Force cached overlay rebuild + redraw
  window.tradeOverlayDirty = true;
  window.requestDraw?.();

  // If trade view is active, trigger recomputation
  if (window.tradeView) {
    window.computeTradeRoutes?.(false);
  }
};



  // =====================
  // Overlay canvas (highlights drawn here)
  // =====================
  let overlayCanvas = null;
  let overlayCtx = null;
  let mapCanvas = null;

  function ensureOverlayCanvas() {
    mapCanvas = document.getElementById('mapCanvas');
    if (!mapCanvas) return;

    if (!overlayCanvas) {
      overlayCanvas = document.createElement('canvas');
      overlayCanvas.id = 'tradeHoverCanvas';
      overlayCanvas.style.position = 'absolute';
      overlayCanvas.style.pointerEvents = 'none';
      overlayCanvas.style.zIndex = '25';

      const parent = mapCanvas.parentElement;
      if (parent) {
        const cs = getComputedStyle(parent);
        if (cs.position === 'static') parent.style.position = 'relative';
        parent.appendChild(overlayCanvas);
      } else {
        document.body.appendChild(overlayCanvas);
      }

      overlayCtx = overlayCanvas.getContext('2d');

      const ro = new ResizeObserver(() => syncOverlayToMap());
      ro.observe(mapCanvas);
      window.addEventListener('resize', syncOverlayToMap);
      syncOverlayToMap();
    }
  }

  function syncOverlayToMap() {
    if (!mapCanvas || !overlayCanvas) return;

    overlayCanvas.width  = mapCanvas.width;
    overlayCanvas.height = mapCanvas.height;

    const rect = mapCanvas.getBoundingClientRect();
    const parentRect = mapCanvas.parentElement?.getBoundingClientRect();
    if (parentRect) {
      overlayCanvas.style.left = (rect.left - parentRect.left) + 'px';
      overlayCanvas.style.top  = (rect.top  - parentRect.top)  + 'px';
    } else {
      overlayCanvas.style.left = rect.left + 'px';
      overlayCanvas.style.top  = rect.top  + 'px';
    }
    overlayCanvas.style.width  = rect.width + 'px';
    overlayCanvas.style.height = rect.height + 'px';

    redrawTradeHoverOverlay();
  }

  function clearOverlay() {
    if (!overlayCtx || !overlayCanvas) return;
    overlayCtx.clearRect(0, 0, overlayCanvas.width, overlayCanvas.height);
  }

  // White with black border highlight
  function strokeHighlightedPath(ctx, path, outerW, innerW) {
    ctx.save();
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';

    ctx.lineWidth = outerW;
    ctx.strokeStyle = 'rgba(0,0,0,0.75)';
    ctx.stroke(path);

    ctx.lineWidth = innerW;
    ctx.strokeStyle = 'rgba(255,255,255,0.95)';
    ctx.stroke(path);

    ctx.restore();
  }

  function redrawTradeHoverOverlay() {
    ensureOverlayCanvas();
    if (!overlayCtx || !overlayCanvas) return;

    clearOverlay();
    if (!window.tradeView) return;

    const routes = window.tradeRoutePaths;
    if (!routes || routes.size === 0) return;

    const ctx = overlayCtx;

    // 1) If hovering a capital: highlight ALL connected routes
    const capId = window.tradeHoveredCapitalId;
    if (capId != null) {
      const out = window.tradeOutgoingById?.get(capId) || [];
      const inc = window.tradeIncomingById?.get(capId) || [];

      // Draw all connected routes (slightly thinner)
      for (const r of out) {
        const p = routes.get(r.key);
        if (p) strokeHighlightedPath(ctx, p, 7, 4);
      }
      for (const r of inc) {
        const p = routes.get(r.key);
        if (p) strokeHighlightedPath(ctx, p, 7, 4);
      }
    }

    // 2) If hovering a route: draw it on top, thicker
    const hk = window.tradeHoveredRouteKey;
    if (hk) {
      const p = routes.get(hk);
      if (p) strokeHighlightedPath(ctx, p, 10, 6);
    }
  }

  // =====================
  // Popup
  // =====================
  const ui = { popup: null };

  function grabUI() {
    ui.popup = document.getElementById('trade-route-popup');
  }

  function showPopup(html, clientX, clientY) {
    if (!ui.popup) return;
    ui.popup.innerHTML = html;
    ui.popup.style.display = 'block';
    ui.popup.style.left = (clientX + 12) + 'px';
    ui.popup.style.top  = (clientY + 12) + 'px';
  }

  function movePopup(clientX, clientY) {
    if (!ui.popup || ui.popup.style.display === 'none') return;
    ui.popup.style.left = (clientX + 12) + 'px';
    ui.popup.style.top  = (clientY + 12) + 'px';
  }

  function hidePopup() {
    if (!ui.popup) return;
    ui.popup.style.display = 'none';
  }

  function empireById(id) {
    return window.tradeIdToEmpire?.get(id) || (window.EmpireManager?.empires || []).find(e => e?.id === id) || null;
  }

  function empireName(id) {
    const e = empireById(id);
    return e?.name || (e ? `Empire ${e.id}` : `Empire ${id}`);
  }

  // =====================
  // Hover hit-testing (capital first, then route)
  // =====================
  let hoverRAF = null;
  let lastEvt = null;

  function canvasPointFromMouseEvent(ev) {
    const rect = mapCanvas.getBoundingClientRect();
    const xCss = ev.clientX - rect.left;
    const yCss = ev.clientY - rect.top;
    const x = xCss * (mapCanvas.width / rect.width);
    const y = yCss * (mapCanvas.height / rect.height);
    return { x, y };
  }

  function pickCapitalAtPoint(px, py) {
    const grid = window.grid;
    if (!grid) return null;

    const emps = (window.EmpireManager?.empires || []).filter(e => e?.capital);
    if (!emps.length) return null;

    const cellW = mapCanvas.width / grid.cols;
    const cellH = mapCanvas.height / grid.rows;
    const hitR = Math.max(10, Math.min(cellW, cellH) * 0.55);

    let bestId = null;
    let bestD2 = Infinity;

    for (const e of emps) {
      const ex = (e.capital.x + 0.5) * cellW;
      const ey = (e.capital.y + 0.5) * cellH;
      const dx = px - ex;
      const dy = py - ey;
      const d2 = dx * dx + dy * dy;
      if (d2 <= hitR * hitR && d2 < bestD2) {
        bestD2 = d2;
        bestId = e.id;
      }
    }
    return bestId;
  }

  function pickRouteAtPoint(px, py) {
    if (!overlayCtx) return null;
    const routes = window.tradeRoutePaths;
    if (!routes || routes.size === 0) return null;

    // Test previously-hovered route first (fast common case)
    const hk = window.tradeHoveredRouteKey;
    if (hk) {
      const p0 = routes.get(hk);
      if (p0) {
        overlayCtx.save();
        overlayCtx.lineWidth = 10;
        const ok = overlayCtx.isPointInStroke(p0, px, py);
        overlayCtx.restore();
        if (ok) return hk;
      }
    }

    // Otherwise scan; acceptable for typical route counts
    overlayCtx.save();
    overlayCtx.lineWidth = 10;
    for (const [key, path] of routes.entries()) {
      if (overlayCtx.isPointInStroke(path, px, py)) {
        overlayCtx.restore();
        return key;
      }
    }
    overlayCtx.restore();
    return null;
  }

  function updateHoverFromEvent(ev) {
    if (!window.tradeView) return;

    const { x, y } = canvasPointFromMouseEvent(ev);

    // 1) Capital hover has priority
    const capId = pickCapitalAtPoint(x, y);
    if (capId != null) {
      if (window.tradeHoveredCapitalId !== capId) {
        window.tradeHoveredCapitalId = capId;
        window.tradeHoveredRouteKey = null; // clear route hover when on capital
        redrawTradeHoverOverlay();
      }

      const outN = (window.tradeOutgoingById?.get(capId) || []).length;
      const inN  = (window.tradeIncomingById?.get(capId) || []).length;
      const conn = outN + inN;
      const pass = window.tradePassThroughById?.get(capId) || 0;

      showPopup(
        `<div style="font-weight:700;margin-bottom:6px;">${empireName(capId)}</div>
         <div style="opacity:0.9;">
           <div><span style="opacity:0.7;">Connections:</span> ${conn} <span style="opacity:0.7;">(out ${outN}, in ${inN})</span></div>
           <div><span style="opacity:0.7;">Routes passing through:</span> ${pass}</div>
         </div>`,
        ev.clientX, ev.clientY
      );
      movePopup(ev.clientX, ev.clientY);
      return;
    }

    // 2) Otherwise route hover
    window.tradeHoveredCapitalId = null;

    const routeKey = pickRouteAtPoint(x, y);
    if (routeKey) {
      if (window.tradeHoveredRouteKey !== routeKey) {
        window.tradeHoveredRouteKey = routeKey;
        redrawTradeHoverOverlay();
      }

      const [srcIdStr, dstIdStr] = routeKey.split('->');
      const srcId = parseInt(srcIdStr, 10);
      const dstId = parseInt(dstIdStr, 10);

      const through = window.tradeRouteThrough?.get(routeKey) || [];
      const names = through.map(empireName).join(' → ');

      showPopup(
        `<div style="font-weight:700;margin-bottom:6px;">${empireName(srcId)} → ${empireName(dstId)}</div>
         <div style="opacity:0.9;">
           <div><span style="opacity:0.7;">Passes through:</span> ${names || '—'}</div>
         </div>`,
        ev.clientX, ev.clientY
      );
      movePopup(ev.clientX, ev.clientY);
      return;
    }

    // 3) Nothing hovered
    if (window.tradeHoveredRouteKey != null || window.tradeHoveredCapitalId != null) {
      window.tradeHoveredRouteKey = null;
      window.tradeHoveredCapitalId = null;
      redrawTradeHoverOverlay();
    }
    hidePopup();
  }

  function onMouseMove(ev) {
    if (!window.tradeView) return;
    lastEvt = ev;
    if (hoverRAF) return;
    hoverRAF = requestAnimationFrame(() => {
      hoverRAF = null;
      if (lastEvt) updateHoverFromEvent(lastEvt);
    });
  }

  function onMouseLeave() {
    if (!window.tradeView) return;
    window.tradeHoveredRouteKey = null;
    window.tradeHoveredCapitalId = null;
    redrawTradeHoverOverlay();
    hidePopup();
  }

  function installHoverHandlers() {
    mapCanvas = document.getElementById('mapCanvas');
    if (!mapCanvas) return;
    mapCanvas.addEventListener('mousemove', onMouseMove);
    mapCanvas.addEventListener('mouseleave', onMouseLeave);
  }

  // =====================
  // Leaderboard (Connections + Pass-through)
  // =====================
  const TRADE_LB_COLUMNS = { name: 'City', conn: 'Connections', pass: 'Pass-through' };

  window.updateTradeLeaderboardHeaders = function () {
    const table = document.getElementById('trade-leaderboard-table');
    if (!table) return;

    const ths = table.querySelectorAll('thead th[data-sort]');
    ths.forEach(th => {
      const key = th.dataset.sort;
      const base = TRADE_LB_COLUMNS[key] || key;

      if (key === window.tradeLbSortKey) {
        th.textContent = base + (window.tradeLbSortDir === -1 ? ' \u25BC' : ' \u25B2');
      } else {
        th.textContent = base;
      }
    });
  };

  window.setTradeLeaderboardSort = function (key) {
    if (!(key in TRADE_LB_COLUMNS)) return;
    if (window.tradeLbSortKey === key) {
      window.tradeLbSortDir *= -1;
    } else {
      window.tradeLbSortKey = key;
      window.tradeLbSortDir = (key === 'name') ? 1 : -1;
    }
    window.renderTradeLeaderboard?.();
  };

  window.renderTradeLeaderboard = function () {
    const tbody = document.querySelector('#trade-leaderboard-table tbody');
    if (!tbody) return;

    const emps = window.tradeEmpires || (window.EmpireManager?.empires || []).filter(e => e?.capital);
    const connById = window.tradeConnectionsById || new Map();
    const passById = window.tradePassThroughById || new Map();

    const rows = emps.map(e => {
      const id = e.id;
      return {
        id,
        name: e.name || `Empire ${id}`,
        conn: connById.get(id) || 0,
        pass: passById.get(id) || 0
      };
    });

    const key = window.tradeLbSortKey;
    const dir = window.tradeLbSortDir;

    rows.sort((a, b) => {
      const va = a[key], vb = b[key];
      if (key === 'name') {
        const cmp = String(va).localeCompare(String(vb));
        return cmp !== 0 ? dir * cmp : 0;
      }
      if (va !== vb) return dir * (va < vb ? -1 : 1);
      return a.name.localeCompare(b.name);
    });

    let html = '';
    for (let i = 0; i < rows.length; i++) {
      const r = rows[i];
      html += `
        <tr>
          <td style="text-align:left; padding:4px 6px; opacity:0.8;">${i + 1}</td>
          <td style="text-align:left; padding:4px 6px;">${r.name}</td>
          <td style="text-align:right; padding:4px 6px;">${r.conn}</td>
          <td style="text-align:right; padding:4px 6px;">${r.pass}</td>
        </tr>`;
    }
    tbody.innerHTML = html;
    window.updateTradeLeaderboardHeaders?.();
  };

function computePassThroughFromCells(cells, ownerByIdx, srcId, dstId) {
  const seen = new Set();
  const out = [];

  const add = (id) => {
    if (id == null || id < 0) return;
    if (!seen.has(id)) { seen.add(id); out.push(id); }
  };

  // Ensure start -> … -> end order
  add(srcId);
  for (let i = 0; i < cells.length; i++) add(ownerByIdx[cells[i]]);
  add(dstId);

  return out;
}

// Recompute ONLY pass-through lists + per-empire pass-through counts
// Uses existing window.tradeRouteCells (no Dijkstra, no route rebuilding)
window.recomputeTradePassThroughOnly = function recomputeTradePassThroughOnly() {
  const grid = window.grid;
  const routeCells = window.tradeRouteCells;

  if (!grid || !routeCells || routeCells.size === 0) return;

  const rows = grid.rows, cols = grid.cols;

  // Build current ownership map from territories
  const allEmps = (window.EmpireManager?.empires || []);
  const ownerByIdx = new Int32Array(rows * cols);
  ownerByIdx.fill(-1);

  for (const e of allEmps) {
    if (!e?.territory) continue;
    for (const idx of e.territory) ownerByIdx[idx] = e.id;
  }

  const newRouteThrough = new Map();
  const passThroughById = new Map();

  for (const [routeKey, cellsArr] of routeCells.entries()) {
    const [srcIdStr, dstIdStr] = routeKey.split('->');
    const srcId = parseInt(srcIdStr, 10);
    const dstId = parseInt(dstIdStr, 10);

    const through = computePassThroughFromCells(cellsArr, ownerByIdx, srcId, dstId);
    newRouteThrough.set(routeKey, through);

    for (const id of through) {
      passThroughById.set(id, (passThroughById.get(id) || 0) + 1);
    }
  }

  window.tradeRouteThrough = newRouteThrough;
  window.tradePassThroughById = passThroughById;

  // If trade view is visible, keep UI/popup data fresh
  if (window.tradeView) {
    window.renderTradeLeaderboard?.();
    // optional: redraw overlay so hover popups immediately reflect new order/list
    // (cheap, since it draws only highlighted routes)
    // redrawTradeHoverOverlay();  // keep if you want “instant” refresh while hovering
  }
};


// Terrain bytes cache access (fast)
function getTerrainBytesForTrade(grid) {
  const N = grid.rows * grid.cols;
  if (window._terrainCodeFlatCache instanceof Uint8Array && window._terrainCodeFlatCache.length === N) {
    return window._terrainCodeFlatCache;
  }
  // fallback if main.js helper exists in global scope
  if (typeof getTerrainBytes === 'function') return getTerrainBytes(grid);
  return null;
}

// Compute how much of the existing shortest-path route is WATER/OCEAN cost.
// We walk srcEmp.parentIdx backwards from dst capital -> src capital.
function computeWaterCostAlongParent(srcEmp, srcCapIdx, dstCapIdx, terrainBytes, cols, rows) {
  const parent = srcEmp.parentIdx;
  if (!(parent instanceof Int32Array)) return Infinity;

  const speeds = srcEmp.travelSpeeds || {};
  const TERRAIN_KEYS = ['PLAIN','DESERT','WATER','MOUNTAIN','FOREST','SHRUB','RIVER','ICE','OCEAN'];

  const WATER_CODE = 2;
  const OCEAN_CODE = 8;

  let cur = dstCapIdx;
  let safety = rows * cols;
  let water = 0;

  while (cur !== srcCapIdx && safety-- > 0) {
    const p = parent[cur];
    if (p < 0 || p === cur) return Infinity;

    const code = terrainBytes[cur] | 0;
    const x1 = cur % cols, y1 = (cur / cols) | 0;
    const x2 = p % cols,   y2 = (p / cols) | 0;
    const isDiag = (x1 !== x2 && y1 !== y2);

    const key = TERRAIN_KEYS[code] || 'PLAIN';
    const base = speeds[key] ?? speeds.PLAIN ?? 1;
    const step = isDiag ? base * Math.SQRT2 : base;

    if (code === WATER_CODE || code === OCEAN_CODE) water += step;

    cur = p;
  }

  return (cur === srcCapIdx) ? water : Infinity;
}



  // =====================
  // Compute trade routes (unchanged idea, but adds pass-through + metrics)
  // =====================
  window.computeTradeRoutes = async function (force = false) {
    // Check if routes are invalid or missing
    if (!force && window.tradeRoutesPath && !window._tradeRoutesInvalid) return;

    // Clear the invalid flag since we're recomputing
    window._tradeRoutesInvalid = false;

    window._tradeComputeQueued = true;
    if (window._tradeComputeRunning) return;
    window._tradeComputeRunning = true;

    try {
      while (window._tradeComputeQueued) {
        window._tradeComputeQueued = false;

        const t0 = performance.now();

        const grid = window.grid;
        const ghostCitiesEnabled = window.ghostCitiesEnabled !== false; // default true

        // Filter empires: must have capital, and if ghost cities disabled, must have territory
        const emps = (window.EmpireManager?.empires || []).filter(e => {
          if (!e?.capital) return false;
          const isGhost = !e.territory || e.territory.size === 0;
          // Include if not a ghost, or if ghost cities are enabled
          return !isGhost || ghostCitiesEnabled;
        });

        window.tradeIdToEmpire = new Map((window.EmpireManager?.empires || []).map(e => [e.id, e]));

        if (!grid || emps.length < 2) {
          window.tradeRoutesPath  = null;
          window.tradeEmpires     = emps;
          window.tradeOutDegree   = null;
          window.tradeRoutesStats = null;

          window.tradeRouteThrough = new Map();
          window.tradeRoutePaths   = new Map();
          window.tradeOutgoingById = new Map();
          window.tradeIncomingById = new Map();
          window.tradeConnectionsById = new Map();
          window.tradePassThroughById = new Map();

          const statsEl = document.getElementById('trade-stats');
          if (statsEl) statsEl.textContent = `Cities: ${emps.length} | (need at least 2 capitals)`;

          window.renderTradeLeaderboard?.();
          redrawTradeHoverOverlay();
          return;
        }

        // Only recompute cost maps if we actually need them.
// Empire elimination alone does NOT require recomputation.
const needsCostMaps = emps.some(e =>
  !(e.costMapFlat instanceof Float32Array) ||
  !(e.parentIdx   instanceof Int32Array) ||
  e._costMapDirty
);

if (needsCostMaps) {
  await window.EmpireManager.updateAllCostMaps(grid);
  for (const e of emps) e._costMapDirty = false;
}

        const rows = grid.rows, cols = grid.cols;

        const canvas = document.getElementById('mapCanvas');
        if (!canvas) return;

        const cellW = canvas.width  / cols;
        const cellH = canvas.height / rows;
        const toPx  = (x, y) => [(x + 0.5) * cellW, (y + 0.5) * cellH];
        const idxOf = (x, y) => y * cols + x;

        const C = emps.length;
        const K = Math.max(0, Math.min(20, window.tradeIncomingSlots | 0));

        // Precompute capital cell-index for fast cost lookup
        const capIdx = new Int32Array(C);
        for (let i = 0; i < C; i++) {
          capIdx[i] = idxOf(emps[i].capital.x, emps[i].capital.y);
        }

const discount = Number.isFinite(window.tradeWaterDiscount) ? window.tradeWaterDiscount : 1.0;

// If discount is active, precompute WATER/OCEAN portion for every (src,dst) capital pair.
// This keeps the slider cheap: selection becomes O(C^2) lookups.
let waterToCap = null;
if (discount < 0.999) {
  const terrainBytes = getTerrainBytesForTrade(grid);
  if (terrainBytes) {
    waterToCap = new Float32Array(C * C);
    waterToCap.fill(NaN);

    for (let src = 0; src < C; src++) {
      const srcEmp = emps[src];
      const srcCap = capIdx[src];

      for (let dst = 0; dst < C; dst++) {
        if (dst === src) continue;
        const dstCap = capIdx[dst];

        const w = computeWaterCostAlongParent(srcEmp, srcCap, dstCap, terrainBytes, cols, rows);
        if (Number.isFinite(w)) waterToCap[src * C + dst] = w;
      }
    }
  }
}



        // 1) Pick directed edges using incoming slots
        const outAdj = Array.from({ length: C }, () => []);
        const outDeg = new Uint16Array(C);
        const edges  = new Set(); // key = src*C + dst

        for (let dst = 0; dst < C; dst++) {
  const dstEmp = emps[dst];
  const dstIdx = idxOf(dstEmp.capital.x, dstEmp.capital.y);

  const cand = [];
  for (let src = 0; src < C; src++) {
    if (src === dst) continue;
    const dist = emps[src].costMapFlat;
let c = dist ? dist[dstIdx] : Infinity;
if (!Number.isFinite(c)) continue;

// Apply discount to the *existing* route cost (selection-only)
// adjusted = total + (discount - 1) * waterPortion
if (waterToCap) {
  const w = waterToCap[src * C + dst];
  if (Number.isFinite(w)) c = c + (discount - 1.0) * w;
}

cand.push([c, src]);

  }
  cand.sort((a, b) => a[0] - b[0]);

          const take = Math.min(K, cand.length);
          for (let n = 0; n < take; n++) {
            const src = cand[n][1];
            const key = src * C + dst;
            if (edges.has(key)) continue;
            edges.add(key);
            outAdj[src].push(dst);
            outDeg[src]++;
          }
        }

        // ------------------------------
// Rule 1: Remove mutual duplicates A->B and B->A
// Keep only the cheaper direction.
// Tie-breaker: keep the direction whose SOURCE empire has fewer total connections (out+in).
// ------------------------------

// Compute in-degree (incoming count) from current edges
const inDeg = new Uint16Array(C);
for (const key of edges) {
  const dst = key % C;
  inDeg[dst]++;
}

// Total connections (out + in) BEFORE pruning (used for tie-breaking)
const conn = new Uint16Array(C);
for (let i = 0; i < C; i++) conn[i] = (outDeg[i] + inDeg[i]) | 0;

const toRemove = new Set(); // directed edge key = src*C + dst

for (let a = 0; a < C; a++) {
  const nbrs = outAdj[a];
  for (let i = 0; i < nbrs.length; i++) {
    const b = nbrs[i];
    if (a >= b) continue; // handle unordered pair once

    const keyAB = a * C + b;
    const keyBA = b * C + a;

    if (!edges.has(keyBA)) continue; // not mutual

    // Compare travel costs A->B vs B->A using cost maps
let costAB = emps[a].costMapFlat ? emps[a].costMapFlat[capIdx[b]] : Infinity;
let costBA = emps[b].costMapFlat ? emps[b].costMapFlat[capIdx[a]] : Infinity;

if (waterToCap) {
  const wAB = waterToCap[a * C + b];
  const wBA = waterToCap[b * C + a];
  if (Number.isFinite(wAB)) costAB = costAB + (discount - 1.0) * wAB;
  if (Number.isFinite(wBA)) costBA = costBA + (discount - 1.0) * wBA;
}


    let removeKey;

    if (costAB < costBA) {
      removeKey = keyBA; // keep A->B
    } else if (costBA < costAB) {
      removeKey = keyAB; // keep B->A
    } else {
      // Tie: keep the direction whose SOURCE has fewer existing connections
      const connA = conn[a] | 0;
      const connB = conn[b] | 0;

      if (connA < connB) removeKey = keyBA;       // keep A->B
      else if (connB < connA) removeKey = keyAB;  // keep B->A
      else {
        // Ultra-rare tie: deterministic fallback to avoid randomness
        removeKey = (emps[a].id <= emps[b].id) ? keyBA : keyAB;
      }
    }

    toRemove.add(removeKey);
  }
}

// Apply removals to edges + outAdj/outDeg (outDeg is used later for marker sizing)
if (toRemove.size) {
  for (const k of toRemove) edges.delete(k);

  for (let src = 0; src < C; src++) {
    const arr = outAdj[src];
    if (!arr.length) continue;

    let w = 0;
    for (let i = 0; i < arr.length; i++) {
      const dst = arr[i];
      const k = src * C + dst;
      if (toRemove.has(k)) continue;
      arr[w++] = dst;
    }
    arr.length = w;
    outDeg[src] = w;
  }
}

// owner map (flattened) for pass-through detection
// IMPORTANT: build from empires' territory sets (reliable), NOT from grid cell fields
const allEmps = (window.EmpireManager?.empires || []);
window.tradeIdToEmpire = new Map(allEmps.map(e => [e.id, e]));

const ownerByIdx = new Int32Array(rows * cols);
ownerByIdx.fill(-1);

for (const e of allEmps) {
  if (!e?.territory) continue;           // territory is a Set<number>
  for (const idx of e.territory) ownerByIdx[idx] = e.id;
}

        // 2) Build geometry + per-route info
        const bigPath  = new Path2D();
        const routeThrough = new Map();
        const routePaths   = new Map();
        const routeCells   = new Map(); // key -> Int32Array cell indices in src->dst order

        const outgoingById = new Map();
        const incomingById = new Map();
        const pushAdj = (map, id, obj) => {
          let arr = map.get(id);
          if (!arr) { arr = []; map.set(id, arr); }
          arr.push(obj);
        };

        for (let src = 0; src < C; src++) {
          const srcEmp = emps[src];
          const parent = srcEmp.parentIdx;
          if (!(parent instanceof Int32Array)) continue;

          const srcIdx = idxOf(srcEmp.capital.x, srcEmp.capital.y);

          const nbrs = outAdj[src];
          for (let k = 0; k < nbrs.length; k++) {
            const dst = nbrs[k];
            const dstEmp = emps[dst];
            const dstIdx = idxOf(dstEmp.capital.x, dstEmp.capital.y);

            let cur = dstIdx;
            let safety = rows * cols + 5;

            if (cur !== srcIdx && parent[cur] < 0) continue;

            // collect empires passed through in *travel order* (src -> dst)
// NOTE: we still walk the parent pointers dst -> src, so we collect owners and then reverse.
// collect route cell indices in travel order (src -> dst)
// NOTE: we walk parent pointers dst -> src, then reverse.
const rawCells = [];
const pushCell = (cellIdx) => { rawCells.push(cellIdx); };

// start at dst cell
pushCell(cur);

// per-route path
const rp = new Path2D();
const [dx0, dy0] = toPx(dstEmp.capital.x, dstEmp.capital.y);

// start at dst, follow parent pointers back toward src
bigPath.moveTo(dx0, dy0);
rp.moveTo(dx0, dy0);

while (cur !== srcIdx && safety-- > 0) {
  const p = parent[cur];
  if (p < 0 || p === cur) break;

  pushCell(p);

  const px = p % cols;
  const py = (p / cols) | 0;
  const [x1, y1] = toPx(px, py);

  bigPath.lineTo(x1, y1);
  rp.lineTo(x1, y1);

  cur = p;
}

// If we didn't actually reach src, skip this route (prevents garbage paths)
if (cur !== srcIdx) continue;

// reverse to get src -> dst order
rawCells.reverse();

// build + store cell list for cheap future pass-through recomputes
const routeKey = `${srcEmp.id}->${dstEmp.id}`;
const cellsArr = Int32Array.from(rawCells);
routeCells.set(routeKey, cellsArr);

// compute pass-through in correct order using CURRENT ownership
const throughIds = computePassThroughFromCells(cellsArr, ownerByIdx, srcEmp.id, dstEmp.id);

routeThrough.set(routeKey, throughIds);
routePaths.set(routeKey, rp);

pushAdj(outgoingById, srcEmp.id, { key: routeKey, srcId: srcEmp.id, dstId: dstEmp.id });
pushAdj(incomingById, dstEmp.id, { key: routeKey, srcId: srcEmp.id, dstId: dstEmp.id });

          }
        }

        // 3) Compute metrics: connections + pass-through count
        const connectionsById = new Map();
        const passThroughById = new Map();

        for (const e of emps) {
          const outN = (outgoingById.get(e.id) || []).length;
          const inN  = (incomingById.get(e.id) || []).length;
          connectionsById.set(e.id, outN + inN);
        }

        for (const ids of routeThrough.values()) {
          for (const id of ids) {
            passThroughById.set(id, (passThroughById.get(id) || 0) + 1);
          }
        }

        // Store
        window.tradeRoutesPath   = bigPath;
        window.tradeEmpires      = emps;
        window.tradeOutDegree    = outDeg;
        window.tradeRoutesStats  = { cities: C, routes: routePaths.size, ms: performance.now() - t0 };

        window.tradeRouteThrough = routeThrough;
        window.tradeRoutePaths   = routePaths;
        window.tradeRouteCells   = routeCells;

        window.tradeOutgoingById = outgoingById;
        window.tradeIncomingById = incomingById;

        window.tradeConnectionsById = connectionsById;
        window.tradePassThroughById = passThroughById;

        // UI stats + leaderboard
        const statsEl = document.getElementById('trade-stats');
        if (statsEl) {
          statsEl.textContent = `Cities: ${C} | Incoming slots: ${K} | Routes: ${routePaths.size} | Compute: ${window.tradeRoutesStats.ms.toFixed(0)} ms`;
        }
        window.renderTradeLeaderboard?.();

        // Tell main render loop to rebuild the cached trade layer + redraw
        window.tradeOverlayDirty = true;
        window.requestDraw?.();

        redrawTradeHoverOverlay();
      }
    } finally {
      window._tradeComputeRunning = false;
    }
  };

  // =====================
  // Trade view toggles
  // =====================
window.activateTradeView = function () {
  window.tradeView = true;
  ensureOverlayCanvas();

  // ensure main loop redraws immediately when entering trade view
  window.tradeOverlayDirty = true;
  window.requestDraw?.();

  // Recompute routes if missing or invalid
  if (!window.tradeRoutesPath || window._tradeRoutesInvalid) {
    window.computeTradeRoutes?.(false);
  }
  redrawTradeHoverOverlay();
};

  window.deactivateTradeView = function () {
    window.tradeView = false;
    window.tradeHoveredRouteKey = null;
    window.tradeHoveredCapitalId = null;
    hidePopup();
    clearOverlay();
  };

  // =====================
  // DOM handlers (slider, recompute, leaderboard headers)
  // =====================
  function installTradeDOMHandlers() {
    const table = document.getElementById('trade-leaderboard-table');
    if (table) {
      table.querySelectorAll('thead th[data-sort]').forEach(th => {
        th.addEventListener('click', () => window.setTradeLeaderboardSort?.(th.dataset.sort));
      });
      window.updateTradeLeaderboardHeaders?.();
    }

    const tradeRecomputeBtn = document.getElementById('trade-recompute-btn');
    tradeRecomputeBtn?.addEventListener('click', () => window.computeTradeRoutes?.(true));

    const tradeSlotsSlider = document.getElementById('trade-incoming-slots');
    const tradeSlotsVal    = document.getElementById('trade-incoming-slots-val');
    if (tradeSlotsSlider && tradeSlotsVal) {
      window.tradeIncomingSlots = parseInt(tradeSlotsSlider.value, 10) || 0;
      tradeSlotsVal.textContent = String(window.tradeIncomingSlots);

      tradeSlotsSlider.addEventListener('input', () => {
        window.tradeIncomingSlots = parseInt(tradeSlotsSlider.value, 10) || 0;
        tradeSlotsVal.textContent = String(window.tradeIncomingSlots);

        // Mark as invalid but don't null - keeps old routes visible during recomputation
        window._tradeRoutesInvalid = true;
        window.tradeOverlayDirty = true;
        if (window.tradeView) window.computeTradeRoutes?.(false);
      });
    }
  }

  // =====================
  // Boot
  // =====================
  function boot() {
    grabUI();
    ensureOverlayCanvas();
    installHoverHandlers();
    installTradeDOMHandlers();
    redrawTradeHoverOverlay();
    window.renderTradeLeaderboard?.();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot, { once: true });
  } else {
    boot();
  }
})();
