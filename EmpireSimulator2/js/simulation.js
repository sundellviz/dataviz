// js/simulation.js - Empire calculations, totals, leaderboard, simulation loop
(function() {
  'use strict';

  // Default travel speeds
  const DEFAULT_TRAVEL_SPEEDS = {
    PLAIN: 3,
    DESERT: 10,
    WATER: 2,
    OCEAN: 4,
    MOUNTAIN: 8,
    FOREST: 5,
    SHRUB: 4,
    RIVER: 1,
    ICE: 10,
    SWITCH: 0
  };

  const TERRAIN_KEYS = ['PLAIN', 'DESERT', 'WATER', 'OCEAN', 'MOUNTAIN', 'FOREST', 'SHRUB', 'RIVER', 'ICE'];

  // Zero-delay yield via MessageChannel (avoids browser's 4ms setTimeout clamp)
  const _yieldCh = new MessageChannel();
  const yieldToUI = () => new Promise(r => { _yieldCh.port1.onmessage = r; _yieldCh.port2.postMessage(null); });

  // Draw full frame including trade overlay (use instead of direct drawCurrent calls)
  function drawFullFrame() {
    if (typeof window.drawCurrent === 'function') window.drawCurrent();

    // Composite trade overlay if trade view is active
    if (window.tradeView && window._tradeLayer && window._ctx) {
      if (window.tradeOverlayDirty && typeof window.redrawTradeOverlay === 'function') {
        window.redrawTradeOverlay();
      }
      window._ctx.drawImage(window._tradeLayer, 0, 0);
    }
  }

  // Average land value helper
  function computeGlobalAvgLandValue() {
    const grid = window.grid;
    if (!grid) return 0;
    const rows = grid.rows, cols = grid.cols;
    if (!grid.valueLayer) return 0;

    let sum = 0, count = 0;
    for (let y = 0; y < rows; y++) {
      const row = grid.cells[y];
      for (let x = 0; x < cols; x++) {
        if (row[x].terrain === 'OCEAN') continue;
        const v = grid.getValueAt(x, y);
        sum += v;
        count++;
      }
    }
    return count ? (sum / count) : 0;
  }

  // Recompute per-empire totals and power.
  function computeEmpireTotals(updateUI = true) {
    const grid = window.grid;
    if (!grid || !window.EmpireManager) return;

    const cols = grid.cols;
    const thr = Math.max(1, Math.min(61, Number(window.growthThreshold ?? 1) | 0 || 1));

    // Use flat cached value array when available (avoids 2D lookups + division/modulo per cell)
    const auctionArrays = (typeof window.getAuctionArrays === 'function')
      ? window.getAuctionArrays(grid) : null;
    const valFlat = auctionArrays?.valFlat;
    const valueLayer = valFlat ? null : grid.valueLayer;

    // --- Pass 1: value + area per empire ---
    let totalOwned = 0;
    for (const e of EmpireManager.empires) {
      if (!e.territory) e.territory = new Set();

      const area = e.territory.size;
      let sum = 0;

      if (valFlat) {
        for (const idx of e.territory) sum += valFlat[idx];
      } else if (valueLayer) {
        for (const idx of e.territory) {
          const y = (idx / cols) | 0;
          const x = idx % cols;
          sum += valueLayer[y]?.[x] ?? 0;
        }
      } else {
        for (const idx of e.territory) {
          const y = (idx / cols) | 0;
          const x = idx % cols;
          sum += Number(grid.cells[y][x].value ?? grid.cells[y][x].landValue ?? 0);
        }
      }

      e._value = sum;
      e._area = area;
      e._avg = area > 0 ? (sum / area) : 0;
      totalOwned += area;
    }

    // --- Pass 2: compute trade shares (zero-sum) using connections + pure passthroughs ---
    const connById = window.tradeConnectionsById;
    const passById = window.tradePassThroughById;

    const tradeCountById = new Map();
    let totalTradeCount = 0;

    for (const e of EmpireManager.empires) {
      const id = e.id;
      const area = e.territory ? e.territory.size : 0;
      const isGhost = area === 0;

      const conn = (connById instanceof Map) ? (connById.get(id) || 0) : 0;
      const pass = (passById instanceof Map) ? (passById.get(id) || 0) : 0;

      // pure passthrough = passthrough - connections (clamped)
      const purePass = Math.max(0, pass - conn);

      // trade count = connections + pure passthroughs
      const tc = conn + purePass;

      tradeCountById.set(id, tc);

      // Ghost empires (no territory) don't count toward the denominator
      // They still participate in trade routes but don't share in trade power
      if (!isGhost) {
        totalTradeCount += tc;
      }
    }

    // weights
    const tradeW = Math.max(0, Math.min(1, Number(window.tradeWeight ?? 0.5)));
    const agW = 1 - tradeW;

    // --- Pass 3: target sizes + slack power ---
    for (const e of EmpireManager.empires) {
      const id = e.id;
      const area = e._area | 0;
      const value = Number(e._value || 0);
      const isGhost = area === 0;

      // agriculture target size in "cells"
      const targetAg = Math.max(0, value / thr);

      // trade target size in "cells" (slice of total owned world)
      // Ghost empires get 0 share - they route trade but don't benefit from it
      const tc = tradeCountById.get(id) || 0;
      const share = (isGhost || totalTradeCount <= 0) ? 0 : (tc / totalTradeCount);
      const targetTrade = (totalOwned > 0) ? (share * totalOwned) : 0;

      // combined target size
      const combinedTarget = agW * targetAg + tradeW * targetTrade;

      // store for UI/debug
      e._targetAg = targetAg;
      e._targetTrade = targetTrade;
      e._tradeShare = share;
      e._targetSize = Math.max(1, Math.min(100000, Math.round(combinedTarget)));

      // slack-based power
      const slack = Math.max(0, combinedTarget - area);
      e.power = Math.sqrt(slack) || 0;

      // keep the panel meta-row in sync (skip during tight simulation loops)
      if (updateUI) {
        if (e._sizeDisplay) e._sizeDisplay.textContent = `Size: ${area}`;
        if (e._valueDisplay) e._valueDisplay.textContent = `Land value: ${Math.round(value)}`;
      }
    }
  }

  // Leaderboard sort state
  const LB_COLUMNS = { name: 'Empire', cells: 'Cells', agri: 'Agri', trade: 'Trade', power: 'Power' };
  let lbSortKey = 'cells';
  let lbSortDir = -1;   // -1 = descending, 1 = ascending

  function setLeaderboardSort(key) {
    if (!(key in LB_COLUMNS)) return;
    if (lbSortKey === key) {
      lbSortDir *= -1;
    } else {
      lbSortKey = key;
      lbSortDir = (key === 'name') ? 1 : -1;   // name defaults ascending, numbers descending
    }
    renderLeaderboard();
  }

  function updateLeaderboardHeaders() {
    const table = document.getElementById('leaderboard-table');
    if (!table) return;
    table.querySelectorAll('thead th[data-sort]').forEach(th => {
      const key = th.dataset.sort;
      const base = LB_COLUMNS[key] || key;
      if (key === lbSortKey) {
        th.textContent = base + (lbSortDir === -1 ? ' \u25BC' : ' \u25B2');
      } else {
        th.textContent = base;
      }
    });
  }

  // Build and draw the Leaderboard
  function renderLeaderboard() {
    const table = document.getElementById('leaderboard-table');
    if (!table || !window.EmpireManager) return;

    // Collect data including agriculture and trade targets
    const rows = EmpireManager.empires.map(e => ({
      name: e.name || `Empire ${e.id}`,
      cells: e.territory ? e.territory.size : 0,
      agri: Number.isFinite(e._targetAg) ? e._targetAg : 0,
      trade: Number.isFinite(e._targetTrade) ? e._targetTrade : 0,
      power: Number.isFinite(e.power) ? e.power : 0
    }));

    // Sort by chosen column
    rows.sort((a, b) => {
      const va = a[lbSortKey], vb = b[lbSortKey];
      if (lbSortKey === 'name') {
        const cmp = String(va).localeCompare(String(vb));
        return cmp !== 0 ? lbSortDir * cmp : 0;
      }
      if (va !== vb) return lbSortDir * (va < vb ? -1 : 1);
      return a.name.localeCompare(b.name);
    });

    // Fill table body
    const tbody = table.querySelector('tbody');
    let html = '';
    for (let i = 0; i < rows.length; i++) {
      const r = rows[i];
      html += `<tr>
        <td style="padding:2px 4px;">${i + 1}</td>
        <td style="padding:2px 4px;">${r.name}</td>
        <td style="text-align:right; padding:2px 4px;">${r.cells}</td>
        <td style="text-align:right; padding:2px 4px;">${r.agri.toFixed(0)}</td>
        <td style="text-align:right; padding:2px 4px;">${r.trade.toFixed(0)}</td>
        <td style="text-align:right; padding:2px 4px;">${r.power.toFixed(2)}</td>
      </tr>`;
    }
    tbody.innerHTML = html;

    updateLeaderboardHeaders();

    // Wire the CSV export button once
    const btn = document.getElementById('export-leaderboard-btn');
    if (btn && !btn.dataset.wired) {
      btn.dataset.wired = '1';
      btn.addEventListener('click', () => {
        computeEmpireTotals();

        const rowsNow = EmpireManager.empires.map(e => ({
          name: e.name || `Empire ${e.id}`,
          cells: e.territory ? e.territory.size : 0,
          power: Number.isFinite(e.power) ? e.power : 0,
          speeds: e.travelSpeeds || {}
        }))
        .sort((a, b) => (b.cells - a.cells) || (b.power - a.power));

        const TERRAIN_ORDER = ['PLAIN', 'DESERT', 'WATER', 'MOUNTAIN', 'FOREST', 'SHRUB', 'RIVER', 'ICE'];

        const header =
          ['rank', 'empire', 'cells', 'power', 'switch', ...TERRAIN_ORDER.map(t => t.toLowerCase())]
          .join(',') + '\n';

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

        const csv = header + lines.join('\n');
        if (typeof window.downloadTextFile === 'function') {
          window.downloadTextFile('leaderboard.csv', csv);
        }
      });
    }
  }

  // Nudge actual caps (e.size) TOWARD e._targetSize by at most `step`.
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

  // Main simulation step: refresh cost maps and recompute ownership
  async function simulateAndDraw() {
    const grid = window.grid;
    if (!grid || !window.EmpireManager) return;

    // 1) Refresh land totals so power reflects current average value
    computeEmpireTotals();

    // 2) Ensure all empires have up-to-date base travel-cost maps
    const needsRefresh = EmpireManager.empires.some(e => e._costMapDirty);
    const hasCostMaps = EmpireManager.empires.every(e =>
      e.costMapFlat instanceof Float32Array && e.costMapFlat.length > 0
    );

    if (needsRefresh || !hasCostMaps) {
      const t0 = performance.now();
      await EmpireManager.updateAllCostMaps(grid);
      if (window.perfMonitor) window.perfMonitor.record('costmap', performance.now() - t0);
      for (const e of EmpireManager.empires) e._costMapDirty = false;
    }

    // 3) Global auction recompute
    if (typeof window.recomputeOwnershipAuctionGlobal === 'function') {
      const t0 = performance.now();
      await window.recomputeOwnershipAuctionGlobal();
      if (window.perfMonitor) window.perfMonitor.record('sim', performance.now() - t0);
    }

    // Borders changed -> refresh pass-through cheaply
    if (!window._tradeNeedsRecompute && typeof window.recomputeTradePassThroughOnly === 'function') {
      window.recomputeTradePassThroughOnly();
    }

    // Refresh totals for the UI
    computeEmpireTotals();

    // Trade routes update when empires are eliminated
    if (window._tradeNeedsRecompute) {
      window._tradeNeedsRecompute = false;
      if (typeof window.computeTradeRoutes === 'function') {
        window.computeTradeRoutes(true).then(() => {
          window.tradeOverlayDirty = true;
          if (typeof window.requestDraw === 'function') window.requestDraw();
        });
      }
    }

    if (typeof window.requestDraw === 'function') window.requestDraw();
    renderLeaderboard();
  }

  // Recalibrate loop using the global auction
  async function runRecalibrateWithDynamicSizes(btn) {
    btn.dataset.running = '1';
    const originalLabel = btn.textContent;
    btn.textContent = 'Stop';

    const grid = window.grid;

    try {
      let iter = 0;

      // Check if we need fresh cost maps before starting
      const needsRefresh = EmpireManager.empires.some(e => e._costMapDirty);
      const hasCostMaps = EmpireManager.empires.every(e =>
        e.costMapFlat instanceof Float32Array && e.costMapFlat.length > 0
      );

      if (needsRefresh || !hasCostMaps) {
        const t0 = performance.now();
        await EmpireManager.updateAllCostMaps(grid);
        if (window.perfMonitor) window.perfMonitor.record('costmap', performance.now() - t0);
        for (const e of EmpireManager.empires) e._costMapDirty = false;
      }

      let lastDrawTime = performance.now();

      while (btn.dataset.running === '1') {
        const prevOwnerVersion = window._ownerVersion | 0;
        const prevEmpireCount = EmpireManager.empires.length;

        // 1) Totals + power from current territory (skip DOM updates in tight loop)
        computeEmpireTotals(false);

        // 2) Nudge actual sizes toward target
        const sizeChanged = nudgeSizesTowardTarget(window.autoGrowAmount, true);

        // 3) Full auction assignment
        if (typeof window.recomputeOwnershipAuctionGlobal === 'function') {
          const t0 = performance.now();
          await window.recomputeOwnershipAuctionGlobal();
          if (window.perfMonitor) window.perfMonitor.record('sim', performance.now() - t0);
        }

        // Borders changed -> refresh pass-through cheaply
        if (typeof window.recomputeTradePassThroughOnly === 'function') {
          window.recomputeTradePassThroughOnly();
        }

        // If an empire was eliminated, rebuild trade routes
        if (EmpireManager.empires.length !== prevEmpireCount) {
          // Don't null the paths - computeTradeRoutes will replace them atomically
          // to avoid flickering
          if (typeof window.computeTradeRoutes === 'function') {
            await window.computeTradeRoutes(true);
          }
        }

        // Throttled UI refresh (~12fps for DOM/leaderboard)
        iter++;
        const now = performance.now();
        if (now - lastDrawTime > 80) {
          lastDrawTime = now;
          // Update panel labels from cached values
          for (const e of EmpireManager.empires) {
            if (e._sizeDisplay) e._sizeDisplay.textContent = `Size: ${e._area || 0}`;
            if (e._valueDisplay) e._valueDisplay.textContent = `Land value: ${Math.round(e._value || 0)}`;
          }
          window._drawDirty = true;
          renderLeaderboard();
        }

        // Exit early if nothing changed
        const ownerUnchanged = (window._ownerVersion | 0) === prevOwnerVersion;
        if (!sizeChanged && ownerUnchanged) break;

        // Yield to UI (zero-delay via MessageChannel)
        await yieldToUI();
      }
    } finally {
      btn.dataset.running = '0';
      btn.textContent = originalLabel || 'Adjust size to land value';
      computeEmpireTotals();
      drawFullFrame();
      renderLeaderboard();
    }
  }

  // Recompute only cost maps (no territory changes)
  async function recomputeCostMapsOnly() {
    const grid = window.grid;
    if (!grid || !window.EmpireManager) return;

    const t0 = performance.now();
    await EmpireManager.updateAllCostMaps(grid);
    if (window.perfMonitor) window.perfMonitor.record('costmap', performance.now() - t0);
  }

  // Debounced recompute for UI changes
  window._recomputeFromSlidersRunning = false;
  window._recomputeFromSlidersQueued = false;

  function requestRecomputeFromSliders() {
    const need =
      !!window.currentHeatEmpire ||
      (!!window.currentRouteEmpire && !!window.currentRouteTarget);
    if (!need) return;

    window._recomputeFromSlidersQueued = true;
    scheduleRecomputeFromSliders();
  }

  function scheduleRecomputeFromSliders() {
    if (window._recomputeFromSlidersRunning) return;
    if (!window._recomputeFromSlidersQueued) return;

    window._recomputeFromSlidersQueued = false;
    window._recomputeFromSlidersRunning = true;

    (async () => {
      try {
        await recomputeCostMapsOnly();
        window._heatOverlayDirty = true;
        drawFullFrame();
      } finally {
        window._recomputeFromSlidersRunning = false;
        if (window._recomputeFromSlidersQueued) scheduleRecomputeFromSliders();
      }
    })();
  }

  // Expose to global scope
  window.DEFAULT_TRAVEL_SPEEDS = DEFAULT_TRAVEL_SPEEDS;
  window.TERRAIN_KEYS = TERRAIN_KEYS;
  window.computeGlobalAvgLandValue = computeGlobalAvgLandValue;
  window.computeEmpireTotals = computeEmpireTotals;
  window.renderLeaderboard = renderLeaderboard;
  window.setLeaderboardSort = setLeaderboardSort;
  window.simulateAndDraw = simulateAndDraw;
  window.runRecalibrateWithDynamicSizes = runRecalibrateWithDynamicSizes;
  window.recomputeCostMapsOnly = recomputeCostMapsOnly;
  window.requestRecomputeFromSliders = requestRecomputeFromSliders;

})();
