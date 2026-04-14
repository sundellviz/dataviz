// js/main.js - Orchestration and initialization
// All functionality has been split into modular files:
// - state.js: Shared state object
// - pathfinding.js: Worker pool, cost maps
// - performance.js: Performance monitoring
// - canvas-setup.js: Canvas initialization, resize
// - painting.js: Paint mode, brushes
// - rendering-main.js: drawCurrent, borders
// - import-export.js: All I/O functions
// - simulation.js: Totals, simulation loop
// - auction.js: Global auction algorithm
// - ui-controls.js: All UI handlers

(function() {
  'use strict';

  window.addEventListener('load', () => {
    // Initialize canvas and offscreen buffers
    const { canvas, ctx, offscreen, offCtx, tradeLayer, tradeCtx } = window.initCanvas();

    // Create initial grid
    const gridWidthSlider = document.getElementById('grid-width-slider');
    const gridHeightSlider = document.getElementById('grid-height-slider');
    const gridWidth = parseInt(gridWidthSlider?.value || '50', 10);
    const gridHeight = parseInt(gridHeightSlider?.value || '50', 10);

    window.gridWidth = gridWidth;
    window.gridHeight = gridHeight;

    const grid = new window.Grid(gridWidth, gridHeight);
    window.grid = grid;
    grid.initValueLayer(11);

    // Initialize canvas resizer with grid reference
    window.initCanvasResizer(grid);

    // Initialize painting controls
    if (typeof window.initPaintingControls === 'function') {
      window.initPaintingControls();
    }

    // Initialize canvas paint events
    if (typeof window.initCanvasPaintEvents === 'function') {
      window.initCanvasPaintEvents(canvas);
    }

    // Initialize magnifying glass
    if (typeof window.initMagnifier === 'function') {
      window.initMagnifier(canvas, ctx);
    }

    // Initialize empire UI (creates the panels container and Add Empire button)
    if (typeof window.initEmpireUI === 'function') {
      window.initEmpireUI();
    }

    // Initialize UI controls
    if (typeof window.initUIControls === 'function') {
      window.initUIControls();
    }

    // Initialize import/export functionality
    if (typeof window.initTerrainMenu === 'function') window.initTerrainMenu();
    if (typeof window.initValueMenu === 'function') window.initValueMenu();
    if (typeof window.initOverlayMenu === 'function') window.initOverlayMenu();
    if (typeof window.initCitiesMenu === 'function') window.initCitiesMenu();
    if (typeof window.initImportExportButtons === 'function') window.initImportExportButtons();

    // Auto-load Europe terrain on startup
    const terrainMenu = document.getElementById('terrain-menu');
    if (terrainMenu) {
      terrainMenu.value = 'europe.txt';
      terrainMenu.dispatchEvent(new Event('change'));
    }

    // Setup tooltip for info mode
    const tooltip = document.createElement('div');
    tooltip.style.position = 'absolute';
    tooltip.style.pointerEvents = 'none';
    tooltip.style.padding = '4px 8px';
    tooltip.style.background = 'rgba(0,0,0,0.7)';
    tooltip.style.color = '#fff';
    tooltip.style.borderRadius = '4px';
    tooltip.style.fontSize = '12px';
    tooltip.style.transition = 'opacity 0.1s';
    tooltip.style.opacity = '0';
    tooltip.style.whiteSpace = 'nowrap';
    document.body.appendChild(tooltip);

    // Info mode helpers
    let _lastInfoIdx = -1;
    let _lastTop3HTML = '';
    let _lastAuctionHTML = '';

    function findOwner(idx) {
      for (const emp of EmpireManager.empires) {
        if (emp.territory.has(idx)) return emp.name;
      }
      return 'None';
    }

    function topThreeCheapestAt(idx) {
      const out = [];
      for (const e of EmpireManager.empires) {
        const A = e.costMapFlat;
        if (!A || A.length === 0) continue;
        const d = A[idx];
        if (!(d < Infinity)) continue;
        out.push({ id: e.id, name: e.name || `Empire ${e.id}`, cost: d });
      }
      out.sort((a, b) => (a.cost - b.cost) || (a.id - b.id));
      return out.slice(0, 3);
    }

    function auctionDebugAt(idx, grid) {
      if (!window.EmpireManager) return '';
      const a = window._lastAuction;
      if (!a) return '<em>No auction data yet</em>';

      const { topKId, topKCost, topKFrontier, topKLen, P_byId, globalPMax, K, N, w, B } = a;
      if (idx < 0 || idx >= N) return '';

      const len = topKLen[idx] | 0;
      if (len === 0) return '<em>Not eligible (no candidates)</em>';

      const off = idx * K;
      const pScale = (globalPMax > 0) ? (w / globalPMax) : 0;

      // Build empire id→name lookup
      const emps = EmpireManager.empires;
      const nameById = {};
      for (const e of emps) nameById[e.id] = e.name || `E${e.id}`;

      // Read actual topK candidates, compute effective cost
      const bidders = [];
      for (let j = 0; j < len; j++) {
        const id = topKId[off + j] | 0;
        const cost = topKCost[off + j] || 0;
        const power = P_byId[id] || 0;
        const isFrontier = !!topKFrontier[off + j];
        const discount = 1 - pScale * power;
        const ec = (cost + B) * discount;
        bidders.push({ id, name: nameById[id] || `E${id}`, cost, power, discount, ec, isFrontier });
      }
      bidders.sort((a, b) => a.ec - b.ec);

      let h = `<strong>Auction (w=${w.toFixed(2)} B=${B}):</strong> gPMax=${globalPMax.toFixed(1)} [${len} cand.]<br/>`;
      for (const b of bidders) {
        const win = b === bidders[0];
        h += `${win ? '▶ ' : '&nbsp;&nbsp;'}${b.name}: `;
        h += `cost=${b.cost.toFixed(1)}`;
        if (B > 0) h += `+${B}`;
        h += ` pwr=${b.power.toFixed(1)} `;
        h += `x${b.discount.toFixed(2)} `;
        h += `= <b>${b.ec.toFixed(1)}</b>`;
        if (!b.isFrontier) h += ' <span style="color:#ff0">[wr]</span>';
        h += '<br/>';
      }
      return h;
    }

    function heatPercentAt(col, row) {
      const info = window.__heatRank;
      const emp = window.currentHeatEmpire;
      const currentGrid = window.grid;
      if (!info || !emp || info.empId !== emp.id || !currentGrid) return null;

      const idx = row * currentGrid.cols + col;
      const owner = findOwner(idx);
      if (owner !== 'None' && owner !== emp.name) return null;

      const t = info.rank[idx];
      if (t == null || t < 0) return null;

      return Math.round(t * 100);
    }

    // Mouse hover info
    canvas.addEventListener('mousemove', e => {
      // IMPORTANT: Use window.grid instead of closure variable
      const currentGrid = window.grid;
      if (!currentGrid) return;

      const rect = canvas.getBoundingClientRect();
      const px = (e.clientX - rect.left) * (canvas.width / rect.width);
      const py = (e.clientY - rect.top) * (canvas.height / rect.height);
      const cellW = canvas.width / currentGrid.cols;
      const cellH = canvas.height / currentGrid.rows;
      const col = Math.floor(px / cellW);
      const row = Math.floor(py / cellH);

      // Check if hovering over a capital - change cursor
      let overCapital = false;
      if (window.EmpireManager && !window.currentMode) {
        const hoverRadius = cellW * 1.2;
        for (const emp of window.EmpireManager.empires) {
          if (!emp.capital) continue;
          const capCx = (emp.capital.x + 0.5) * cellW;
          const capCy = (emp.capital.y + 0.5) * cellH;
          const dist = Math.hypot(px - capCx, py - capCy);
          if (dist <= hoverRadius) {
            overCapital = true;
            break;
          }
        }
      }
      canvas.style.cursor = overCapital ? 'pointer' : '';

      if (col < 0 || col >= currentGrid.cols || row < 0 || row >= currentGrid.rows) {
        tooltip.style.opacity = '0';
        return;
      }

      if (!window.infoMode) {
        tooltip.style.opacity = '0';
        return;
      }

      const idx = row * currentGrid.cols + col;
      const terr = currentGrid.cells[row][col].terrain;
      const owner = findOwner(idx);
      const pct = heatPercentAt(col, row);

      const landVal = (typeof currentGrid.getValueAt === 'function')
        ? currentGrid.getValueAt(col, row)
        : Number(currentGrid.cells[row][col].value ?? currentGrid.cells[row][col].landValue ?? 0);
      const valChar = (typeof window.valToCharLocal === 'function') ? ` (${window.valToCharLocal(landVal)})` : '';

      let topHTML = '';
      let auctionHTML = '';
      if (idx !== _lastInfoIdx) {
        const top3 = topThreeCheapestAt(idx);
        topHTML = top3.map((t, i) => `${i + 1}) ${t.name} (${t.cost.toFixed(2)})`).join('<br/>');
        auctionHTML = auctionDebugAt(idx, currentGrid);
        _lastInfoIdx = idx;
        _lastTop3HTML = topHTML;
        _lastAuctionHTML = auctionHTML;
      } else {
        topHTML = _lastTop3HTML;
        auctionHTML = _lastAuctionHTML;
      }

      tooltip.innerHTML = `
        <strong>Owner:</strong> ${owner}<br/>
        <strong>Terrain:</strong> ${terr}<br/>
        <strong>Land value:</strong> ${landVal}${valChar}${
          (pct == null ? '' : `<br/><strong>Heatmap:</strong> ${pct.toFixed(0)}%`)
        }${
          (topHTML ? `<br/><strong>Cheapest:</strong><br/>${topHTML}` : '')
        }${
          (auctionHTML ? `<br/><hr style="margin:4px 0;border-color:#555">${auctionHTML}` : '')
        }
      `;

      tooltip.style.left = e.pageX + 10 + 'px';
      tooltip.style.top = e.pageY + 10 + 'px';
      tooltip.style.opacity = '1';
    });

    canvas.addEventListener('mouseleave', () => {
      tooltip.style.opacity = '0';
    });

    // Canvas click handler for capital placement and route finding
    canvas.addEventListener('click', e => {
      // IMPORTANT: Use window.grid instead of closure variable
      // because grid can be replaced when loading terrain maps
      const currentGrid = window.grid;
      if (!currentGrid) return;

      const rect = canvas.getBoundingClientRect();
      const scaleX = canvas.width / rect.width;
      const scaleY = canvas.height / rect.height;
      const cx = (e.clientX - rect.left) * scaleX;
      const cy = (e.clientY - rect.top) * scaleY;
      const cellW = canvas.width / currentGrid.cols;
      const cellH = canvas.height / currentGrid.rows;
      const x = Math.floor(cx / cellW);
      const y = Math.floor(cy / cellH);

      // Route-finding mode
      if (window.currentMode === 'findRoute' && window.pendingRouteEmpire) {
        window.currentRouteEmpire = window.pendingRouteEmpire;
        window.currentRouteTarget = { x, y };
        window.pendingRouteEmpire = null;
        window.currentMode = null;

        (async () => {
          if (typeof window.recomputeCostMapsOnly === 'function') {
            await window.recomputeCostMapsOnly();
          }
          // Trigger full render loop (includes route overlay)
          if (typeof window.requestDraw === 'function') {
            window.requestDraw();
          } else {
            window._drawDirty = true;
          }
        })();

        return;
      }

      // Capital-placement mode
      if (window.currentMode === 'placeCapital' && window.currentEmpire) {
        const emp = window.currentEmpire;
        emp.capital = { x, y };

        // Immediately claim the capital cell so it shows up right away
        if (!emp.territory) emp.territory = new Set();
        emp.territory.clear();
        emp.territory.add(y * currentGrid.cols + x);

        if (emp._capitalDisplay) {
          emp._capitalDisplay.textContent = `Capital: (${x},${y})`;
        }

        window.currentEmpire = null;
        window.currentMode = null;

        // Mark cost map as dirty so it gets recomputed
        emp._costMapDirty = true;

        if (typeof window.drawCurrent === 'function') window.drawCurrent();
        return;
      }

      // No special mode - check if user clicked on a capital to open empire panel
      if (!window.currentMode && window.EmpireManager) {
        const clickRadius = cellW * 1.5; // Detection radius around capital
        for (const emp of window.EmpireManager.empires) {
          if (!emp.capital) continue;
          const capCx = (emp.capital.x + 0.5) * cellW;
          const capCy = (emp.capital.y + 0.5) * cellH;
          const dist = Math.hypot(cx - capCx, cy - capCy);
          if (dist <= clickRadius) {
            // Found a capital - switch to Empires tab and scroll to panel
            window.openEmpirePanel?.(emp.id);
            return;
          }
        }
      }
    });

    // Initial render
    if (typeof window.generateVariantGrid === 'function') window.generateVariantGrid();
    if (typeof window.resizeCanvases === 'function') window.resizeCanvases();
    if (typeof window.precomputeWaterShading === 'function') {
      window.precomputeWaterShading(grid, offscreen.width, offscreen.height);
    }
    if (typeof window.renderBackground === 'function') window.renderBackground();

    // Initial simulation
    if (typeof window.simulateAndDraw === 'function') window.simulateAndDraw();

    // Draw dirty flag for render loop
    window._drawDirty = true;

    // Main render loop
    (function renderLoop() {
      if (window._drawDirty) {
        window._drawDirty = false;

        // Base map (terrain + grid + territory + capitals)
        if (typeof window.drawCurrent === 'function') window.drawCurrent();

        // Trade overlay (cached)
        if (window.tradeView) {
          if (window.tradeOverlayDirty && typeof window.redrawTradeOverlay === 'function') {
            window.redrawTradeOverlay();
          }
          if (window._tradeLayer) ctx.drawImage(window._tradeLayer, 0, 0);
        }

        // Heatmap overlay (cached to offscreen canvas)
        if (window.currentHeatEmpire && typeof window.drawHeatmap === 'function') {
          window.drawHeatmap(window.currentHeatEmpire);
          if (window._heatLayer) {
            ctx.save();
            ctx.globalAlpha = 0.5;
            ctx.drawImage(window._heatLayer, 0, 0);
            ctx.restore();
          }
        }

        // Route overlay
        if (window.currentRouteEmpire && window.currentRouteTarget && typeof window.drawRoute === 'function') {
          window.drawRoute(
            window.currentRouteEmpire,
            window.currentRouteTarget.x,
            window.currentRouteTarget.y
          );
        }

        // Magnifying glass (must be last - reads from fully composited canvas)
        if (window.magnifierEnabled && typeof window.drawMagnifier === 'function') {
          window.drawMagnifier(ctx, canvas);
        }
      }

      requestAnimationFrame(renderLoop);
    })();
  });

})();
