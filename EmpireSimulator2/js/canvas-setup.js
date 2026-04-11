// js/canvas-setup.js - Canvas initialization and resize handling
(function() {
  'use strict';

  // These will be set on window.addEventListener('load')
  let canvas, ctx, offscreen, offCtx, tradeLayer, tradeCtx;
  let resizer, canvasSizeSlider, canvasSizeDisplay;

  function initCanvas() {
    // --- Cache DOM elements ---
    canvas = document.getElementById('mapCanvas');
    ctx = canvas.getContext('2d');

    canvasSizeSlider = document.getElementById('canvas-size-slider');
    canvasSizeDisplay = document.getElementById('canvas-size-display');
    resizer = document.getElementById('canvas-resizer');

    // Offscreen buffer for background rendering
    offscreen = document.createElement('canvas');
    offCtx = offscreen.getContext('2d');

    // Trade overlay cache (draw once, blit every frame)
    tradeLayer = document.createElement('canvas');
    tradeCtx = tradeLayer.getContext('2d');

    // Heatmap overlay cache (draw once, blit every frame)
    const heatLayer = document.createElement('canvas');
    const heatCtx = heatLayer.getContext('2d');

    // Store references globally
    window._canvas = canvas;
    window._ctx = ctx;
    window._offscreen = offscreen;
    window._offCtx = offCtx;
    window._tradeLayer = tradeLayer;
    window._tradeCtx = tradeCtx;
    window._heatLayer = heatLayer;
    window._heatCtx = heatCtx;
    window._resizer = resizer;

    return { canvas, ctx, offscreen, offCtx, tradeLayer, tradeCtx, resizer };
  }

  // Helper: redraw cached trade overlay when needed
  function redrawTradeOverlay() {
    if (!window._tradeLayer || !window._tradeCtx) return;
    const tc = window._tradeCtx;
    const tl = window._tradeLayer;

    // If routes are being recomputed and we have nothing to draw, skip
    // This prevents flickering by keeping old content visible
    if (!window.tradeRoutesPath) {
      // Mark as not dirty so we don't keep trying
      window.tradeOverlayDirty = false;
      return;
    }

    tc.clearRect(0, 0, tl.width, tl.height);

    const grid = window.grid;
    const emps = window.tradeEmpires || [];
    const out = window.tradeOutDegree || null;

    // 1) Routes
    if (window.tradeRoutesPath) {
      tc.save();
      tc.lineWidth = 2.5;
      tc.strokeStyle = 'rgba(255,0,0,0.65)';
      tc.stroke(window.tradeRoutesPath);
      tc.restore();
    }

    // 2) City markers (scaled by outgoing route count)
    if (grid && emps.length && out && out.length === emps.length) {
      const cols = grid.cols, rows = grid.rows;
      const cellW = tl.width / cols;
      const cellH = tl.height / rows;
      const cell = Math.min(cellW, cellH);

      tc.save();
      tc.fillStyle = 'rgba(255,0,0,0.5)';
      tc.strokeStyle = 'rgba(255,0,0,0.65)';
      tc.lineWidth = Math.max(1, cell * 0.12);

      for (let i = 0; i < emps.length; i++) {
        const e = emps[i];
        if (!e || !e.capital) continue;

        const k = out[i] | 0;

        // base ring slightly larger than the normal capital marker
        let r = cell * (1.10 + 0.25 * Math.sqrt(Math.max(0, k)));
        r = Math.max(cell * 1.10, Math.min(cell * 3.50, r));

        const cx = (e.capital.x + 0.5) * cellW;
        const cy = (e.capital.y + 0.5) * cellH;

        tc.beginPath();
        tc.arc(cx, cy, r, 0, Math.PI * 2);
        tc.fill();
        tc.stroke();
      }

      tc.restore();
    }

    window.tradeOverlayDirty = false;
  }

  function resizeCanvases() {
    const canvas = window._canvas;
    const offscreen = window._offscreen;

    if (!canvas || !offscreen) return;

    // Canvas bitmap size is set elsewhere (applyCanvasSizeFromWrapper).
    // Here we only keep the offscreen buffer in sync with the current canvas size.
    offscreen.width = canvas.width;
    offscreen.height = canvas.height;

    // Trade cache canvas must match bitmap size too
    if (window._tradeLayer) {
      window._tradeLayer.width = canvas.width;
      window._tradeLayer.height = canvas.height;
    }

    // Heatmap cache canvas must match bitmap size too
    if (window._heatLayer) {
      window._heatLayer.width = canvas.width;
      window._heatLayer.height = canvas.height;
      window._heatOverlayDirty = true;
    }

    // Trade overlay depends on pixel geometry - old Path2D objects are invalid
    // for the new canvas size, so we must null them
    window.tradeRoutesPath = null;
    window.tradeOverlayDirty = true;

    // Skip trade recomputation during terrain import to avoid freeze
    if (window._terrainImportInProgress) return;

    // IMPORTANT: don't recompute routes immediately on every resize tick.
    // Debounce it a bit so drag-resizing doesn't melt the CPU.
    if (window.tradeView) {
      clearTimeout(window._tradeResizeTimer);
      window._tradeResizeTimer = setTimeout(() => window.computeTradeRoutes?.(true), 150);
    }
  }

  // === Resizable wrapper → resize canvas bitmap only (no grid/rows/cols changes) ===
  function initCanvasResizer() {
    const resizer = window._resizer;
    const canvas = window._canvas;

    if (!resizer || !canvas) return;

    // Keep the wrapper's height matching current grid aspect (rows/cols)
    // Always use window.grid to get the current grid (not a stale reference)
    function heightFromWidth(w) {
      const grid = window.grid;
      if (!grid) return w; // fallback to square if no grid
      const aspect = grid.rows / grid.cols;
      return Math.max(1, Math.round(w * aspect));
    }

    // Sync canvas bitmap to wrapper width; do NOT change grid rows/cols
    function applyCanvasSizeFromWrapper(cssW) {
      const w = Math.max(100, Math.round(cssW));
      const h = heightFromWidth(w);

      // Snap and lock the wrapper's height so it cannot be dragged independently
      resizer.style.height = h + 'px';
      resizer.style.minHeight = h + 'px';
      resizer.style.maxHeight = h + 'px';

      // Cap DPR and size bitmap accordingly (keep CSS size unchanged)
      const dpr = window.getEffectiveDPR ? window.getEffectiveDPR() : Math.min(window.devicePixelRatio || 1, 1.5);
      canvas.style.width = w + 'px';
      canvas.style.height = h + 'px';
      canvas.width = Math.round(w * dpr);
      canvas.height = Math.round(h * dpr);

      // Offscreen + redraw
      resizeCanvases();
      if (typeof window.renderBackground === 'function') window.renderBackground();
      if (typeof window.drawCurrent === 'function') window.drawCurrent();

      // Optional UI mirrors (safe even if removed in HTML)
      if (canvasSizeDisplay) {
        canvasSizeDisplay.textContent = String(w);
      }
      if (canvasSizeSlider) {
        canvasSizeSlider.value = String(w);
      }
    }

    // Expose helpers so other code (terrain load / grid resize) can re-lock height
    window.__resizerApplyFromWidth = function(w) {
      try { applyCanvasSizeFromWrapper(Math.round(w)); } catch (e) {}
    };
    window.__resizerSnapToCurrent = function() {
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
  }

  // Expose to global scope
  window.initCanvas = initCanvas;
  window.redrawTradeOverlay = redrawTradeOverlay;
  window.resizeCanvases = resizeCanvases;
  window.initCanvasResizer = initCanvasResizer;

})();
