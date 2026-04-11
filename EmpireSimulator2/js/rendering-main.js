// js/rendering-main.js - Main rendering functions (drawCurrent, buildBorderPath, etc.)
(function() {
  'use strict';

  // --- Variant Grid for color variation ---
  function generateVariantGrid() {
    const grid = window.grid;
    if (!grid) return;

    window.variantGrid = Array.from({ length: grid.rows }, () =>
      Array.from({ length: grid.cols }, () => 0)
    );
    for (let y = 0; y < grid.rows; y++) {
      for (let x = 0; x < grid.cols; x++) {
        const t = grid.cells[y][x].terrain;
        const variants = window.TERRAIN_VARIANTS ? (window.TERRAIN_VARIANTS[t] || [window.TERRAIN[t].color]) : [window.TERRAIN[t].color];
        window.variantGrid[y][x] = Math.floor(Math.random() * variants.length);
      }
    }
  }

  // Render static background into offscreen
  function renderBackground() {
    const offscreen = window._offscreen;
    const offCtx = window._offCtx;
    const grid = window.grid;

    if (!offscreen || !offCtx || !grid) return;

    offCtx.clearRect(0, 0, offscreen.width, offscreen.height);
    const cellSize = offscreen.width / grid.cols;
    const showGrid = window.showGrid || false;

    if (window.renderMode === 'value' && typeof window.drawValueGrid === 'function') {
      window.drawValueGrid(offCtx, grid, cellSize, showGrid);
    } else {
      if (typeof window.drawGrid === 'function') {
        window.drawGrid(offCtx, grid, cellSize, showGrid);
      }
    }
  }

  // Build cached border path for an empire (LAND CELLS ONLY)
  function buildBorderPath(emp, cols, cellSize) {
    const terr = emp.territory;
    if (!terr || terr.size === 0) return new Path2D();

    const path = new Path2D();
    const grid = window.grid;

    // Get terrain bytes for fast lookup
    const terrainBytes = window._terrainCodeFlatCache || (window.getTerrainBytes ? window.getTerrainBytes(grid) : null);
    const WATER_CODE = window.TERRAIN_CODE?.WATER ?? 2;
    const hasTerrainBytes = !!terrainBytes;

    // Use for...of instead of forEach for performance
    for (const idx of terr) {
      // Skip water cells entirely for border drawing
      if (hasTerrainBytes && terrainBytes[idx] === WATER_CODE) continue;

      const x = idx % cols;
      const y = (idx / cols) | 0;
      const px = x * cellSize, py = y * cellSize;
      const pxEnd = px + cellSize, pyEnd = py + cellSize;

      // Inline neighbor checks to avoid function creation overhead
      // top neighbor
      const topIdx = (y - 1) * cols + x;
      if (!terr.has(topIdx) || (hasTerrainBytes && terrainBytes[topIdx] === WATER_CODE)) {
        path.moveTo(px, py);
        path.lineTo(pxEnd, py);
      }
      // right neighbor
      const rightIdx = y * cols + (x + 1);
      if (!terr.has(rightIdx) || (hasTerrainBytes && terrainBytes[rightIdx] === WATER_CODE)) {
        path.moveTo(pxEnd, py);
        path.lineTo(pxEnd, pyEnd);
      }
      // bottom neighbor
      const bottomIdx = (y + 1) * cols + x;
      if (!terr.has(bottomIdx) || (hasTerrainBytes && terrainBytes[bottomIdx] === WATER_CODE)) {
        path.moveTo(pxEnd, pyEnd);
        path.lineTo(px, pyEnd);
      }
      // left neighbor
      const leftIdx = y * cols + (x - 1);
      if (!terr.has(leftIdx) || (hasTerrainBytes && terrainBytes[leftIdx] === WATER_CODE)) {
        path.moveTo(px, pyEnd);
        path.lineTo(px, py);
      }
    }

    return path;
  }

  function drawCurrent() {
    const canvas = window._canvas;
    const ctx = window._ctx;
    const offscreen = window._offscreen;
    const grid = window.grid;

    if (!canvas || !ctx || !offscreen || !grid) return;

    const cellSize = canvas.width / grid.cols;

    // 1) draw the cached background (terrain OR value) from offscreen
    ctx.drawImage(offscreen, 0, 0);

    // Overlay image
    const overlayImage = window.overlayImage;
    const overlayOpacity = window.overlayOpacity ?? 0.5;
    if (overlayImage) {
      ctx.save();
      ctx.globalAlpha = overlayOpacity;
      ctx.drawImage(overlayImage, 0, 0, canvas.width, canvas.height);
      ctx.restore();
    }

    // draw territories (fills) only when *not* showing a heatmap
    if (!window.currentHeatEmpire && window.EmpireManager) {
      const cols = grid.cols;
      const ownerVer = window._ownerVersion || 0;

      // Cached territory fill + water stripe Path2D per empire
      for (const emp of EmpireManager.empires) {
        const terr = emp.territory;
        if (!terr || terr.size === 0) continue;

        // Rebuild fill/water paths only when ownership or cellSize changes
        if (emp._fillVersion !== ownerVer || emp._fillCellSize !== cellSize) {
          const terrainBytes = window.getTerrainBytes ? window.getTerrainBytes(grid) : null;
          const WATER_CODE = (window.TERRAIN_CODE || { WATER: 2 }).WATER;
          const hasTerrainBytes = !!terrainBytes;

          const fillPath = new Path2D();
          const waterPath = new Path2D();
          let hasWater = false;
          for (const idx of terr) {
            const x = idx % cols;
            const y = (idx / cols) | 0;
            const px = x * cellSize;
            const py = y * cellSize;
            if (hasTerrainBytes && terrainBytes[idx] === WATER_CODE) {
              waterPath.moveTo(px + cellSize, py);
              waterPath.lineTo(px, py + cellSize);
              hasWater = true;
            } else {
              fillPath.rect(px, py, cellSize, cellSize);
            }
          }
          emp._fillPath = fillPath;
          emp._waterStripePath = waterPath;
          emp._hasWaterStripes = hasWater;
          emp._fillVersion = ownerVer;
          emp._fillCellSize = cellSize;
        }

        // Land fills
        ctx.fillStyle = emp.color;
        ctx.fill(emp._fillPath);

        // Water stripes
        if (emp._hasWaterStripes) {
          const base = emp.color.slice(0, 7);
          ctx.save();
          ctx.strokeStyle = base + 'A0';
          ctx.lineWidth = Math.max(1, cellSize * 0.25);
          ctx.lineCap = 'square';
          ctx.stroke(emp._waterStripePath);
          ctx.restore();
        }
      }
    }

    // draw thick border around each empire's territory (cached Path2D)
    if (window.EmpireManager) {
      const ownerVersion = window._ownerVersion || 0;
      const cols = grid.cols;
      const borderWidth = Math.max(2, cellSize * 0.1);

      // Set border styles once
      ctx.save();
      ctx.strokeStyle = '#000';
      ctx.lineWidth = borderWidth;
      ctx.lineJoin = 'round';
      ctx.lineCap = 'round';

      for (const emp of EmpireManager.empires) {
        const terr = emp.territory;
        if (!terr || terr.size === 0) continue;

        // Rebuild border path only if ownership changed
        if (emp._borderVersion !== ownerVersion) {
          emp._borderPath = buildBorderPath(emp, cols, cellSize);
          emp._borderVersion = ownerVersion;
        }

        ctx.stroke(emp._borderPath);
      }
      ctx.restore();

      // draw capitals
      const halfCell = cellSize / 2;
      const radius = cellSize;
      const ghostRadius = cellSize * 0.5; // smaller for ghost cities
      const capStrokeWidth = Math.max(2, Math.ceil(cellSize * 0.05));
      const fontPx = Math.max(12, cellSize * 3.5);
      const hideNames = window.hideEmpireNames;
      const hasOutlinedLabel = typeof window.drawOutlinedLabel === 'function';
      const ghostCitiesEnabled = window.ghostCitiesEnabled !== false; // default true

      for (const emp of EmpireManager.empires) {
        if (!emp.capital) continue;

        const isGhost = !emp.territory || emp.territory.size === 0;

        // Skip ghost cities if the feature is disabled
        if (isGhost && !ghostCitiesEnabled) continue;

        const { x, y } = emp.capital;
        const cx = x * cellSize + halfCell;
        const cy = y * cellSize + halfCell;

        if (isGhost) {
          // Ghost capital: small black filled circle
          ctx.beginPath();
          ctx.arc(cx, cy, ghostRadius, 0, Math.PI * 2);
          ctx.fillStyle = '#000';
          ctx.fill();
          ctx.lineWidth = Math.max(1, capStrokeWidth * 0.5);
          ctx.strokeStyle = '#444';
          ctx.stroke();
        } else {
          // Normal capital marker
          ctx.beginPath();
          ctx.arc(cx, cy, radius, 0, Math.PI * 2);
          ctx.fillStyle = '#fff';
          ctx.fill();
          ctx.lineWidth = capStrokeWidth;
          ctx.strokeStyle = '#000';
          ctx.stroke();
        }

        // name label (outlined) - show for both, but dimmer for ghosts
        if (!hideNames && !(isGhost && window.hideDeadNames)) {
          const labelX = cx + (isGhost ? ghostRadius : radius) + 6;
          if (hasOutlinedLabel) {
            if (isGhost) {
              ctx.globalAlpha = 0.5;
            }
            window.drawOutlinedLabel(ctx, emp.name, labelX, cy, fontPx, 'left');
            ctx.globalAlpha = 1.0;
          } else {
            ctx.fillStyle = isGhost ? '#888' : '#FFF';
            ctx.font = `${Math.ceil(fontPx)}px sans-serif`;
            ctx.textBaseline = 'middle';
            ctx.textAlign = 'left';
            ctx.fillText(emp.name, labelX, cy);
          }
        }
      }
    }

  }

  // Wrap drawCurrent with performance timing
  function drawCurrentWithTiming() {
    const t0 = performance.now();
    drawCurrent();
    const elapsed = performance.now() - t0;
    if (window.perfMonitor) {
      window.perfMonitor.record('render', elapsed);
    }
  }

  // Sets dirty flag so main render loop handles it
  function requestDraw() {
    window._drawDirty = true;
  }

  // Expose to global scope
  window.generateVariantGrid = generateVariantGrid;
  window.renderBackground = renderBackground;
  window.drawCurrent = drawCurrentWithTiming;
  window.requestDraw = requestDraw;

})();
