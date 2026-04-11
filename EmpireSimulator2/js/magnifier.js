// js/magnifier.js - Magnifying glass zoom lens overlay
// Re-renders the scene at zoomed cell size but original border/label sizes
(function() {
  'use strict';

  const ZOOM = 3;
  const LENS_CSS_SIZE = 300;

  let magnifierCanvas = null;
  let magnifierCtx = null;
  let _mouseX = -1;
  let _mouseY = -1;
  let _visible = false;

  function initMagnifier(canvas, ctx) {
    magnifierCanvas = document.createElement('canvas');
    magnifierCtx = magnifierCanvas.getContext('2d');

    canvas.addEventListener('mousemove', e => {
      if (!window.magnifierEnabled) {
        if (_visible) { _visible = false; window._drawDirty = true; }
        return;
      }
      const rect = canvas.getBoundingClientRect();
      _mouseX = (e.clientX - rect.left) * (canvas.width / rect.width);
      _mouseY = (e.clientY - rect.top) * (canvas.height / rect.height);
      _visible = true;
      window._drawDirty = true;
    });

    canvas.addEventListener('mouseleave', () => {
      if (_visible) { _visible = false; window._drawDirty = true; }
    });
  }

  function drawMagnifier(ctx, canvas) {
    if (!_visible || !magnifierCanvas) return;

    const grid = window.grid;
    if (!grid) return;

    const dpr = canvas.width / (canvas.getBoundingClientRect().width || canvas.width);
    const lensSize = Math.round(LENS_CSS_SIZE * dpr);
    const lensRadius = lensSize / 2;

    if (magnifierCanvas.width !== lensSize || magnifierCanvas.height !== lensSize) {
      magnifierCanvas.width = lensSize;
      magnifierCanvas.height = lensSize;
    }

    const mCtx = magnifierCtx;
    const cols = grid.cols;
    const rows = grid.rows;

    // Main canvas cell size (original) and zoomed cell size
    const cellSize = canvas.width / cols;
    const zCell = cellSize * ZOOM;

    // Grid position under mouse (fractional)
    const gridCx = _mouseX / cellSize;
    const gridCy = _mouseY / cellSize;

    // How many grid cells fit in the lens
    const visCells = lensSize / zCell;
    const gridX0 = gridCx - visCells / 2;
    const gridY0 = gridCy - visCells / 2;

    // Visible cell range (with 1-cell margin for borders)
    const minCol = Math.max(0, Math.floor(gridX0) - 1);
    const maxCol = Math.min(cols - 1, Math.ceil(gridX0 + visCells) + 1);
    const minRow = Math.max(0, Math.floor(gridY0) - 1);
    const maxRow = Math.min(rows - 1, Math.ceil(gridY0 + visCells) + 1);

    mCtx.clearRect(0, 0, lensSize, lensSize);

    // Clip to circle
    mCtx.save();
    mCtx.beginPath();
    mCtx.arc(lensRadius, lensRadius, lensRadius, 0, Math.PI * 2);
    mCtx.clip();

    // --- Layer 1: Terrain (pixel-zoom from offscreen buffer) ---
    const offscreen = window._offscreen;
    if (offscreen) {
      const offCellW = offscreen.width / cols;
      const offCellH = offscreen.height / rows;
      const sx = gridX0 * offCellW;
      const sy = gridY0 * offCellH;
      const sw = visCells * offCellW;
      const sh = visCells * offCellH;
      mCtx.imageSmoothingEnabled = false;
      mCtx.drawImage(offscreen, sx, sy, sw, sh, 0, 0, lensSize, lensSize);
    }

    // --- Layer 2: Overlay image ---
    const overlayImage = window.overlayImage;
    if (overlayImage) {
      const overlayOpacity = window.overlayOpacity ?? 0.5;
      // Source region in overlay coords (overlay covers full canvas)
      const oxScale = overlayImage.width || canvas.width;
      const oyScale = overlayImage.height || canvas.height;
      const osx = (gridX0 / cols) * oxScale;
      const osy = (gridY0 / rows) * oyScale;
      const osw = (visCells / cols) * oxScale;
      const osh = (visCells / rows) * oyScale;
      mCtx.save();
      mCtx.globalAlpha = overlayOpacity;
      mCtx.drawImage(overlayImage, osx, osy, osw, osh, 0, 0, lensSize, lensSize);
      mCtx.restore();
    }

    // Helper: convert grid coords to magnifier pixel coords
    const toMx = (col) => (col - gridX0) * zCell;
    const toMy = (row) => (row - gridY0) * zCell;

    // --- Layer 3: Territory fills (zoomed cells, original colors) ---
    const EmpireManager = window.EmpireManager;
    if (EmpireManager && !window.currentHeatEmpire) {
      const terrainBytes = window._terrainCodeFlatCache ||
        (window.getTerrainBytes ? window.getTerrainBytes(grid) : null);
      const WATER_CODE = (window.TERRAIN_CODE || { WATER: 2 }).WATER;

      for (const emp of EmpireManager.empires) {
        const terr = emp.territory;
        if (!terr || terr.size === 0) continue;

        // Land fills
        mCtx.fillStyle = emp.color;
        for (let row = minRow; row <= maxRow; row++) {
          for (let col = minCol; col <= maxCol; col++) {
            const idx = row * cols + col;
            if (!terr.has(idx)) continue;
            if (terrainBytes && terrainBytes[idx] === WATER_CODE) continue;
            mCtx.fillRect(toMx(col), toMy(row), zCell + 0.5, zCell + 0.5);
          }
        }

        // Water stripes
        if (terrainBytes) {
          const base = emp.color.slice(0, 7);
          mCtx.save();
          mCtx.strokeStyle = base + 'A0';
          mCtx.lineWidth = Math.max(1, cellSize * 0.25);
          mCtx.lineCap = 'square';
          mCtx.beginPath();
          for (let row = minRow; row <= maxRow; row++) {
            for (let col = minCol; col <= maxCol; col++) {
              const idx = row * cols + col;
              if (!terr.has(idx)) continue;
              if (terrainBytes[idx] !== WATER_CODE) continue;
              const px = toMx(col), py = toMy(row);
              mCtx.moveTo(px + zCell, py);
              mCtx.lineTo(px, py + zCell);
            }
          }
          mCtx.stroke();
          mCtx.restore();
        }
      }
    }

    // --- Layer 4: Borders (zoomed positions, ORIGINAL line width) ---
    if (EmpireManager) {
      const borderWidth = Math.max(2, cellSize * 0.1);
      const terrainBytes = window._terrainCodeFlatCache ||
        (window.getTerrainBytes ? window.getTerrainBytes(grid) : null);
      const WATER_CODE = (window.TERRAIN_CODE || { WATER: 2 }).WATER;

      mCtx.save();
      mCtx.strokeStyle = '#000';
      mCtx.lineWidth = borderWidth;
      mCtx.lineJoin = 'round';
      mCtx.lineCap = 'round';

      for (const emp of EmpireManager.empires) {
        const terr = emp.territory;
        if (!terr || terr.size === 0) continue;

        mCtx.beginPath();
        for (let row = minRow; row <= maxRow; row++) {
          for (let col = minCol; col <= maxCol; col++) {
            const idx = row * cols + col;
            if (!terr.has(idx)) continue;
            if (terrainBytes && terrainBytes[idx] === WATER_CODE) continue;

            const px = toMx(col), py = toMy(row);
            const pxEnd = px + zCell, pyEnd = py + zCell;

            // Top
            const topIdx = (row - 1) * cols + col;
            if (!terr.has(topIdx) || (terrainBytes && terrainBytes[topIdx] === WATER_CODE)) {
              mCtx.moveTo(px, py); mCtx.lineTo(pxEnd, py);
            }
            // Right
            const rightIdx = row * cols + (col + 1);
            if (!terr.has(rightIdx) || (terrainBytes && terrainBytes[rightIdx] === WATER_CODE)) {
              mCtx.moveTo(pxEnd, py); mCtx.lineTo(pxEnd, pyEnd);
            }
            // Bottom
            const bottomIdx = (row + 1) * cols + col;
            if (!terr.has(bottomIdx) || (terrainBytes && terrainBytes[bottomIdx] === WATER_CODE)) {
              mCtx.moveTo(pxEnd, pyEnd); mCtx.lineTo(px, pyEnd);
            }
            // Left
            const leftIdx = row * cols + (col - 1);
            if (!terr.has(leftIdx) || (terrainBytes && terrainBytes[leftIdx] === WATER_CODE)) {
              mCtx.moveTo(px, pyEnd); mCtx.lineTo(px, py);
            }
          }
        }
        mCtx.stroke();
      }
      mCtx.restore();
    }

    // --- Layer 5: Capitals and labels (ORIGINAL sizes) ---
    if (EmpireManager) {
      const capRadius = cellSize;
      const ghostCapRadius = cellSize * 0.5;
      const capStroke = Math.max(2, Math.ceil(cellSize * 0.05));
      const fontPx = Math.max(12, cellSize * 3.5);
      const hideNames = window.hideEmpireNames;
      const hideDeadNames = window.hideDeadNames;
      const ghostOn = window.ghostCitiesEnabled !== false;
      const hasOutlined = typeof window.drawOutlinedLabel === 'function';

      for (const emp of EmpireManager.empires) {
        if (!emp.capital) continue;
        const isGhost = !emp.territory || emp.territory.size === 0;
        if (isGhost && !ghostOn) continue;

        const { x, y } = emp.capital;
        if (x < gridX0 - 2 || x > gridX0 + visCells + 2) continue;
        if (y < gridY0 - 2 || y > gridY0 + visCells + 2) continue;

        const cx = toMx(x) + zCell / 2;
        const cy = toMy(y) + zCell / 2;
        const r = isGhost ? ghostCapRadius : capRadius;

        if (isGhost) {
          mCtx.beginPath();
          mCtx.arc(cx, cy, ghostCapRadius, 0, Math.PI * 2);
          mCtx.fillStyle = '#000';
          mCtx.fill();
          mCtx.lineWidth = Math.max(1, capStroke * 0.5);
          mCtx.strokeStyle = '#444';
          mCtx.stroke();
        } else {
          mCtx.beginPath();
          mCtx.arc(cx, cy, capRadius, 0, Math.PI * 2);
          mCtx.fillStyle = '#fff';
          mCtx.fill();
          mCtx.lineWidth = capStroke;
          mCtx.strokeStyle = '#000';
          mCtx.stroke();
        }

        if (!hideNames && !(isGhost && hideDeadNames)) {
          const labelX = cx + r + 6;
          if (hasOutlined) {
            if (isGhost) mCtx.globalAlpha = 0.5;
            window.drawOutlinedLabel(mCtx, emp.name, labelX, cy, fontPx, 'left');
            mCtx.globalAlpha = 1.0;
          } else {
            mCtx.fillStyle = isGhost ? '#888' : '#FFF';
            mCtx.font = `${Math.ceil(fontPx)}px sans-serif`;
            mCtx.textBaseline = 'middle';
            mCtx.textAlign = 'left';
            mCtx.fillText(emp.name, labelX, cy);
          }
        }
      }
    }

    mCtx.restore(); // end circle clip

    // --- Blit lens onto main canvas ---
    const dx = _mouseX - lensRadius;
    const dy = _mouseY - lensRadius;

    ctx.save();
    ctx.drawImage(magnifierCanvas, dx, dy);

    // Border ring
    ctx.beginPath();
    ctx.arc(_mouseX, _mouseY, lensRadius, 0, Math.PI * 2);
    ctx.lineWidth = Math.max(2, 3 * dpr);
    ctx.strokeStyle = 'rgba(255,255,255,0.8)';
    ctx.stroke();
    ctx.lineWidth = Math.max(1, 1.5 * dpr);
    ctx.strokeStyle = 'rgba(0,0,0,0.5)';
    ctx.stroke();

    // Crosshair
    const ch = Math.max(4, 6 * dpr);
    ctx.strokeStyle = 'rgba(255,255,255,0.6)';
    ctx.lineWidth = Math.max(1, dpr);
    ctx.beginPath();
    ctx.moveTo(_mouseX - ch, _mouseY);
    ctx.lineTo(_mouseX + ch, _mouseY);
    ctx.moveTo(_mouseX, _mouseY - ch);
    ctx.lineTo(_mouseX, _mouseY + ch);
    ctx.stroke();

    ctx.restore();
  }

  window.initMagnifier = initMagnifier;
  window.drawMagnifier = drawMagnifier;

})();
