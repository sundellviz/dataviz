// js/painting.js - Paint mode, brush handling, value painting
(function() {
  'use strict';

  // Paint state
  let painting = false;
  let paintType = 'PLAIN';
  let brushSize = 1;

  // Global paint mode (no radios anymore)
  function setPaintMode(mode) {
    window.paintMode = mode === 'value' ? 'value' : 'terrain';
    const controls = document.getElementById('value-paint-controls');
    if (controls) controls.style.display = (window.paintMode === 'value') ? 'flex' : 'none';
  }

  function currentPaintTarget() {
    return window.paintMode || 'terrain';
  }

  // Value to character conversion for display
  function valToCharLocal(v) {
    v = Math.max(0, Math.min(61, Math.floor(v)));
    if (v <= 9) return String.fromCharCode(48 + v);
    if (v <= 35) return String.fromCharCode(97 + (v - 10));
    return String.fromCharCode(65 + (v - 36));
  }

  // Initialize painting controls
  function initPaintingControls() {
    const paintSelect = document.getElementById('paint-select');
    const paintModeBtn = document.getElementById('paint-mode-btn');
    const brushSizeSlider = document.getElementById('brush-size-slider');
    const brushSizeDisplay = document.getElementById('brush-size-display');
    const paintHeader = document.getElementById('paint-header');
    const paintControls = document.getElementById('paint-controls');
    const paintValueInput = document.getElementById('paint-value');
    const paintValueChar = document.getElementById('paint-value-char');
    const valuePaintControls = document.getElementById('value-paint-controls');
    const valueBrushSlider = document.getElementById('value-brush-size');
    const valueBrushDisplay = document.getElementById('value-brush-size-display');

    // Paint select
    if (paintSelect) {
      paintType = paintSelect.value;
      paintSelect.addEventListener('change', () => {
        paintType = paintSelect.value;
      });
    }

    // Paint mode toggle button
    if (paintModeBtn) {
      paintModeBtn.addEventListener('click', () => {
        const paintMode = window.currentMode === 'paint';
        window.currentMode = paintMode ? null : 'paint';
        paintModeBtn.textContent = window.currentMode === 'paint'
          ? 'Stop Terrain Painting'
          : 'Start Terrain Painting';
      });
    }

    // Brush size slider
    if (brushSizeSlider) {
      brushSize = parseInt(brushSizeSlider.value, 10);
      brushSizeSlider.addEventListener('input', () => {
        brushSize = parseInt(brushSizeSlider.value, 10);
        if (brushSizeDisplay) brushSizeDisplay.textContent = brushSize;
      });
    }

    // Paint header collapsible
    if (paintHeader && paintControls) {
      paintHeader.addEventListener('click', () => {
        const hidden = paintControls.classList.toggle('hidden');
        paintHeader.textContent = hidden
          ? 'Paint terrain ▶'
          : 'Paint terrain ▼';
      });
    }

    // Value paint input
    if (paintValueInput && paintValueChar) {
      paintValueInput.addEventListener('input', () => {
        let v = parseInt(paintValueInput.value || '0', 10);
        v = Math.max(0, Math.min(61, v));
        paintValueInput.value = v;
        paintValueChar.textContent = `(${valToCharLocal(v)})`;
      });
    }

    // Value brush size
    if (valueBrushSlider && valueBrushDisplay) {
      window.valueBrushSize = parseInt(valueBrushSlider.value, 10) || 1;
      valueBrushDisplay.textContent = String(window.valueBrushSize);

      valueBrushSlider.addEventListener('input', () => {
        window.valueBrushSize = Math.max(1, Math.min(25, parseInt(valueBrushSlider.value || '1', 10)));
        valueBrushDisplay.textContent = String(window.valueBrushSize);
      });
    }

    // Value paint toggle button
    initValuePaintToggle();

    // Initialize default paint mode
    setPaintMode(window.paintMode || 'terrain');
  }

  // Value paint toggle (Start/Stop)
  function initValuePaintToggle() {
    const btn = document.getElementById('start-value-paint-btn');
    const pmBtn = document.getElementById('paint-mode-btn');
    const isPaintingOn = () => window.currentMode === 'paint';

    if (!btn || !pmBtn) return;

    let active = false;

    function setBtn(on) {
      active = !!on;
      btn.textContent = active ? 'Stop Value Painting' : 'Start Value Painting';
      btn.classList.toggle('btn-danger', active);
      btn.classList.toggle('btn-secondary', !active);
      btn.setAttribute('aria-pressed', String(active));
    }

    function enableValuePaint() {
      // switch view to Value (optional, makes painting clearer)
      if (typeof window.renderMode !== 'undefined') {
        window.renderMode = 'value';
        if (typeof window.renderBackground === 'function') window.renderBackground();
        if (typeof window.drawCurrent === 'function') window.drawCurrent();
      }
      // ensure global paint mode is ON
      if (!isPaintingOn()) pmBtn.click();
      // force paint target to "value"
      setPaintMode('value');
      setBtn(true);
    }

    function disablePainting() {
      // turn global paint mode OFF & revert target to terrain as default
      if (isPaintingOn()) pmBtn.click();
      setPaintMode('terrain');
      setBtn(false);
    }

    // Main toggle
    btn.addEventListener('click', () => (active ? disablePainting() : enableValuePaint()));

    // Keep the button in sync if user clicks the global Paint Mode button directly
    function syncFromUI() {
      const paintingOn = isPaintingOn();
      setBtn(paintingOn && window.paintMode === 'value');
      // if painting turned off externally, reset target to terrain
      if (!paintingOn && window.paintMode === 'value') setPaintMode('terrain');
    }
    pmBtn.addEventListener('click', () => setTimeout(syncFromUI, 0));

    // initial state
    syncFromUI();
  }

  // Painting at event (mouse position)
  function paintAtEvent(e, grid, canvas) {
    const rect = canvas.getBoundingClientRect();
    const sx = (e.clientX - rect.left) * (canvas.width / rect.width);
    const sy = (e.clientY - rect.top) * (canvas.height / rect.height);

    // recompute after any resizes
    const cellW = canvas.width / grid.cols;
    const cellH = canvas.height / grid.rows;
    const x = Math.floor(sx / cellW);
    const y = Math.floor(sy / cellH);

    if (x >= 0 && x < grid.cols && y >= 0 && y < grid.rows) {
      const paintTarget = currentPaintTarget();
      const half = Math.floor(((paintTarget === 'value' ? (window.valueBrushSize || 1) : brushSize)) / 2);
      const paintValueInput = document.getElementById('paint-value');
      const valueToPaint = parseInt(paintValueInput?.value || '0', 10);

      for (let dy = -half; dy <= half; dy++) {
        for (let dx = -half; dx <= half; dx++) {
          const xx = x + dx, yy = y + dy;
          if (xx < 0 || xx >= grid.cols || yy < 0 || yy >= grid.rows) continue;

          if (paintTarget === 'value') {
            grid.setValueAt(xx, yy, valueToPaint);
          } else {
            grid.cells[yy][xx].terrain = paintType;
            if (window.variantGrid && window.TERRAIN_VARIANTS && window.TERRAIN) {
              window.variantGrid[yy][xx] = Math.floor(
                Math.random() * (window.TERRAIN_VARIANTS[paintType] || [window.TERRAIN[paintType].color]).length
              );
            }
            // keep value layer aligned with terrain type
            if (window.terrainValueMap) {
              grid.setValueAt(xx, yy, (window.terrainValueMap[paintType] ?? 1));
            }

            // Keep bytes cache in sync (if it exists)
            if (window._terrainCodeFlatCache && window.TERRAIN_CODE) {
              const idx = yy * grid.cols + xx;
              window._terrainCodeFlatCache[idx] = (window.TERRAIN_CODE[paintType] || 0);
              window._terrainCodeFlatRev++;
            }
          }
        }
      }

      if (paintTarget !== 'value') {
        if (typeof window.scheduleTerrainShading === 'function') {
          window.scheduleTerrainShading();
        }
      }

      if (typeof window.renderBackground === 'function') window.renderBackground();
      if (typeof window.drawCurrent === 'function') window.drawCurrent();
      if (typeof window.requestRecomputeFromSliders === 'function') window.requestRecomputeFromSliders();
    }
  }

  // Initialize canvas painting events
  function initCanvasPaintEvents(canvas) {
    canvas.addEventListener('mousedown', e => {
      if (window.currentMode === 'paint' && window.grid) {
        painting = true;
        paintAtEvent(e, window.grid, canvas);
      }
    });

    canvas.addEventListener('mousemove', e => {
      if (window.currentMode === 'paint' && painting && window.grid) {
        paintAtEvent(e, window.grid, canvas);
      }
    });

    document.addEventListener('mouseup', () => {
      painting = false;
    });
  }

  // Expose to global scope
  window.valToCharLocal = valToCharLocal;
  window.initPaintingControls = initPaintingControls;
  window.initCanvasPaintEvents = initCanvasPaintEvents;

})();
