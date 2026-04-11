// js/ui-controls.js - All UI slider and button event handlers
(function() {
  'use strict';

  // Grid size state
  let gridWidth = 50;
  let gridHeight = 50;

  // Initialize all UI controls
  function initUIControls() {
    initGridSizeSliders();
    initAutoGrowSlider();
    initGrowthThresholdSlider();
    initDiscriminationSlider();
    initContestWeightSlider();
    initBaseCostOffsetSlider();
    initTradeWeightSlider();
    initOverlayControls();
    initHideNamesCheckbox();
    initHideDeadNamesCheckbox();
    initInfoModeCheckbox();
    initGhostCitiesCheckbox();
    initMagnifierCheckbox();
    initTradeSliders();
    initViewModeButtons();
    initHelpPopovers();
    initGlobalSpeedSliders();
    initTerrainValueSliders();
    initRandomEmpiresButton();
    initAddEmpireButton();
    initCollapseAllEmpiresButton();
    initComboAdjustButton();
    initRandomizeGridButton();
    initToggleGridButton();
    initLeaderboardSortHeaders();
    initAdaptTerrainButton();
  }

  // Grid size sliders
  function initGridSizeSliders() {
    const gridWidthSlider = document.getElementById('grid-width-slider');
    const gridWidthDisplay = document.getElementById('grid-width-display');
    const gridHeightSlider = document.getElementById('grid-height-slider');
    const gridHeightDisplay = document.getElementById('grid-height-display');

    if (gridWidthSlider) {
      gridWidth = parseInt(gridWidthSlider.value, 10);
      if (gridWidthDisplay) gridWidthDisplay.textContent = String(gridWidth);

      gridWidthSlider.addEventListener('input', () => {
        gridWidth = parseInt(gridWidthSlider.value, 10);
        if (gridWidthDisplay) gridWidthDisplay.textContent = String(gridWidth);
      });
    }

    if (gridHeightSlider) {
      gridHeight = parseInt(gridHeightSlider.value, 10);
      if (gridHeightDisplay) gridHeightDisplay.textContent = String(gridHeight);

      gridHeightSlider.addEventListener('input', () => {
        gridHeight = parseInt(gridHeightSlider.value, 10);
        if (gridHeightDisplay) gridHeightDisplay.textContent = String(gridHeight);
      });
    }

    window.gridWidth = gridWidth;
    window.gridHeight = gridHeight;
  }

  // Auto-grow slider
  function initAutoGrowSlider() {
    const slider = document.getElementById('auto-grow-speed');
    const display = document.getElementById('auto-grow-speed-val');

    if (slider && display) {
      window.autoGrowAmount = parseInt(slider.value, 10);
      display.textContent = String(window.autoGrowAmount);

      slider.addEventListener('input', () => {
        window.autoGrowAmount = parseInt(slider.value, 10);
        display.textContent = String(window.autoGrowAmount);
      });
    }
  }

  // Growth threshold slider
  function initGrowthThresholdSlider() {
    const s = document.getElementById('growth-threshold');
    const out = document.getElementById('growth-threshold-val');
    if (!s || !out) return;

    window.growthThreshold = parseInt(s.value || '10', 10);
    out.textContent = String(window.growthThreshold);

    s.addEventListener('input', () => {
      window.growthThreshold = Math.max(1, Math.min(61, parseInt(s.value || '10', 10)));
      out.textContent = String(window.growthThreshold);
    });
  }

  // Discrimination threshold slider
  function initDiscriminationSlider() {
    const s = document.getElementById('discrimination-threshold');
    const out = document.getElementById('discrimination-threshold-val');

    window.discriminationThreshold = Math.round(
      Number(s?.value ?? window.growthThreshold ?? 1)
    );

    if (out) out.textContent = String(window.discriminationThreshold);

    s?.addEventListener('input', () => {
      const v = Math.max(0, Math.min(61, parseInt(s.value || '1', 10)));
      window.discriminationThreshold = v;
      if (out) out.textContent = String(v);
    });
  }

  // Contest weight slider
  function initContestWeightSlider() {
    const s = document.getElementById('contest-weight');
    const out = document.getElementById('contest-weight-val');

    window.contestWeight = Number(s?.value ?? 0.5);
    if (out) out.textContent = window.contestWeight.toFixed(2);

    s?.addEventListener('input', () => {
      const v = Math.max(0, Math.min(1, Number(s.value || 0)));
      window.contestWeight = v;
      if (out) out.textContent = v.toFixed(2);
    });
  }

  // Base cost offset slider
  function initBaseCostOffsetSlider() {
    const s = document.getElementById('base-cost-offset');
    const out = document.getElementById('base-cost-offset-val');

    window.baseCostOffset = Number(s?.value ?? 0);
    if (out) out.textContent = window.baseCostOffset;

    s?.addEventListener('input', () => {
      const v = Math.max(0, Math.min(100, Number(s.value || 0) | 0));
      window.baseCostOffset = v;
      if (out) out.textContent = v;
    });
  }

  // Trade weight slider
  function initTradeWeightSlider() {
    const s = document.getElementById('trade-weight');
    const out = document.getElementById('trade-weight-val');

    window.tradeWeight = Number(s?.value ?? 0.5);
    if (out) out.textContent = window.tradeWeight.toFixed(2);

    s?.addEventListener('input', () => {
      const v = Math.max(0, Math.min(1, Number(s.value || 0)));
      window.tradeWeight = v;
      if (out) out.textContent = v.toFixed(2);
    });
  }

  // Overlay controls
  function initOverlayControls() {
    const overlayInput = document.getElementById('overlay-input');
    const opacitySlider = document.getElementById('overlay-opacity');
    const opacityDisplay = document.getElementById('overlay-opacity-display');
    const clearOverlayBtn = document.getElementById('clear-overlay-btn');

    if (overlayInput) {
      overlayInput.addEventListener('change', () => {
        const file = overlayInput.files[0];
        if (!file) return;
        const reader = new FileReader();
        reader.onload = () => {
          window.overlayImage = new Image();
          window.overlayImage.onload = () => {
            if (typeof window.drawCurrent === 'function') window.drawCurrent();
          };
          window.overlayImage.src = reader.result;
        };
        reader.readAsDataURL(file);
        overlayInput.value = '';
      });
    }

    if (opacitySlider && opacityDisplay) {
      window.overlayOpacity = parseInt(opacitySlider.value, 10) / 100;
      opacityDisplay.textContent = opacitySlider.value + '%';

      opacitySlider.addEventListener('input', () => {
        window.overlayOpacity = parseInt(opacitySlider.value, 10) / 100;
        opacityDisplay.textContent = opacitySlider.value + '%';
        if (typeof window.drawCurrent === 'function') window.drawCurrent();
      });
    }

    if (clearOverlayBtn) {
      clearOverlayBtn.addEventListener('click', () => {
        window.overlayImage = null;
        if (typeof window.drawCurrent === 'function') window.drawCurrent();
      });
    }
  }

  // Hide eliminated city names checkbox
  function initHideDeadNamesCheckbox() {
    const checkbox = document.getElementById('hide-dead-names-checkbox');
    if (checkbox) {
      checkbox.addEventListener('change', e => {
        window.hideDeadNames = e.target.checked;
        if (typeof window.drawCurrent === 'function') window.drawCurrent();
      });
    }
  }

  // Hide empire names checkbox
  function initHideNamesCheckbox() {
    const checkbox = document.getElementById('hide-names-checkbox');
    if (checkbox) {
      checkbox.addEventListener('change', e => {
        window.hideEmpireNames = e.target.checked;
        if (typeof window.drawCurrent === 'function') window.drawCurrent();
      });
    }
  }

  // Info mode checkbox
  function initInfoModeCheckbox() {
    const checkbox = document.getElementById('info-mode-checkbox');
    if (checkbox) {
      checkbox.addEventListener('change', e => {
        window.infoMode = e.target.checked;
      });
    }
  }

  // Magnifying glass checkbox
  function initMagnifierCheckbox() {
    const checkbox = document.getElementById('magnifier-checkbox');
    if (checkbox) {
      checkbox.addEventListener('change', e => {
        window.magnifierEnabled = e.target.checked;
        window._drawDirty = true;
      });
    }
  }

  // Ghost cities checkbox (eliminated empires remain as trade nodes)
  function initGhostCitiesCheckbox() {
    // Default to enabled
    window.ghostCitiesEnabled = true;

    const checkbox = document.getElementById('ghost-cities-checkbox');
    if (checkbox) {
      checkbox.checked = true;
      checkbox.addEventListener('change', e => {
        window.ghostCitiesEnabled = e.target.checked;
        // Recompute trade routes and redraw
        window._tradeRoutesInvalid = true;
        if (typeof window.computeTradeRoutes === 'function') {
          window.computeTradeRoutes(true);
        }
        if (typeof window.requestDraw === 'function') {
          window.requestDraw();
        }
      });
    }
  }

  // Trade sliders
  function initTradeSliders() {
    // Incoming slots slider
    const tradeSlotsSlider = document.getElementById('trade-incoming-slots');
    const tradeSlotsVal = document.getElementById('trade-incoming-slots-val');
    if (tradeSlotsSlider && tradeSlotsVal) {
      window.tradeIncomingSlots = parseInt(tradeSlotsSlider.value, 10) || 0;
      tradeSlotsVal.textContent = String(window.tradeIncomingSlots);

      tradeSlotsSlider.addEventListener('input', () => {
        window.tradeIncomingSlots = parseInt(tradeSlotsSlider.value, 10) || 0;
        tradeSlotsVal.textContent = String(window.tradeIncomingSlots);
        // Mark as invalid but don't null - keeps old routes visible during recomputation
        window._tradeRoutesInvalid = true;
        window.tradeOverlayDirty = true;
        if (window.tradeView && typeof window.computeTradeRoutes === 'function') {
          window.computeTradeRoutes(false);
        }
      });
    }

    // Water discount slider
    const tradeWaterSlider = document.getElementById('trade-water-discount');
    const tradeWaterVal = document.getElementById('trade-water-discount-val');
    if (tradeWaterSlider && tradeWaterVal) {
      window.tradeWaterDiscount = parseFloat(tradeWaterSlider.value) || 1.0;
      tradeWaterVal.textContent = window.tradeWaterDiscount.toFixed(2);

      tradeWaterSlider.addEventListener('input', () => {
        window.tradeWaterDiscount = parseFloat(tradeWaterSlider.value) || 1.0;
        tradeWaterVal.textContent = window.tradeWaterDiscount.toFixed(2);
        // Mark as invalid but don't null - keeps old routes visible during recomputation
        window._tradeRoutesInvalid = true;
        window.tradeOverlayDirty = true;
        if (window.tradeView && typeof window.computeTradeRoutes === 'function') {
          window.computeTradeRoutes(false);
        }
      });
    }

    // Recompute button
    const tradeRecomputeBtn = document.getElementById('trade-recompute-btn');
    if (tradeRecomputeBtn) {
      tradeRecomputeBtn.addEventListener('click', () => {
        if (typeof window.computeTradeRoutes === 'function') {
          window.computeTradeRoutes(true);
        }
      });
    }

    // Trade leaderboard header sorting is handled in trade.js installTradeDOMHandlers
  }

  // View mode buttons
  function initViewModeButtons() {
    const viewTerrainBtn = document.getElementById('view-terrain-btn');
    const viewValueBtn = document.getElementById('view-value-btn');

    function setViewMode(mode) {
      window.viewMode = mode;
      window.renderMode = mode;
      if (mode === 'value') {
        viewValueBtn?.classList.add('btn-secondary');
        viewTerrainBtn?.classList.remove('btn-secondary');
      } else {
        viewTerrainBtn?.classList.add('btn-secondary');
        viewValueBtn?.classList.remove('btn-secondary');
      }
      if (typeof window.renderBackground === 'function') window.renderBackground();
      if (typeof window.drawCurrent === 'function') window.drawCurrent();
    }

    viewTerrainBtn?.addEventListener('click', () => setViewMode('terrain'));
    viewValueBtn?.addEventListener('click', () => setViewMode('value'));
  }

  // Help popovers
  function initHelpPopovers() {
    const HELP = {
      canvas: { title: 'Canvas', text: 'Resize the pixel canvas and toggle grid/labels. This does not change the simulation grid (rows × cols).' },
      terrain: { title: 'Terrain', text: 'Load terrain, paint cells, and place an overlay guide. Rows × cols set the simulation grid.' },
      empires: { title: 'Empires', text: 'Add empires, set sizes and travel costs, and tweak hostile-territory penalty and heatmap options.' },
      value: { title: 'Value', text: 'Land-value layer: view, paint numeric values, import/export, or set terrain-wide defaults.' },
      hostile: { title: 'Hostile penalty', text: 'How much pathing through enemy territory should be penalized.' },
      empiresload: { title: 'Load / export empires', text: 'Load empire capitals and characteristics from file, or export. Can also export ownership grid.' },
      valuesliders: { title: 'Value sliders', text: 'Change the land value of all terrain of a certain type.' },
      terrainmodification: { title: 'Terrain modification', text: 'Paint terrain type or randomize a completely new map. Warning: Randomizing or changing grid size erases current map.' },
      optimizationdetail: { title: 'Empire simulation', text: 'Growth threshold: Above which land value empires grow. Discrimination: Threshold for which cells that should be ignored. Power weight: How much weight given to power relative to travel cost.' },
      growthdetail: { title: 'Set threshold for land value that causes growth. Empire size becomes land value divided by growth threshold.' },
      optimization: { title: 'Optimization', text: 'Recalibrate recomputes territories fully. Optimization varies capital locations or empire characteristics to improve territory or land value. ' }
    };

    let pop;
    function closePop() { if (pop) { pop.remove(); pop = null; } }

    document.addEventListener('click', (e) => {
      const btn = e.target.closest('.help-icon');
      if (!btn) return closePop();

      e.stopPropagation();
      const key = btn.getAttribute('data-help');
      const data = HELP[key];
      if (!data) return;

      closePop();
      pop = document.createElement('div');
      pop.className = 'help-pop';
      pop.innerHTML = `<h4>${data.title}</h4><p>${data.text}</p>`;
      document.body.appendChild(pop);

      const r = btn.getBoundingClientRect();
      const x = r.left + window.scrollX + r.width + 10;
      const y = r.top + window.scrollY - 4;
      pop.style.left = x + 'px';
      pop.style.top = y + 'px';
    });

    window.addEventListener('scroll', closePop, true);
  }

  // Global speed sliders
  function initGlobalSpeedSliders() {
    const wrap = document.getElementById('global-speed-sliders');
    if (!wrap) return;

    const TERRAIN_KEYS = window.TERRAIN_KEYS || ['PLAIN', 'DESERT', 'WATER', 'OCEAN', 'MOUNTAIN', 'FOREST', 'SHRUB', 'RIVER', 'ICE'];
    const DEFAULT_TRAVEL_SPEEDS = window.DEFAULT_TRAVEL_SPEEDS || {
      PLAIN: 3, DESERT: 10, WATER: 2, OCEAN: 4, MOUNTAIN: 8, FOREST: 5, SHRUB: 4, RIVER: 1, ICE: 10, SWITCH: 0
    };

    // Initialize global travel speeds
    if (!window.globalTravelSpeeds) {
      const first = (window.EmpireManager && EmpireManager.empires && EmpireManager.empires[0]) || null;
      window.globalTravelSpeeds = (first && first.travelSpeeds) ? { ...first.travelSpeeds } : { ...DEFAULT_TRAVEL_SPEEDS };
    }
    if (window.globalTravelSpeeds.SWITCH == null) window.globalTravelSpeeds.SWITCH = 0;

    function setGlobalSpeed(key, v) {
      let raw = parseFloat(v || 0);
      if (key === 'SWITCH') {
        raw = Math.max(0, Math.round(raw * 10) / 10);
      } else {
        raw = Math.max(0.1, Math.min(10, Math.round(raw * 10) / 10));
      }

      window.globalTravelSpeeds[key] = raw;

      if (window.EmpireManager && EmpireManager.empires) {
        for (const e of EmpireManager.empires) {
          e.travelSpeeds[key] = raw;
          e._costMapDirty = true;
          if (typeof window.applySliderToUI === 'function') window.applySliderToUI(e, key);
        }
      }

      if (typeof window.drawCurrent === 'function') window.drawCurrent();
      if (typeof window.requestRecomputeFromSliders === 'function') window.requestRecomputeFromSliders();
    }

    wrap.innerHTML = '';

    // Terrain speed sliders
    for (const t of TERRAIN_KEYS) {
      const row = document.createElement('label');
      row.style.display = 'grid';
      row.style.gridTemplateColumns = '90px 1fr 56px';
      row.style.alignItems = 'center';
      row.style.columnGap = '8px';
      row.style.margin = '4px 0';

      const name = document.createElement('span');
      name.textContent = t[0] + t.slice(1).toLowerCase();

      const slider = document.createElement('input');
      slider.type = 'range';
      slider.min = '0.1';
      slider.max = '10';
      slider.step = '0.1';
      slider.value = String(window.globalTravelSpeeds[t]);
      slider.style.width = '100%';
      slider.style.margin = '0';

      const num = document.createElement('input');
      num.type = 'number';
      num.min = '0.1';
      num.max = '10';
      num.step = '0.1';
      num.value = Number(window.globalTravelSpeeds[t]).toFixed(1);
      num.style.width = '56px';
      num.style.textAlign = 'right';

      function apply(v) {
        setGlobalSpeed(t, v);
        slider.value = String(window.globalTravelSpeeds[t]);
        num.value = Number(window.globalTravelSpeeds[t]).toFixed(1);
      }

      slider.addEventListener('input', () => apply(slider.value));
      num.addEventListener('change', () => apply(num.value));

      row.append(name, slider, num);
      wrap.appendChild(row);
    }

    // SWITCH row
    {
      const row = document.createElement('label');
      row.style.display = 'grid';
      row.style.gridTemplateColumns = '90px 1fr 56px';
      row.style.alignItems = 'center';
      row.style.columnGap = '8px';
      row.style.margin = '4px 0';

      const name = document.createElement('span');
      name.textContent = 'Switching';

      const slider = document.createElement('input');
      slider.type = 'range';
      slider.min = '0';
      slider.max = '10';
      slider.step = '0.1';
      slider.value = String(window.globalTravelSpeeds.SWITCH);
      slider.style.width = '100%';
      slider.style.margin = '0';

      const num = document.createElement('input');
      num.type = 'number';
      num.min = '0';
      num.step = '0.1';
      num.value = Number(window.globalTravelSpeeds.SWITCH).toFixed(1);
      num.style.width = '56px';
      num.style.textAlign = 'right';
      num.removeAttribute('max');

      function applyFromSlider(v) {
        setGlobalSpeed('SWITCH', v);
        slider.value = String(window.globalTravelSpeeds.SWITCH);
        num.value = Number(window.globalTravelSpeeds.SWITCH).toFixed(1);
      }

      function applyFromNumber(v) {
        setGlobalSpeed('SWITCH', v);
        slider.value = String(Math.min(10, window.globalTravelSpeeds.SWITCH));
        num.value = Number(window.globalTravelSpeeds.SWITCH).toFixed(1);
      }

      slider.addEventListener('input', () => applyFromSlider(slider.value));
      num.addEventListener('change', () => applyFromNumber(num.value));

      row.append(name, slider, num);
      wrap.appendChild(row);
    }
  }

  // Terrain value sliders
  function initTerrainValueSliders() {
    const terrainValueSliders = document.getElementById('terrain-value-sliders');
    const resetTerrainValuesBtn = document.getElementById('reset-terrain-values');

    if (!terrainValueSliders) return;

    // Initialize terrainValueMap
    if (!window.terrainValueMap && window.TERRAIN) {
      window.terrainValueMap = Object.fromEntries(
        Object.keys(window.TERRAIN).map(k => [k, 1])
      );
    }

    function valToChar(v) {
      v = Math.max(0, Math.min(61, Math.floor(v)));
      if (v <= 9) return String.fromCharCode(48 + v);
      if (v <= 35) return String.fromCharCode(97 + (v - 10));
      return String.fromCharCode(65 + (v - 36));
    }

    function buildTerrainValueSliders() {
      terrainValueSliders.innerHTML = '';

      for (const type of Object.keys(window.TERRAIN || {})) {
        const row = document.createElement('div');
        row.className = 'row';
        row.style.gap = '8px';
        row.style.alignItems = 'center';

        const label = document.createElement('span');
        label.style.width = '90px';
        label.textContent = type.charAt(0) + type.slice(1).toLowerCase();

        const num = document.createElement('input');
        num.type = 'number';
        num.min = '0';
        num.max = '61';
        num.step = '1';
        num.value = String(window.terrainValueMap[type]);
        num.style.width = '40px';

        const slider = document.createElement('input');
        slider.type = 'range';
        slider.min = '0';
        slider.max = '61';
        slider.step = '1';
        slider.value = String(window.terrainValueMap[type]);
        slider.id = `val-slider-${type}`;
        slider.style.flex = '1';

        const readout = document.createElement('span');
        const setReadout = (n) => { readout.textContent = `(${valToChar(n)})`; };
        setReadout(parseInt(slider.value, 10));

        function apply(n) {
          n = Math.max(0, Math.min(61, Math.floor(n)));
          window.terrainValueMap[type] = n;
          slider.value = String(n);
          num.value = String(n);
          setReadout(n);
          applyTerrainValueToGrid(type, n);
          if (typeof window.scheduleValueRebuild === 'function') window.scheduleValueRebuild();
        }

        slider.addEventListener('input', (e) => apply(e.target.value));
        num.addEventListener('change', (e) => apply(e.target.value));

        row.append(label, slider, num);
        terrainValueSliders.appendChild(row);
      }
    }

    function applyTerrainValueToGrid(type, v) {
      const grid = window.grid;
      if (!grid) return;
      const vv = Math.max(0, Math.min(61, v | 0));
      for (let y = 0; y < grid.rows; y++) {
        const row = grid.cells[y];
        for (let x = 0; x < grid.cols; x++) {
          if (row[x].terrain === type) {
            grid.setValueAt(x, y, vv);
          }
        }
      }
    }

    window.buildTerrainValueSliders = buildTerrainValueSliders;

    window.applyAllTerrainValues = function() {
      for (const [type, v] of Object.entries(window.terrainValueMap)) {
        applyTerrainValueToGrid(type, v);
      }
    };

    if (resetTerrainValuesBtn) {
      resetTerrainValuesBtn.addEventListener('click', () => {
        Object.keys(window.terrainValueMap).forEach(k => (window.terrainValueMap[k] = 1));
        buildTerrainValueSliders();
        window.applyAllTerrainValues();
        if (typeof window.scheduleValueRebuild === 'function') window.scheduleValueRebuild();
      });
    }

    buildTerrainValueSliders();
  }

  // Random empires button
  function initRandomEmpiresButton() {
    const btn = document.getElementById('add-random-empires-btn');
    const num = document.getElementById('random-empires-count');
    if (!btn || !num) return;

    btn.addEventListener('click', () => {
      const n = parseInt(num.value || '1', 10);
      if (typeof window.addRandomEmpires === 'function') {
        window.addRandomEmpires(n);
      }
    });
  }

  // Add empire button
  function initAddEmpireButton() {
    const btn = document.getElementById('add-empire-btn');
    if (!btn) return;

    btn.addEventListener('click', () => {
      if (!window.EmpireManager) return;

      const emp = EmpireManager.addEmpire();
      if (typeof window.createEmpirePanel === 'function') {
        window.createEmpirePanel(emp);
      }

      // Default travel costs from global settings
      const TERRAIN_KEYS = window.TERRAIN_KEYS || ['PLAIN', 'DESERT', 'WATER', 'OCEAN', 'MOUNTAIN', 'FOREST', 'SHRUB', 'RIVER', 'ICE'];
      if (window.globalTravelSpeeds) {
        emp.travelSpeeds = JSON.parse(JSON.stringify(window.globalTravelSpeeds));
      }
      if (typeof window.applySliderToUI === 'function') {
        for (const k of TERRAIN_KEYS) window.applySliderToUI(emp, k);
      }

      window.currentMode = 'placeCapital';
      window.currentEmpire = emp;
    });
  }

  // Collapse all empires button
  function initCollapseAllEmpiresButton() {
    const btn = document.getElementById('collapse-all-empires-btn');
    if (!btn) return;

    btn.addEventListener('click', () => {
      document.querySelectorAll('#empire-panels details').forEach(detail => detail.open = false);
    });
  }

  // Combo adjust button (Run simulation)
  function initComboAdjustButton() {
    const comboBtn = document.getElementById('combo-adjust-optimize-btn');
    if (!comboBtn) return;

    async function toggleRun(btn) {
      if (btn.dataset.running === '1') {
        btn.dataset.running = '0';
        return;
      }
      if (typeof window.runRecalibrateWithDynamicSizes === 'function') {
        await window.runRecalibrateWithDynamicSizes(btn);
      }
    }

    comboBtn.addEventListener('click', () => toggleRun(comboBtn));
  }

  // Randomize grid button
  function initRandomizeGridButton() {
    const btn = document.getElementById('randomize-btn');
    if (!btn) return;

    btn.addEventListener('click', () => {
      if (typeof window.rebuildGrid === 'function') {
        window.rebuildGrid();
      }
    });
  }

  // Toggle grid button
  function initToggleGridButton() {
    const btn = document.getElementById('toggle-grid-btn');
    if (!btn) return;

    window.showGrid = false;
    btn.textContent = 'Show Grid Lines';

    btn.addEventListener('click', () => {
      window.showGrid = !window.showGrid;
      btn.textContent = window.showGrid ? 'Hide Grid Lines' : 'Show Grid Lines';
      if (typeof window.drawCurrent === 'function') window.drawCurrent();
    });
  }

  // Helper: update empire capital label
  function updateCapitalLabel(emp) {
    if (emp._capitalDisplay && emp.capital) {
      emp._capitalDisplay.textContent = `Capital: (${emp.capital.x},${emp.capital.y})`;
    }
  }

  // Helper: apply slider value to UI
  function applySliderToUI(emp, key) {
    const s = emp._speedSliders?.[key];
    const v = emp._speedValues?.[key];
    const formatted = Number(emp.travelSpeeds[key]).toFixed(1);
    if (s) s.value = emp.travelSpeeds[key];
    if (v) {
      if (v.tagName === 'INPUT') v.value = formatted;
      else v.textContent = formatted;
    }
  }

  // Random land cells picker
  function pickRandomLandCells(n) {
    const grid = window.grid;
    if (!grid) return [];

    const land = [];
    for (let y = 0; y < grid.rows; y++) {
      for (let x = 0; x < grid.cols; x++) {
        if (grid.cells[y][x].terrain !== 'WATER' && grid.cells[y][x].terrain !== 'OCEAN') {
          land.push({ x, y });
        }
      }
    }

    // Exclude already-occupied capitals
    const occ = new Set();
    if (window.EmpireManager) {
      for (const e of EmpireManager.empires) {
        if (e.capital) occ.add(e.capital.x + ',' + e.capital.y);
      }
    }

    // Fisher-Yates shuffle
    for (let i = land.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [land[i], land[j]] = [land[j], land[i]];
    }

    const out = [];
    for (const c of land) {
      const key = c.x + ',' + c.y;
      if (!occ.has(key)) {
        out.push(c);
        if (out.length >= n) break;
      }
    }
    return out;
  }

  // Add random empires
  async function addRandomEmpires(n) {
    n = Math.max(1, Math.min(100, n | 0));
    const cells = pickRandomLandCells(n);
    if (!cells.length || !window.EmpireManager) return;

    const TERRAIN_KEYS = window.TERRAIN_KEYS || ['PLAIN', 'DESERT', 'WATER', 'OCEAN', 'MOUNTAIN', 'FOREST', 'SHRUB', 'RIVER', 'ICE'];

    for (let i = 0; i < cells.length; i++) {
      const emp = EmpireManager.addEmpire();
      if (typeof window.createEmpirePanel === 'function') window.createEmpirePanel(emp);

      // Default travel costs from global settings
      if (window.globalTravelSpeeds) {
        emp.travelSpeeds = JSON.parse(JSON.stringify(window.globalTravelSpeeds));
      }
      for (const k of TERRAIN_KEYS) applySliderToUI(emp, k);
      applySliderToUI(emp, 'SWITCH');

      // Place capital and seed initial territory (matches manual placement in main.js)
      emp.capital = { x: cells[i].x, y: cells[i].y };
      const grid = window.grid;
      if (grid) {
        if (!emp.territory) emp.territory = new Set();
        emp.territory.add(cells[i].y * grid.cols + cells[i].x);
      }
      emp._costMapDirty = true;
      updateCapitalLabel(emp);
    }

    if (typeof window.requestDraw === 'function') window.requestDraw();
  }

  // Rebuild grid
  function rebuildGrid() {
    const grid = new window.Grid(gridWidth, gridHeight);
    window.grid = grid;

    window.tradeRoutesPath = null;

    grid.initValueLayer(11);
    if (typeof window.applyAllTerrainValues === 'function') window.applyAllTerrainValues();

    if (window.__resizerSnapToCurrent) { window.__resizerSnapToCurrent(); }

    if (typeof window.generateVariantGrid === 'function') window.generateVariantGrid();
    if (typeof window.computeMountainDepth === 'function') window.computeMountainDepth(grid);
    if (typeof window.rebuildTerrainByteCache === 'function') window.rebuildTerrainByteCache(grid);
    if (typeof window.resizeCanvases === 'function') window.resizeCanvases();
    if (typeof window.renderBackground === 'function') window.renderBackground();
    if (typeof window.simulateAndDraw === 'function') window.simulateAndDraw();
  }

  // Schedule terrain shading helper
  let shadeQueued = false;
  function scheduleTerrainShading() {
    if (shadeQueued) return;
    shadeQueued = true;
    requestAnimationFrame(() => {
      const grid = window.grid;
      const offscreen = window._offscreen;
      if (typeof window.computeMountainDepth === 'function') window.computeMountainDepth(grid);
      if (typeof window.precomputeWaterShading === 'function' && offscreen) {
        window.precomputeWaterShading(grid, offscreen.width, offscreen.height);
      }
      if (typeof window.renderBackground === 'function') window.renderBackground();
      if (typeof window.drawCurrent === 'function') window.drawCurrent();
      shadeQueued = false;
    });
  }

  // Schedule value rebuild helper
  let valueRebuildQueued = false;
  function scheduleValueRebuild() {
    if (valueRebuildQueued) return;
    valueRebuildQueued = true;
    requestAnimationFrame(() => {
      if (typeof window.renderBackground === 'function') window.renderBackground();
      if (typeof window.drawCurrent === 'function') window.drawCurrent();
      valueRebuildQueued = false;
    });
  }

  // Wire leaderboard sortable headers
  function initLeaderboardSortHeaders() {
    const table = document.getElementById('leaderboard-table');
    if (!table) return;
    table.querySelectorAll('thead th[data-sort]').forEach(th => {
      th.addEventListener('click', () => {
        if (typeof window.setLeaderboardSort === 'function') {
          window.setLeaderboardSort(th.dataset.sort);
        }
      });
    });
  }

  // Adapt Costs to Terrain button + radius slider
  function initAdaptTerrainButton() {
    const btn = document.getElementById('adapt-terrain-btn');
    const slider = document.getElementById('adapt-terrain-radius');
    const valSpan = document.getElementById('adapt-terrain-radius-val');

    if (slider && valSpan) {
      valSpan.textContent = slider.value;
      slider.addEventListener('input', () => {
        valSpan.textContent = slider.value;
      });
    }

    if (!btn) return;

    btn.addEventListener('click', () => {
      const grid = window.grid;
      if (!grid) return;
      if (!window.EmpireManager || !EmpireManager.empires.length) return;

      const radius = parseInt(slider?.value || '10', 10);
      const rows = grid.rows;
      const cols = grid.cols;

      const TERRAIN_KEYS = window.TERRAIN_KEYS || ['PLAIN', 'DESERT', 'WATER', 'OCEAN', 'MOUNTAIN', 'FOREST', 'SHRUB', 'RIVER', 'ICE'];
      const baseline = 10;
      const minCost = 0.5;

      for (const emp of EmpireManager.empires) {
        if (!emp.capital) continue;

        const cx = emp.capital.x;
        const cy = emp.capital.y;

        // Count terrain types within Chebyshev radius
        const counts = {};
        for (const t of TERRAIN_KEYS) counts[t] = 0;
        let total = 0;

        const yMin = Math.max(0, cy - radius);
        const yMax = Math.min(rows - 1, cy + radius);
        const xMin = Math.max(0, cx - radius);
        const xMax = Math.min(cols - 1, cx + radius);

        for (let y = yMin; y <= yMax; y++) {
          for (let x = xMin; x <= xMax; x++) {
            const terrain = grid.cells[y][x].terrain;
            if (terrain in counts) {
              counts[terrain]++;
              total++;
            }
          }
        }

        if (total === 0) continue;

        // Compute fraction and map to cost
        for (const t of TERRAIN_KEYS) {
          const fraction = counts[t] / total;
          // cost = max(minCost, baseline - fraction * (baseline - minCost))
          const cost = Math.max(minCost, baseline - fraction * (baseline - minCost));
          const rounded = Math.round(cost * 10) / 10;
          emp.travelSpeeds[t] = rounded;

          if (typeof window.applySliderToUI === 'function') {
            window.applySliderToUI(emp, t);
          }
        }

        emp._costMapDirty = true;
      }

      if (typeof window.requestRecomputeFromSliders === 'function') {
        window.requestRecomputeFromSliders();
      }
      if (typeof window.invalidateTradeRoutes === 'function') {
        window.invalidateTradeRoutes();
      }
      if (typeof window.drawCurrent === 'function') {
        window.drawCurrent();
      }
    });
  }

  // Expose to global scope
  window.initUIControls = initUIControls;
  window.updateCapitalLabel = updateCapitalLabel;
  window.applySliderToUI = applySliderToUI;
  window.addRandomEmpires = addRandomEmpires;
  window.rebuildGrid = rebuildGrid;
  window.scheduleTerrainShading = scheduleTerrainShading;
  window.scheduleValueRebuild = scheduleValueRebuild;

})();
