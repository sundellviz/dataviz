// js/import-export.js - All import/export functionality for terrain, empires, value layer
(function() {
  'use strict';

  // --- Download Helpers ---
  function downloadTextFile(name, text) {
    const blob = new Blob([text], { type: 'text/plain' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = name;
    a.click();
  }

  function downloadJSON(name, text) {
    const blob = new Blob([text], { type: 'application/json' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = name;
    a.click();
  }

  // --- Import/Export Mappings ---
  const TERRAIN_CHAR = {
    PLAIN: 'P',
    DESERT: 'D',
    WATER: 'W',
    OCEAN: 'O',
    MOUNTAIN: 'M',
    FOREST: 'F',
    SHRUB: 'S',
    RIVER: 'R',
    ICE: 'I'
  };
  const CHAR_TERRAIN = Object.fromEntries(
    Object.entries(TERRAIN_CHAR).map(([k, v]) => [v, k])
  );

  // --- Load Terrain From Text ---
  function loadTerrainFromText(text, gridWidthSlider, gridWidthDisplay, gridHeightSlider, gridHeightDisplay) {
    // Prevent trade route recomputation during import
    window._terrainImportInProgress = true;

    try {
      // parse lines → token matrix (handles space-separated or single-char files)
      const rawLines = text.trim().split(/\r?\n/).filter(l => l.trim());
      const mapTokens = rawLines.map(l => {
        const parts = l.trim().split(/\s+/);
        return (parts.length > 1) ? parts : l.trim().split('');
      });

      const rows = mapTokens.length;
      const cols = mapTokens[0].length;

      // update grid-slider & UI state
      if (gridWidthSlider) gridWidthSlider.value = cols;
      if (gridWidthDisplay) gridWidthDisplay.textContent = cols;
      if (gridHeightSlider) gridHeightSlider.value = rows;
      if (gridHeightDisplay) gridHeightDisplay.textContent = rows;

      window.gridWidth = cols;
      window.gridHeight = rows;

      const grid = new window.Grid(cols, rows);
      window.grid = grid;
      grid.initValueLayer(11);

      // Re-snap height to the new aspect (keeping current wrapper width)
      if (window.__resizerSnapToCurrent) { window.__resizerSnapToCurrent(); }

      // populate terrains (use for loop instead of forEach for speed)
      for (let y = 0; y < rows; y++) {
        const rowTokens = mapTokens[y];
        const gridRow = grid.cells[y];
        for (let x = 0; x < cols; x++) {
          gridRow[x].terrain = CHAR_TERRAIN[rowTokens[x]] || 'PLAIN';
        }
      }

      // Rebuild terrain bytes cache once
      if (typeof window.rebuildTerrainByteCache === 'function') {
        window.rebuildTerrainByteCache(grid);
      }

      // regenerate variants + shading + redraw
      if (typeof window.generateVariantGrid === 'function') window.generateVariantGrid();
      if (typeof window.computeMountainDepth === 'function') window.computeMountainDepth(grid);
      if (typeof window.resizeCanvases === 'function') window.resizeCanvases();
      if (typeof window.precomputeWaterShading === 'function') {
        const offscreen = window._offscreen;
        if (offscreen) window.precomputeWaterShading(grid, offscreen.width, offscreen.height);
      }
      if (typeof window.renderBackground === 'function') window.renderBackground();
      if (typeof window.drawCurrent === 'function') window.drawCurrent();

      return grid;
    } finally {
      // Clear the import flag
      window._terrainImportInProgress = false;

      // Invalidate trade routes after import is complete
      // They will be recomputed lazily when trade view is activated
      window._tradeRoutesInvalid = true;
      window.tradeOverlayDirty = true;
    }
  }

  // --- Import Empires From Text ---
  function importEmpiresFromText(jsonText, sourceLabel = 'JSON') {
    const configs = JSON.parse(jsonText);
    if (!Array.isArray(configs)) {
      throw new Error('Expected a JSON array of empire configs.');
    }

    // Clear existing (including the reset snapshot)
    if (Array.isArray(EmpireManager.empires)) EmpireManager.empires.length = 0;
    else EmpireManager.empires = [];
    EmpireManager._allEmpires = [];

    EmpireManager.nextId = 1;
    EmpireManager.nextColorIdx = 0;
    document.getElementById('empire-panels').innerHTML = '';

    const DEFAULT_TRAVEL_SPEEDS = window.DEFAULT_TRAVEL_SPEEDS || {
      PLAIN: 3, DESERT: 10, WATER: 2, OCEAN: 4, MOUNTAIN: 8,
      FOREST: 5, SHRUB: 4, RIVER: 1, ICE: 10, SWITCH: 0
    };
    const defaults = window.globalTravelSpeeds || { ...DEFAULT_TRAVEL_SPEEDS };

    const TERRAIN_KEYS = ['PLAIN', 'DESERT', 'WATER', 'OCEAN', 'MOUNTAIN', 'FOREST', 'SHRUB', 'RIVER', 'ICE'];

    const grid = window.grid;

    configs.forEach(cfg => {
      const emp = EmpireManager.addEmpire(cfg.name, cfg.color);

      // Build UI first so sliders exist
      if (typeof EmpireManager.createEmpirePanel === 'function') {
        EmpireManager.createEmpirePanel(emp);
      } else if (typeof window.createEmpirePanel === 'function') {
        window.createEmpirePanel(emp);
      }

      // Restore properties (with sane fallbacks)
      emp.size = (cfg.size != null) ? cfg.size : emp.size;
      emp.travelSpeeds = Object.assign({}, defaults, cfg.travelSpeeds || {});
      emp.capital = cfg.capital || null;

      // IMPORTANT: Initialize territory with capital cell so empire isn't treated as ghost
      if (emp.capital && grid) {
        if (!emp.territory) emp.territory = new Set();
        emp.territory.clear();
        const capIdx = emp.capital.y * grid.cols + emp.capital.x;
        emp.territory.add(capIdx);
        emp._costMapDirty = true; // Mark for cost map computation
      }

      // Sync UI values
      if (typeof window.applySliderToUI === 'function') {
        for (const k of TERRAIN_KEYS) window.applySliderToUI(emp, k);
        window.applySliderToUI(emp, 'SWITCH');
      }

      // Capital label
      if (typeof window.updateCapitalLabel === 'function') {
        window.updateCapitalLabel(emp);
      }
    });

    if (typeof window.drawCurrent === 'function') window.drawCurrent();
  }

  // --- Export terrain ---
  function exportTerrain(grid) {
    let out = '';
    for (let y = 0; y < grid.rows; y++) {
      for (let x = 0; x < grid.cols; x++) {
        out += TERRAIN_CHAR[grid.cells[y][x].terrain];
      }
      out += '\n';
    }
    downloadTextFile('terrain.txt', out);
  }

  // --- Export empires ---
  function exportEmpires() {
    const data = EmpireManager.empires.map(emp => ({
      name: emp.name,
      color: emp.color,
      size: emp.size,
      travelSpeeds: emp.travelSpeeds,
      capital: emp.capital
    }));
    downloadJSON('empires.json', JSON.stringify(data, null, 2));
  }

  // --- Export ownership grid ---
  function exportOwnership(grid) {
    const rows = grid.rows, cols = grid.cols;

    // 1) Build a 2D char buffer, default '0'
    const buf = Array.from({ length: rows }, () => Array(cols).fill('0'));

    // Map 1..9 -> '1'..'9', 10..35 -> 'A'..'Z', 36..61 -> 'a'..'z'
    function ordinalToSymbol(ord) {
      if (ord <= 9) return String(ord);
      ord -= 9;
      if (ord <= 26) return String.fromCharCode(64 + ord);
      ord -= 26;
      if (ord <= 26) return String.fromCharCode(96 + ord);
      return '?';
    }

    const symbolByEmpireId = new Map();
    EmpireManager.empires.forEach((emp, idx) => {
      const ord = idx + 1;
      symbolByEmpireId.set(emp.id, ordinalToSymbol(ord));
    });

    // Fill owned cells
    EmpireManager.empires.forEach(emp => {
      const sym = symbolByEmpireId.get(emp.id);
      if (!sym) return;
      emp.territory.forEach(idx => {
        const y = Math.floor(idx / cols);
        const x = idx % cols;
        buf[y][x] = sym;
      });
    });

    const text = buf.map(row => row.join('')).join('\n');
    downloadTextFile('ownership.txt', text);
  }

  // --- Init terrain menu ---
  function initTerrainMenu() {
    const menu = document.getElementById('terrain-menu');
    if (!menu) return;

    menu.addEventListener('change', async () => {
      const fname = menu.value;
      if (!fname) return;

      try {
        const res = await fetch(`./terrain/${fname}`);
        if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
        const text = await res.text();

        const gridWidthSlider = document.getElementById('grid-width-slider');
        const gridWidthDisplay = document.getElementById('grid-width-display');
        const gridHeightSlider = document.getElementById('grid-height-slider');
        const gridHeightDisplay = document.getElementById('grid-height-display');

        loadTerrainFromText(text, gridWidthSlider, gridWidthDisplay, gridHeightSlider, gridHeightDisplay);

        // Automatically load value layer
        try {
          const res_value = await fetch(`./landvalue/landvalue_${fname}`);
          if (res_value.ok) {
            const text_value = await res_value.text();
            const grid = window.grid;
            grid.importValueLayerFromText(text_value);

            // Set growth threshold
            const avgValue = window.computeGlobalAvgLandValue ? window.computeGlobalAvgLandValue() : 1;
            window.growthThreshold = 1;

            const slider = document.getElementById('growth-threshold');
            const label = document.getElementById('growth-threshold-val');
            if (slider) {
              slider.value = '1';
              if (label) label.textContent = '1';
            }
          }

          // Set water land value to 15 after value layer is loaded
          if (window.terrainValueMap) {
            window.terrainValueMap['WATER'] = 15;
            if (typeof window.buildTerrainValueSliders === 'function') {
              window.buildTerrainValueSliders();
            }
            const waterSlider = document.getElementById('val-slider-WATER');
            if (waterSlider) {
              waterSlider.value = '16';
              waterSlider.dispatchEvent(new Event('input'));
              waterSlider.value = '15';
              waterSlider.dispatchEvent(new Event('input'));
            }
          }
        } catch (e) {
          // Value layer not found, that's OK
        }

      } catch (err) {
        alert('Failed to load terrain: ' + err.message);
      } finally {
        menu.value = '';
      }
    });
  }

  // --- Init value menu ---
  function initValueMenu() {
    const menu = document.getElementById('value-menu');
    if (!menu) return;

    menu.addEventListener('change', async () => {
      const fname = menu.value;
      if (!fname) return;

      try {
        const res = await fetch(`./${fname}`);
        if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
        const text = await res.text();

        const grid = window.grid;
        const ok = grid.importValueLayerFromText(text);
        if (!ok) {
          alert('Value map size does not match the current grid. Load the matching terrain first (same rows × cols), then try again.');
        } else {
          window.renderMode = 'value';
          if (typeof window.renderBackground === 'function') window.renderBackground();
          if (typeof window.drawCurrent === 'function') window.drawCurrent();
        }
      } catch (err) {
        alert('Failed to load value map: ' + err.message);
      } finally {
        menu.value = '';
      }
    });
  }

  // --- Init overlay menu ---
  function initOverlayMenu() {
    const overlayMenu = document.getElementById('overlay-menu');
    if (!overlayMenu) return;

    overlayMenu.addEventListener('change', async (e) => {
      const fname = e.target.value;
      if (!fname) return;

      const img = new Image();
      img.onload = () => {
        window.overlayImage = img;
        if (typeof window.drawCurrent === 'function') window.drawCurrent();
      };
      img.onerror = () => {
        alert('Failed to load overlay: ' + fname);
      };
      img.src = fname;

      overlayMenu.value = '';
    });
  }

  // --- Init cities menu ---
  function initCitiesMenu() {
    const menu = document.getElementById('cities-menu');
    if (!menu) return;

    menu.addEventListener('change', async () => {
      const fname = menu.value;
      if (!fname) return;

      try {
        const res = await fetch(`./${fname}`);
        if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
        const text = await res.text();

        importEmpiresFromText(text, fname);
      } catch (err) {
        alert('Failed to load preset: ' + fname + '\n' + err.message);
      } finally {
        menu.value = '';
      }
    });
  }

  // --- Wire all import/export buttons ---
  function initImportExportButtons() {
    const exportTerrainBtn = document.getElementById('export-terrain-btn');
    const importTerrainInput = document.getElementById('import-terrain-input');
    const exportEmpiresBtn = document.getElementById('export-empires-btn');
    const importEmpiresBtn = document.getElementById('import-empires-btn');
    const importEmpiresInput = document.getElementById('import-empires-input');
    const exportOwnershipBtn = document.getElementById('export-ownership-btn');
    const importValueBtn = document.getElementById('import-value-btn');
    const importValueFile = document.getElementById('import-value-file');
    const exportValueBtn = document.getElementById('export-value-btn');

    // Export terrain
    if (exportTerrainBtn) {
      exportTerrainBtn.addEventListener('click', () => {
        if (window.grid) exportTerrain(window.grid);
      });
    }

    // Import terrain
    if (importTerrainInput) {
      importTerrainInput.addEventListener('change', function() {
        const file = this.files[0];
        if (!file) return;
        const reader = new FileReader();
        reader.onload = () => {
          const gridWidthSlider = document.getElementById('grid-width-slider');
          const gridWidthDisplay = document.getElementById('grid-width-display');
          const gridHeightSlider = document.getElementById('grid-height-slider');
          const gridHeightDisplay = document.getElementById('grid-height-display');
          loadTerrainFromText(reader.result, gridWidthSlider, gridWidthDisplay, gridHeightSlider, gridHeightDisplay);
        };
        reader.readAsText(file);
        this.value = '';
      });
    }

    // Export empires
    if (exportEmpiresBtn) {
      exportEmpiresBtn.addEventListener('click', exportEmpires);
    }

    // Import empires button (opens file input)
    if (importEmpiresBtn && importEmpiresInput) {
      importEmpiresBtn.addEventListener('click', () => importEmpiresInput.click());
    }

    // Import empires from file
    if (importEmpiresInput) {
      importEmpiresInput.addEventListener('change', function() {
        const file = this.files[0];
        if (!file) return;
        const reader = new FileReader();
        reader.onload = () => {
          try {
            importEmpiresFromText(String(reader.result), file.name || 'file');
          } catch (err) {
            alert('Error loading ' + (file.name || 'empires.json') + ': ' + err.message);
          }
        };
        reader.readAsText(file);
        this.value = '';
      });
    }

    // Export ownership
    if (exportOwnershipBtn) {
      exportOwnershipBtn.addEventListener('click', () => {
        if (window.grid) exportOwnership(window.grid);
      });
    }

    // Import value layer
    if (importValueBtn && importValueFile) {
      importValueBtn.addEventListener('click', () => importValueFile.click());
      importValueFile.addEventListener('change', () => {
        const file = importValueFile.files?.[0];
        if (!file) return;
        const reader = new FileReader();
        reader.onload = () => {
          const grid = window.grid;
          const ok = grid.importValueLayerFromText(String(reader.result));
          if (ok) {
            if (typeof window.renderBackground === 'function') window.renderBackground();
            if (typeof window.drawCurrent === 'function') window.drawCurrent();
          }
        };
        reader.readAsText(file);
        importValueFile.value = '';
      });
    }

    // Export value layer
    if (exportValueBtn) {
      exportValueBtn.addEventListener('click', () => {
        const grid = window.grid;
        const txt = grid.exportValueLayerToText();
        const name = `landvalue_${grid.cols}x${grid.rows}.txt`;
        downloadTextFile(name, txt);
      });
    }
  }

  // Expose to global scope
  window.downloadTextFile = downloadTextFile;
  window.loadTerrainFromText = loadTerrainFromText;
  window.importEmpiresFromText = importEmpiresFromText;
  window.exportTerrain = exportTerrain;
  window.exportEmpires = exportEmpires;
  window.exportOwnership = exportOwnership;
  window.initTerrainMenu = initTerrainMenu;
  window.initValueMenu = initValueMenu;
  window.initOverlayMenu = initOverlayMenu;
  window.initCitiesMenu = initCitiesMenu;
  window.initImportExportButtons = initImportExportButtons;

})();
