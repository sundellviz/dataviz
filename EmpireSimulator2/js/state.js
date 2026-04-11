// js/state.js - Shared state initialization for all modules
(function() {
  'use strict';

  // Initialize state directly on window
  window.currentMode = null;
  window.currentHeatEmpire = null;
  window.currentRouteEmpire = null;
  window.currentRouteTarget = null;
  window.isRecalibrating = false;
  window.recalibrateCancel = false;
  window.autoGrowAmount = 50;
  window.growthThreshold = 1;
  window.discriminationThreshold = 1;
  window.contestWeight = 0.5;
  window.tradeWeight = 0.5;
  window.tradeIncomingSlots = 2;
  window.tradeWaterDiscount = 1.0;
  window.tradeView = false;
  window.tradeRoutesPath = null;
  window.tradeOverlayDirty = true;
  window.infoMode = false;
  window.hideEmpireNames = false;
  window.hideDeadNames = true;
  window.magnifierEnabled = false;
  window._ownerVersion = 0;
  window._terrainCodeFlatCache = null;
  window._terrainCodeFlatRev = 0;
  window._drawDirty = true;
  window._heatOverlayDirty = true;
  window.viewMode = 'terrain';
  window.renderMode = 'terrain';
  window.paintMode = 'terrain';
  window.valueBrushSize = 1;
  window.variantGrid = [];

})();
