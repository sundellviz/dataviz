// js/performance.js - Performance monitoring
(function() {
  'use strict';

  const perfMonitor = {
    enabled: false,
    el: null,
    els: {},

    // Timing accumulators (rolling averages)
    samples: { render: [], sim: [], costmap: [] },
    maxSamples: 30,

    init() {
      this.el = document.getElementById('perf-monitor');
      this.els.sps = document.getElementById('perf-sps');
      this.els.render = document.getElementById('perf-render');
      this.els.sim = document.getElementById('perf-sim');
      this.els.costmap = document.getElementById('perf-costmap');

      // Wire up the checkbox
      const checkbox = document.getElementById('perf-monitor-checkbox');
      if (checkbox) {
        checkbox.addEventListener('change', (e) => {
          this.setEnabled(e.target.checked);
        });
        // Load saved preference
        try {
          const saved = localStorage.getItem('perfMonitorEnabled');
          if (saved === 'true') {
            checkbox.checked = true;
            this.setEnabled(true);
          }
        } catch {}
      }
    },

    setEnabled(on) {
      this.enabled = on;
      if (this.el) {
        this.el.classList.toggle('visible', on);
      }
      try {
        localStorage.setItem('perfMonitorEnabled', on ? 'true' : 'false');
      } catch {}
    },

    // Record a timing sample
    record(category, ms) {
      if (!this.enabled) return;
      const arr = this.samples[category];
      if (!arr) return;
      arr.push(ms);
      if (arr.length > this.maxSamples) arr.shift();
    },

    // Get rolling average
    avg(category) {
      const arr = this.samples[category];
      if (!arr || arr.length === 0) return 0;
      return arr.reduce((a, b) => a + b, 0) / arr.length;
    },

    // Update the display
    update() {
      if (!this.enabled || !this.el) return;

      const render = this.avg('render');
      const sim = this.avg('sim');
      const costmap = this.avg('costmap');

      // Steps per second = 1000 / (sim + render) to reflect true throughput
      const stepTime = sim + render;
      const sps = stepTime > 0 ? (1000 / stepTime) : 0;

      // Color coding helper
      const colorClass = (val, warnAt, badAt, higherIsBetter = false) => {
        if (higherIsBetter) {
          if (val <= badAt) return 'bad';
          if (val <= warnAt) return 'warn';
          return '';
        } else {
          if (val >= badAt) return 'bad';
          if (val >= warnAt) return 'warn';
          return '';
        }
      };

      if (this.els.sps) {
        this.els.sps.textContent = sps.toFixed(1);
        // Higher steps/sec is better: warn < 10, bad < 5
        this.els.sps.className = 'perf-value ' + colorClass(sps, 10, 5, true);
      }
      if (this.els.sim) {
        this.els.sim.textContent = sim.toFixed(1) + ' ms';
        this.els.sim.className = 'perf-value ' + colorClass(sim, 50, 200, false);
      }
      if (this.els.render) {
        this.els.render.textContent = render.toFixed(1) + ' ms';
        this.els.render.className = 'perf-value ' + colorClass(render, 16, 33, false);
      }
      if (this.els.costmap) {
        this.els.costmap.textContent = costmap.toFixed(0) + ' ms';
        this.els.costmap.className = 'perf-value ' + colorClass(costmap, 100, 500, false);
      }
    }
  };

  // Expose globally
  window.perfMonitor = perfMonitor;

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
      perfMonitor.init();
      // Update display periodically (not every frame to reduce overhead)
      setInterval(() => perfMonitor.update(), 200);
    });
  } else {
    // DOM already loaded
    perfMonitor.init();
    setInterval(() => perfMonitor.update(), 200);
  }

})();
