// js/pathfinding.js - Worker pool and cost map computation
(function() {
  'use strict';

  const USE_REACH = false;  // When false, bidding ignores reach maps; eligibility = (finite cost && cap>0)

  // ─────────── Web-Worker Pathfinding Setup (POOL) ───────────
  let _pfMsgId = 0;
  // id -> resolve() for results coming back from any worker
  const _pfPending = new Map();

  /**
   * Minimal worker pool that feeds jobs to idle workers.
   * We keep your existing "message id" pattern so the rest of the code stays unchanged.
   */
  class PathWorkerPool {
    constructor(url, size) {
      this.url = url;
      this.queue = [];             // pending jobs: {payload, transfer}
      this.workers = [];
      this._makeWorkers(size);
      this._terrainKey = null; // tracks what terrain workers currently have
    }

    _makeWorkers(size) {
      for (let i = 0; i < size; i++) {
        const w = new Worker(this.url);
        w._busy = false;

        // When a worker finishes a job, resolve the pending promise by id,
        // mark the worker idle, and dispatch the next queued job.
        w.onmessage = (e) => {
          const { id } = e.data || {};
          const resolve = _pfPending.get(id);
          if (resolve) {
            resolve(e.data);
            _pfPending.delete(id);
          }
          w._busy = false;
          this._drain();
        };

        w.onerror = (err) => {
          console.error('[path worker] error:', err);
          // Mark idle and keep going so one worker crash doesn't stall everything
          w._busy = false;
          this._drain();
        };

        this.workers.push(w);
      }
    }

    /**
     * Enqueue a job for the pool. The job MUST already contain a unique "id".
     * Optionally pass Transferable objects via `transfer`.
     */
    postMessage(payload, transfer) {
      this.queue.push({ payload, transfer });
      this._drain();
    }

    setTerrainIfNeeded(terrainCodeFlat, rows, cols, rev) {
      const key = `${rows}x${cols}@${rev}`;
      if (this._terrainKey === key) return;
      this._terrainKey = key;

      // Send to each worker ONCE (transfer a per-worker copy to avoid repeated structured cloning)
      for (const w of this.workers) {
        const copy = new Uint8Array(terrainCodeFlat); // clones bytes
        w.postMessage(
          { type: 'setTerrain', rows, cols, terrainCodeFlat: copy },
          [copy.buffer] // transfer ownership of the copy to the worker
        );
      }
    }

    _drain() {
      for (const w of this.workers) {
        if (this.queue.length === 0) return;
        if (w._busy) continue;
        const job = this.queue.shift();
        w._busy = true;
        if (job.transfer && job.transfer.length) {
          w.postMessage(job.payload, job.transfer);
        } else {
          w.postMessage(job.payload);
        }
      }
    }
  }

  // Create a pool sized to your CPU (cap a bit so we don't spawn dozens)
  const _POOL_SIZE = Math.max(
    1,
    Math.min((navigator.hardwareConcurrency || 4), 4)
  );

  const pathPool = new PathWorkerPool('js/pathfindingWorker_latest.js?v=1', _POOL_SIZE);

  // Map the engine's terrain strings to compact byte codes.
  // Keep this in sync with the worker's mapping.
  const TERRAIN_CODE = {
    PLAIN: 0, DESERT: 1, WATER: 2, MOUNTAIN: 3, FOREST: 4, SHRUB: 5, RIVER: 6, ICE: 7, OCEAN: 8
  };

  // ---- Cached terrain bytes (0..7 per cell) ----
  function rebuildTerrainByteCache(grid) {
    const { rows, cols } = grid;
    const A = new Uint8Array(rows * cols);
    let k = 0;
    for (let y = 0; y < rows; y++) {
      const row = grid.cells[y];
      for (let x = 0; x < cols; x++) {
        A[k++] = (TERRAIN_CODE[row[x].terrain] || 0);
      }
    }
    window._terrainCodeFlatCache = A;
    window._terrainCodeFlatRev++;
  }

  function getTerrainBytes(grid) {
    const N = grid.rows * grid.cols;
    if (!(window._terrainCodeFlatCache instanceof Uint8Array) ||
        window._terrainCodeFlatCache.length !== N) {
      rebuildTerrainByteCache(grid);
    }
    return window._terrainCodeFlatCache;
  }

  // offload one empire's cost-map job to the pool
  function computeCostMapOffload(emp, grid) {
    const id = ++_pfMsgId;

    return new Promise(resolve => {
      _pfPending.set(id, resolve);

      const terrainCodeFlat = getTerrainBytes(grid);
      pathPool.setTerrainIfNeeded(
        terrainCodeFlat,
        grid.rows,
        grid.cols,
        window._terrainCodeFlatRev || 0
      );

      const payload = {
        id,
        empireId: emp.id,
        rows: grid.rows,
        cols: grid.cols,
        travelSpeeds: emp.travelSpeeds,
        capital: emp.capital,
      };

      pathPool.postMessage(payload);
    });
  }

  // --- Display density cap (for memory/CPU sanity) ---
  const MAX_DPR = 1.5;
  function getEffectiveDPR() {
    return Math.min(window.devicePixelRatio || 1, MAX_DPR);
  }

  // Expose to global scope
  window.TERRAIN_CODE = TERRAIN_CODE;
  window.pathPool = pathPool;
  window.rebuildTerrainByteCache = rebuildTerrainByteCache;
  window.getTerrainBytes = getTerrainBytes;
  window.computeCostMapOffload = computeCostMapOffload;
  window.getEffectiveDPR = getEffectiveDPR;

})();
