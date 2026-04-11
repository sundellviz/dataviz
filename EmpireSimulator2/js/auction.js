// js/auction.js - Global auction algorithm for territory assignment
(function() {
  'use strict';

  // ═══════════════════════════════════════════════════════════════════════════
  // AUCTION CACHE - Reusable arrays that don't change during simulation
  // ═══════════════════════════════════════════════════════════════════════════
  const _auctionCache = {
    rows: 0,
    cols: 0,
    terrainRev: 0,
    valueRev: 0,
    isLand: null,    // Uint8Array - 1 if not OCEAN
    valFlat: null,   // Float32Array - land values
  };

  // ═══════════════════════════════════════════════════════════════════════════
  // TYPED-ARRAY POOL - Reuse across auction calls to avoid GC pressure
  // ═══════════════════════════════════════════════════════════════════════════
  const _pool = { N: 0, NK: 0 };

  function ensurePool(N, K) {
    const NK = N * K;
    if (_pool.N === N && _pool.NK === NK) return _pool;
    _pool.N = N;
    _pool.NK = NK;
    _pool.ownerPrev     = new Int16Array(N);
    _pool.topKId        = new Int16Array(NK);
    _pool.topKCost      = new Float32Array(NK);
    _pool.topKFrontier  = new Uint8Array(NK);   // 1=frontier, 0=water-reach
    _pool.topKLen       = new Uint8Array(N);
    _pool.topKUsed      = new Uint8Array(NK);
    _pool.maxCostByCell = new Float32Array(N);
    _pool.maxPowerByCell= new Float32Array(N);
    _pool.heapCost      = new Float32Array(NK);
    _pool.heapCell      = new Int32Array(NK);
    _pool.heapEmp       = new Int16Array(NK);
    _pool.heapPtr       = new Uint8Array(NK);
    _pool.owner         = new Int16Array(N);
    _pool.frontier      = new Uint8Array(N);
    _pool.waterCache    = new Uint8Array(N);  // 0=unknown, 1=false, 2=true
    _pool.eligible      = new Int32Array(N);  // pre-filtered cell indices (isLand && value >= delta)
    return _pool;
  }

  function getAuctionArrays(grid) {
    const rows = grid.rows, cols = grid.cols, N = rows * cols;
    const terrRev = window._terrainCodeFlatRev || 0;
    const valRev = window._valueLayerRev || 0;

    // Check if cache is valid
    if (_auctionCache.isLand && _auctionCache.valFlat &&
        _auctionCache.rows === rows && _auctionCache.cols === cols &&
        _auctionCache.terrainRev === terrRev && _auctionCache.valueRev === valRev) {
      return { isLand: _auctionCache.isLand, valFlat: _auctionCache.valFlat };
    }

    // Rebuild cache
    const isLand = new Uint8Array(N);
    const valFlat = new Float32Array(N);

    let i = 0;
    for (let y = 0; y < rows; y++) {
      const row = grid.cells[y];
      for (let x = 0; x < cols; x++, i++) {
        const cell = row[x];
        isLand[i] = (cell.terrain === 'OCEAN') ? 0 : 1;
        valFlat[i] = (typeof grid.getValueAt === 'function')
          ? grid.getValueAt(x, y)
          : Number(cell.value ?? cell.landValue ?? 0);
      }
    }

    // Store in cache
    _auctionCache.rows = rows;
    _auctionCache.cols = cols;
    _auctionCache.terrainRev = terrRev;
    _auctionCache.valueRev = valRev;
    _auctionCache.isLand = isLand;
    _auctionCache.valFlat = valFlat;

    return { isLand, valFlat };
  }

  // 8-neighbour offsets
  const DX8 = [1, -1, 0, 0, 1, 1, -1, -1];
  const DY8 = [0, 0, 1, -1, 1, -1, 1, -1];

  // Memoized water-reachability check (caches intermediate path results)
  // Cache is a pooled Uint8Array: 0=unknown, 1=cached false, 2=cached true
  // IMPORTANT: only TRAIL cells (water cells on the path) are cached, never idx itself.
  // Trail cells are guaranteed water, so their cache value correctly represents
  // "from this water cell backward, does a continuous water path reach owned territory?"
  // Caching idx would be incorrect because idx may be land, and a later trace
  // encountering idx as an intermediate cell would wrongly treat it as water-reachable.
  function reachableByWaterFromOwnedMemo(emp, idx, ownerPrev, grid, waterCache) {
    const cols = grid.cols;
    const terrainBytes = window._terrainCodeFlatCache;

    let getParentIdx;
    if (emp.parentIdx instanceof Int32Array) {
      getParentIdx = (i) => emp.parentIdx[i];
    } else if (emp.parentMap) {
      getParentIdx = (i) => {
        const x = i % cols, y = (i / cols) | 0;
        const p = emp.parentMap[y]?.[x];
        return p ? (p.y * cols + p.x) : -1;
      };
    } else {
      return false;
    }

    let i = getParentIdx(idx);
    if (i < 0) return false;

    let sawWater = false;
    const trail = [];
    let hops = 0, N = grid.rows * grid.cols;
    let result;

    while (i >= 0 && hops++ < N) {
      // Cache hit on intermediate water cell
      const ci = waterCache[i];
      if (ci) {
        if (ci === 2) { sawWater = true; result = true; }
        else { result = false; }
        break;
      }

      if (ownerPrev[i] === emp.id) {
        result = sawWater;
        break;
      }

      // Terrain check (same as original)
      let isWaterOrOcean;
      if (terrainBytes) {
        const code = terrainBytes[i];
        isWaterOrOcean = (code === 2 || code === 8);
      } else {
        const x = i % cols, y = (i / cols) | 0;
        const terr = grid.cells[y][x].terrain;
        isWaterOrOcean = (terr === 'WATER' || terr === 'OCEAN');
      }

      if (isWaterOrOcean) {
        sawWater = true;
        trail.push(i);
        i = getParentIdx(i);
        continue;
      }

      // Non-water, non-owned: path fails
      result = false;
      break;
    }

    if (result === undefined) result = false;

    // Cache only trail cells (guaranteed water) — never idx
    const val = result ? 2 : 1;
    for (let t = 0; t < trail.length; t++) waterCache[trail[t]] = val;

    return result;
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // GLOBAL AUCTION - Main territory assignment function
  // ═══════════════════════════════════════════════════════════════════════════
  async function recomputeOwnershipAuctionGlobal() {
    const grid = window.grid;
    if (!grid || !window.EmpireManager) return null;

    // --- Config ---
    const EPS = 1e-6;
    const K = 8;
    const EPSD = 1e-6;

    // --- Inputs & basics ---
    const emps = EmpireManager.empires;
    const rows = grid.rows, cols = grid.cols, N = rows * cols;

    // Use cached terrain arrays
    const { isLand, valFlat } = getAuctionArrays(grid);

    // Bidding gates
    const delta = Number(window.discriminationThreshold ?? window.growthThreshold ?? 1);
    const w = Math.max(0, Math.min(1, Number(window.contestWeight ?? 0.5)));
    const B = Math.max(0, Number(window.baseCostOffset ?? 0));

    // Max id
    let maxId = 0;
    for (const e of emps) if (e.id > maxId) maxId = e.id;

    // Filter out ghost empires (no territory) from auction bidding
    // Ghosts can't expand - they have zero power/slack
    const activeEmps = emps.filter(e => e.territory && e.territory.size > 0);

    // --- Previous ownership (pooled) ---
    const pool = ensurePool(N, K);
    const ownerPrev = pool.ownerPrev;
    ownerPrev.fill(0);
    for (const e of emps) {
      if (!e.territory) continue;
      for (const idx of e.territory) ownerPrev[idx] = e.id;
    }

    // --- Per-tick quotas ---
    const curr = new Int32Array(maxId + 1);
    for (let i = 0; i < N; i++) { const id = ownerPrev[i]; if (id > 0) curr[id]++; }

    const target = new Int32Array(maxId + 1);
    for (const e of emps) {
      target[e.id] = (e._targetSize != null) ? (e._targetSize | 0) : (e.size | 0);
    }

    // Power from slack capacity
    const thr = Number(window.growthThreshold ?? 0) || 0;
    const sumVal = new Float64Array(maxId + 1);
    for (let i = 0; i < N; i++) {
      const id = ownerPrev[i];
      if (id > 0) sumVal[id] += valFlat[i];
    }

    const P_byId = new Float32Array(maxId + 1);
    let globalPMax = 0;
    for (const e of emps) {
      const id = e.id;
      const slack = Math.max(0, (target[id] | 0) - (curr[id] | 0));
      const p = Math.sqrt(slack) || 0;
      P_byId[id] = p;
      if (p > globalPMax) globalPMax = p;
    }

    const G = Math.max(1, (Number(window.assignGrowthStep ?? window.autoGrowAmount ?? 5) | 0));
    const quota = new Int32Array(maxId + 1);
    let sumQuota = 0;
    // Only active empires (non-ghosts) participate in bidding
    for (const e of activeEmps) {
      const q = Math.max(0, Math.min(target[e.id], curr[e.id] + G));
      quota[e.id] = q;
      sumQuota += q;
    }

    // Ensure all empires have ready cost maps
    // (ghosts need cost maps too for trade route computation)
    let mapsReady = true;
    for (const e of emps) {
      if (!e.capital) continue;
      const base = e.costMapFlat;
      if (!base || base.length !== N) { mapsReady = false; break; }
      const ci = e.capital.y * cols + e.capital.x;
      if (!(base[ci] < Infinity)) { mapsReady = false; break; }
    }
    if (!mapsReady) {
      const t0 = performance.now();
      await EmpireManager.updateAllCostMaps(grid);
      if (window.perfMonitor) window.perfMonitor.record('costmap', performance.now() - t0);
    }

    // Capital indices
    const capIdxById = new Int32Array(maxId + 1);
    capIdxById.fill(-1);
    for (const e of emps) {
      if (e.capital) {
        capIdxById[e.id] = e.capital.y * cols + e.capital.x;
      }
    }

    // Cost maps by id
    const costById = new Array(maxId + 1);
    for (const e of emps) costById[e.id] = e.costMapFlat || null;

    // --- Top-K candidates per cell (pooled arrays, frontier merged per-empire) ---
    const topKId = pool.topKId;
    const topKCost = pool.topKCost;
    const topKFrontier = pool.topKFrontier;
    const topKLen = pool.topKLen;
    topKLen.fill(0);
    topKCost.fill(Infinity);

    const frontier = pool.frontier;
    const waterCache = pool.waterCache;

    // Pre-filter: build list of cells passing grid-level checks (same for all empires)
    const eligible = pool.eligible;
    let eligibleLen = 0;
    for (let i = 0; i < N; i++) {
      if (isLand[i] && valFlat[i] >= delta) {
        eligible[eligibleLen++] = i;
      }
    }

    for (const e of activeEmps) {
      const id = e.id;
      if (quota[id] <= curr[id]) continue;

      const base = e.costMapFlat;
      if (!base || base.length !== N) continue;

      // Build frontier for this empire (reuse single buffer)
      frontier.fill(0);
      waterCache.fill(0);
      for (const idx of e.territory) {
        const x = idx % cols, y = (idx / cols) | 0;
        for (let k = 0; k < 8; k++) {
          const nx = x + DX8[k], ny = y + DY8[k];
          if (nx >= 0 && nx < cols && ny >= 0 && ny < rows) {
            frontier[ny * cols + nx] = 1;
          }
        }
      }

      for (let ei = 0; ei < eligibleLen; ei++) {
        const i = eligible[ei];

        const d = base[i];
        if (d >= Infinity) continue;

        const isFront = frontier[i];
        if (!isFront && !reachableByWaterFromOwnedMemo(e, i, ownerPrev, grid, waterCache)) continue;

        const c = d + EPSD;
        const off = i * K;
        let len = topKLen[i];

        if (len === K) {
          const worstIdx = off + K - 1;
          const worstC = topKCost[worstIdx];
          if (c > worstC + EPS) continue;
          if (c >= worstC - EPS && id >= topKId[worstIdx]) continue;
        }

        let pos = len < K ? len : K - 1;
        while (pos > 0) {
          const prevIdx = off + pos - 1;
          const prevC = topKCost[prevIdx];
          const prevId = topKId[prevIdx];
          if (c < prevC - EPS || (c <= prevC + EPS && id < prevId)) {
            topKCost[off + pos] = prevC;
            topKId[off + pos] = prevId;
            topKFrontier[off + pos] = topKFrontier[prevIdx];
            pos--;
          } else break;
        }
        topKCost[off + pos] = c;
        topKId[off + pos] = id;
        topKFrontier[off + pos] = isFront ? 1 : 0;
        if (len < K) topKLen[i] = len + 1;
      }
    }

    // --- Min-heap (pooled) ---
    const heapCost = pool.heapCost;
    const heapCell = pool.heapCell;
    const heapEmp = pool.heapEmp;
    const heapPtr = pool.heapPtr;
    let H = 0;

    function less(a, b) {
      const da = heapCost[a], db = heapCost[b];
      const diff = da - db;
      if (diff < -EPS) return true;
      if (diff > EPS) return false;
      if (heapCell[a] !== heapCell[b]) return heapCell[a] < heapCell[b];
      return heapEmp[a] < heapEmp[b];
    }

    function swap(a, b) {
      let t;
      t = heapCost[a]; heapCost[a] = heapCost[b]; heapCost[b] = t;
      t = heapCell[a]; heapCell[a] = heapCell[b]; heapCell[b] = t;
      t = heapEmp[a]; heapEmp[a] = heapEmp[b]; heapEmp[b] = t;
      t = heapPtr[a]; heapPtr[a] = heapPtr[b]; heapPtr[b] = t;
    }

    function push(cost, cell, emp, ptr) {
      heapCost[H] = cost; heapCell[H] = cell; heapEmp[H] = emp; heapPtr[H] = ptr;
      let i = H++;
      while (i > 0) {
        const p = (i - 1) >> 1;
        if (less(i, p)) { swap(i, p); i = p; } else break;
      }
    }

    const _popResult = { cost: 0, cell: 0, emp: 0, ptr: 0 };
    function pop() {
      if (H <= 0) return null;
      _popResult.cost = heapCost[0];
      _popResult.cell = heapCell[0];
      _popResult.emp = heapEmp[0];
      _popResult.ptr = heapPtr[0];
      const last = --H;
      if (last > 0) {
        heapCost[0] = heapCost[last];
        heapCell[0] = heapCell[last];
        heapEmp[0] = heapEmp[last];
        heapPtr[0] = heapPtr[last];
        let i = 0;
        while (true) {
          const l = (i << 1) + 1, r = l + 1;
          if (l >= H) break;
          const m = (r < H && less(r, l)) ? r : l;
          if (less(m, i)) { swap(m, i); i = m; } else break;
        }
      }
      return _popResult;
    }

    // Track used candidates (pooled)
    const topKUsed = pool.topKUsed;
    topKUsed.fill(0);

    // Effective cost: (cost + B) * (1 - w * power/globalPMax)
    // B = base cost offset (makes low-cost cells conquerable by high-power empires)
    // Power is globally normalized; cost is raw. No per-cell normalization.
    const pScale = (globalPMax > 0) ? (w / globalPMax) : 0;

    // Pick best (lowest effective cost) candidate for cell i
    function pickBest(i) {
      const off = i * K;
      const len = topKLen[i] | 0;
      if (len <= 0) return null;

      let bestJ = -1;
      let bestEC = Infinity;

      for (let j = 0; j < len; j++) {
        if (topKUsed[off + j]) continue;

        const id2 = topKId[off + j] | 0;
        const c2 = topKCost[off + j] || 0;
        const p2 = P_byId[id2] || 0;

        const ec = (c2 + B) * (1 - pScale * p2);

        if (ec < bestEC) {
          bestEC = ec;
          bestJ = j;
        }
      }

      if (bestJ < 0) return null;
      return { j: bestJ, ec: bestEC };
    }

    // Seed heap
    for (let i = 0; i < N; i++) {
      if (!isLand[i]) continue;
      if ((topKLen[i] | 0) === 0) continue;

      const pick = pickBest(i);
      if (!pick) continue;

      const j = pick.j;
      const id = topKId[i * K + j] | 0;
      topKUsed[i * K + j] = 1;
      push(pick.ec, i, id, j);
    }

    // --- Global auction (pooled owner array) ---
    const owner = pool.owner;
    owner.fill(0);
    while (H > 0 && sumQuota > 0) {
      const it = pop(); if (!it) break;
      const i = it.cell, e = it.emp, p = it.ptr;

      if (owner[i] !== 0) continue;

      if (quota[e] > 0) {
        owner[i] = e;
        quota[e]--; sumQuota--;
        continue;
      }

      // Advance to next best
      const pick = pickBest(i);
      if (pick) {
        const j2 = pick.j;
        const id2 = topKId[i * K + j2] | 0;
        topKUsed[i * K + j2] = 1;
        push(pick.ec, i, id2, j2);
      }
    }

    // --- Rebuild territories ---
    const idToEmp = new Array(maxId + 1);
    for (const e of emps) idToEmp[e.id] = e;

    for (const e of emps) {
      if (e.territory instanceof Set) e.territory.clear();
      else e.territory = new Set();
    }

    for (let i = 0; i < N; i++) {
      const id = owner[i];
      if (id > 0) {
        const e = idToEmp[id];
        if (e) e.territory.add(i);
      }
    }

    // --- Elimination & cleanup ---
    {
      const toRemove = [];
      const ghostCitiesEnabled = window.ghostCitiesEnabled !== false; // default true

      // Capital taken by someone else - only remove if ghost cities disabled
      // (with ghost cities enabled, they stay as trade nodes even if capital cell is owned by another)
      if (!ghostCitiesEnabled) {
        for (const e of emps) {
          if (!e.capital) continue;
          const capIdx = e.capital.y * cols + e.capital.x;
          const capOwner = owner[capIdx] | 0;
          if (capOwner > 0 && capOwner !== e.id) {
            toRemove.push(e);
          }
        }
      }

      // Zero-size empires - only remove if ghost cities disabled
      if (!ghostCitiesEnabled) {
        for (const e of emps) {
          const area = e && e.territory ? e.territory.size : 0;
          if (area === 0) {
            toRemove.push(e);
          }
        }
      }

      // Deduplicate
      const seen = new Set();
      const unique = [];
      for (const e of toRemove) {
        if (!e || seen.has(e.id)) continue;
        seen.add(e.id);
        unique.push(e);
      }

      let removedAny = false;

      for (const e of unique) {
        removedAny = true;

        for (let i = 0; i < N; i++) {
          if (owner[i] === e.id) owner[i] = 0;
        }

        e._dead = true;
        EmpireManager.removeEmpire(e.id);

        const panel = document.getElementById(`empire-panel-${e.id}`);
        if (panel && panel.parentNode) panel.parentNode.removeChild(panel);
      }

      if (removedAny) {
        // Don't null the paths here - let computeTradeRoutes replace them atomically
        // to avoid flickering during recomputation
        window._tradeNeedsRecompute = true;
      }
    }

    // Notify visuals that ownership changed
    window._ownerVersion = (window._ownerVersion | 0) + 1;

    // Expose last auction's scoring data for debug tooltip
    window._lastAuction = {
      topKId: pool.topKId,
      topKCost: pool.topKCost,
      topKFrontier: pool.topKFrontier,
      topKLen: pool.topKLen,
      P_byId,
      globalPMax,
      K, N, w, B,
    };

    return owner;
  }

  // Expose to global scope
  window.getAuctionArrays = getAuctionArrays;
  window.recomputeOwnershipAuctionGlobal = recomputeOwnershipAuctionGlobal;

})();
