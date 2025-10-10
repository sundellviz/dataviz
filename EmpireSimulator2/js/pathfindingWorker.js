/*
 * js/pathfindingWorker.js
 * Web Worker for Dijkstra pathfinding to offload computeCostMap
 */


const TERRAIN_TO_CODE = Object.freeze({
  PLAIN: 0, DESERT: 1, WATER: 2, MOUNTAIN: 3,
  FOREST: 4, SHRUB: 5, RIVER: 6, ICE: 7
});
const NUM_TERRAINS = 8;

// Movement directions & penalty
const DIAGONALS = [
  [ 1,  0], [-1,  0], [ 0,  1], [ 0, -1],
  [ 1,  1], [ 1, -1], [-1,  1], [-1, -1]
];
const CAPTURE_PENALTY = 1000;

// TEMP rollout flag; set to false to disable typed arrays instantly.
const USE_TYPED_ARRAYS = true;



function computeHostileDepth(rows, cols, ownerIdFlat, empireId) {
  const N = rows * cols;
  const dist = new Int32Array(N);
  dist.fill(1e9);

  // outside = your empire + neutral + water (ownerId==0 or == empireId)
  // hostile = everything else (ownerId != 0 && != empireId)
  const q = new Int32Array(N);
  let head = 0, tail = 0;

  // Seed all "outside" cells at distance 0
  for (let i = 0; i < N; i++) {
    const owner = ownerIdFlat[i] | 0;
    if (owner === 0 || owner === empireId) {
      dist[i] = 0;
      q[tail++] = i;
    }
  }

  // 4-neighbor BFS that flows only into hostile cells
  const step = (i, dx, dy) => {
    const x = i % cols, y = (i / cols) | 0;
    const nx = x + dx, ny = y + dy;
    if (nx < 0 || nx >= cols || ny < 0 || ny >= rows) return -1;
    return ny * cols + nx;
  };

  const dirs = [1,0,-1,0, 0,1,0,-1]; // dx,dy pairs

  while (head < tail) {
    const i = q[head++];
    const d = dist[i] + 1;

    for (let k = 0; k < 8; k += 2) {
      const j = step(i, dirs[k], dirs[k+1]);
      if (j < 0) continue;
      const ownerJ = ownerIdFlat[j] | 0;
      if (ownerJ === 0 || ownerJ === empireId) continue; // don't enter "outside"
      if (d < dist[j]) {
        dist[j] = d;
        q[tail++] = j;
      }
    }
  }

  // Convert to "depth inside hostile union": border → 0 (dist==1 → 0)
  const depth = new Float32Array(N);
  for (let i = 0; i < N; i++) {
    const owner = ownerIdFlat[i] | 0;
    if (owner !== 0 && owner !== empireId) {
      const inner = Math.max(0, dist[i] - 1);
      depth[i] = inner;
    }
  }
  return depth;
}



// Simple min-heap
// Simple min-heap (parallel arrays; no per-node objects)
class MinHeap {
  constructor() {
    this._idx  = [];   // heap nodes' cell indices
    this._cost = [];   // matching costs
    this._n    = 0;    // heap size
  }
  get size() { return this._n; }   // keep the same API as before

  push(idx, cost) {
    let k = this._n++;
    this._idx[k]  = idx;
    this._cost[k] = cost;
    // sift up
    while (k > 0) {
      const p = (k - 1) >> 1;
      if (this._cost[p] <= this._cost[k]) break;
      // swap (idx + cost) with parent
      [this._idx[p],  this._idx[k]]  = [this._idx[k],  this._idx[p]];
      [this._cost[p], this._cost[k]] = [this._cost[k], this._cost[p]];
      k = p;
    }
  }

  // Returns just the index; read the current cost from dist[idx]
  pop() {
    if (this._n === 0) return -1;
    const topIdx = this._idx[0];
    const lastI = this._idx[--this._n];
    const lastC = this._cost[this._n];
    if (this._n > 0) {
      this._idx[0]  = lastI;
      this._cost[0] = lastC;
      // sift down
      let i = 0;
      while (true) {
        let l = 2 * i + 1, r = l + 1, s = i;
        if (l < this._n && this._cost[l] < this._cost[s]) s = l;
        if (r < this._n && this._cost[r] < this._cost[s]) s = r;
        if (s === i) break;
        [this._idx[i],  this._idx[s]]  = [this._idx[s],  this._idx[i]];
        [this._cost[i], this._cost[s]] = [this._cost[s], this._cost[i]];
        i = s;
      }
    }
    return topIdx;
  }
}

// Worker entry point
self.onmessage = function(e) {
// Unpack (no 'terrains' required now)
const data = e.data;
const {
  id, empireId,
  rows, cols,
  travelSpeeds,
  capital,
  ownerIdFlat,
  penaltyScale,
  penaltyGamma
} = data;
const N = rows * cols;

// Prefer pre-encoded terrain codes from main; fallback to strings if missing
let terrainCodeFlat = data.terrainCodeFlat;
if (!(terrainCodeFlat instanceof Uint8Array)) {
  // Fallback: build from string grid
  terrainCodeFlat = new Uint8Array(N);
  const terr2D = data.terrains; // only used in fallback
  for (let y = 0, i = 0; y < rows; y++) {
    const row = terr2D[y];
    for (let x = 0; x < cols; x++, i++) {
      terrainCodeFlat[i] = (TERRAIN_TO_CODE[row[x]] ?? 0) | 0; // 0..7
    }
  }
} else {
  // Normalize code range if main encoded 1..8 (we use 0..7 here)
  let max = 0;
  for (let i = 0; i < terrainCodeFlat.length; i++) if (terrainCodeFlat[i] > max) max = terrainCodeFlat[i];
  if (max >= 8) {
    const norm = new Uint8Array(N);
    for (let i = 0; i < N; i++) {
      const c = terrainCodeFlat[i];
      norm[i] = c ? (c - 1) : 0;
    }
    terrainCodeFlat = norm;
  }
}

// Per-code base speed table (read once from travelSpeeds)
const speedByCode = new Float64Array(NUM_TERRAINS);
for (const [name, code] of Object.entries(TERRAIN_TO_CODE)) {
  const v = travelSpeeds[name];
  speedByCode[code] = (v > 0 && Number.isFinite(v)) ? v : 1;
}

// NEW: per-empire hostile-union depth
const hostileDepthFlat = computeHostileDepth(rows, cols, ownerIdFlat, empireId);

  // Flatten buffers
  const dist    = new Float32Array(N).fill(Infinity);
  const visited = new Uint8Array(N);
  const parentX = new Int16Array(N);
  const parentY = new Int16Array(N);

if (capital == null) {
    const N = rows * cols;
    const dist64 = new Float64Array(N).fill(Infinity);
    const parentIdx = new Int32Array(N).fill(-1);
    self.postMessage({ id, empireId, dist64, parentIdx, costMap: null, parentMap: null },
                     [dist64.buffer, parentIdx.buffer]);
  return;
}

  
  // Seed Dijkstra
  const startIdx = capital.y * cols + capital.x;
  dist[startIdx] = 0;
  parentX[startIdx] = -1;          // ← add
parentY[startIdx] = -1;          // ← add

  const pq = new MinHeap();
  pq.push(startIdx, 0);


  // Main Dijkstra
  while (pq.size) {
    const idx = pq.pop();
if (idx === -1) break;          // heap empty (defensive)
if (visited[idx]) continue;
visited[idx] = 1;
const cost = dist[idx];         // read the authoritative cost
    const x = idx % cols;
    const y = (idx / cols) | 0;

    for (let [dx, dy] of DIAGONALS) {
      const nx = x + dx, ny = y + dy;
      if (nx < 0 || nx >= cols || ny < 0 || ny >= rows) continue;

const nIdx = ny * cols + nx;        // (move this line above if needed)
const code = terrainCodeFlat[nIdx];
const baseRaw = speedByCode[code];

const base = (baseRaw > 0 && Number.isFinite(baseRaw)) ? baseRaw : 1;
const step = (dx && dy) ? base * Math.SQRT2 : base;
      let newCost = cost + step;
      
// Depth-based territorial penalty: 0 at border, grows inward
const owner = ownerIdFlat[nIdx] | 0;
if (owner !== 0 && owner !== empireId) {
  const d = hostileDepthFlat[nIdx] || 0; // 0 at hostile frontier, grows inward
  const s = (penaltyScale > 0 ? penaltyScale : 0);
  const g = (penaltyGamma > 0 ? penaltyGamma : 1);
  if (d > 0 && s > 0) {
    //newCost += step * s * Math.pow(d, g); // Change here from multiplicative to additive
    newCost += s * Math.pow(d, g);
  }
}




      if (newCost < dist[nIdx]) {
        dist[nIdx] = newCost;
        parentX[nIdx] = x;
        parentY[nIdx] = y;
        pq.push(nIdx, newCost);
      }
    }
  }

  // ── Micro clean-up: free heap storage before building big maps ──
  pq._idx.length  = 0;
  pq._cost.length = 0;
  pq._n           = 0;
  
// Pack as Float32 to halve RAM + transfer cost
const dist32 = new Float32Array(N);
for (let i = 0; i < N; i++) dist32[i] = dist[i];

const parentIdx = new Int32Array(N);
for (let i = 0; i < N; i++) {
  const px = parentX[i];
  parentIdx[i] = (px === -1) ? -1 : (parentY[i] * cols + px);
}

self.postMessage(
  { id, empireId, dist: dist32, parentIdx },
  [dist32.buffer, parentIdx.buffer]
);
};