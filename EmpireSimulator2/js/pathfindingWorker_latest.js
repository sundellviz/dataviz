/*
 * js/pathfindingWorker.js
 * Web Worker for Dijkstra pathfinding to offload computeCostMap
 */


const TERRAIN_TO_CODE = Object.freeze({
  PLAIN: 0, DESERT: 1, WATER: 2, MOUNTAIN: 3,
  FOREST: 4, SHRUB: 5, RIVER: 6, ICE: 7, OCEAN: 8
});
const NUM_TERRAINS = 9;

// Movement directions & penalty
const DIAGONALS = [
  [ 1,  0], [-1,  0], [ 0,  1], [ 0, -1],
  [ 1,  1], [ 1, -1], [-1,  1], [-1, -1]
];


let _cachedTerrain = null;
let _cachedRows = 0;
let _cachedCols = 0;




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
const data = e.data || {};

if (data.type === 'setTerrain') {
  _cachedRows = data.rows | 0;
  _cachedCols = data.cols | 0;
  _cachedTerrain = data.terrainCodeFlat; // Uint8Array (buffer transferred)
  return;
}

const {
  id, empireId,
  rows, cols,
  travelSpeeds,
  capital,
} = data;
const N = rows * cols;

// Prefer pre-encoded terrain codes from main; fallback to strings if missing
let terrainCodeFlat = data.terrainCodeFlat;
if (!(terrainCodeFlat instanceof Uint8Array)) {
  terrainCodeFlat = _cachedTerrain;
}

if (!(terrainCodeFlat instanceof Uint8Array)) {
  throw new Error('Worker has no cached terrain; did you forget to send type:"setTerrain"?');
}


// Per-code base speed table (read once from travelSpeeds)
const speedByCode = new Float64Array(NUM_TERRAINS);
for (const [name, code] of Object.entries(TERRAIN_TO_CODE)) {
  const v = travelSpeeds[name];
  speedByCode[code] = (v > 0 && Number.isFinite(v)) ? v : 1;
}

// Single per-step penalty when a move changes terrain (empire-specific)
const switchCost = (travelSpeeds && Number.isFinite(+travelSpeeds.SWITCH)) ? +travelSpeeds.SWITCH : 0;


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
let step   = (dx && dy) ? base * Math.SQRT2 : base;

// NEW: add SWITCH only when terrain changes (and only if > 0)
if (switchCost > 0) {
  const fromCode = terrainCodeFlat[idx];  // current cell's terrain code
  if (fromCode !== code) {
    step += switchCost;
  }
}

let newCost = cost + step;



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