/*
 * js/pathfindingWorker.js
 * Web Worker for Dijkstra pathfinding to offload computeCostMap
 */

// Movement directions & penalty
const DIAGONALS = [
  [ 1,  0], [-1,  0], [ 0,  1], [ 0, -1],
  [ 1,  1], [ 1, -1], [-1,  1], [-1, -1]
];
const CAPTURE_PENALTY = 1000;



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
class MinHeap {
  constructor(){ this.data = [] }
  push(node) {
    this.data.push(node);
    let i = this.data.length - 1;
    while (i > 0) {
      const p = Math.floor((i-1)/2);
      if (this.data[p].cost <= this.data[i].cost) break;
      [this.data[p], this.data[i]] = [this.data[i], this.data[p]];
      i = p;
    }
  }
  pop() {
    const top = this.data[0];
    const last = this.data.pop();
    if (this.data.length) {
      this.data[0] = last;
      let i = 0;
      while (true) {
        let l = 2*i+1, r = 2*i+2, smallest = i;
        if (l < this.data.length && this.data[l].cost < this.data[smallest].cost) smallest = l;
        if (r < this.data.length && this.data[r].cost < this.data[smallest].cost) smallest = r;
        if (smallest === i) break;
        [this.data[i], this.data[smallest]] = [this.data[smallest], this.data[i]];
        i = smallest;
      }
    }
    return top;
  }
  get size() { return this.data.length }
}

// Worker entry point
self.onmessage = function(e) {
const {
  id,
  empireId,
  rows, cols,
  terrains,         // 2D array of terrain strings
  travelSpeeds,     // object { PLAIN:2.0, ... }
  capital,          // { x, y }

  // NEW:
  ownerIdFlat,      // Int32Array
  penaltyScale,     // number
  penaltyGamma      // number
} = e.data;

// NEW: per-empire hostile-union depth
const hostileDepthFlat = computeHostileDepth(rows, cols, ownerIdFlat, empireId);

  const N = rows * cols;
  // Flatten buffers
  const dist    = new Float32Array(N).fill(Infinity);
  const visited = new Uint8Array(N);
  const parentX = new Int16Array(N);
  const parentY = new Int16Array(N);

  if (capital == null) {
    // no capital: return infinite maps
    const costMap = Array.from({ length: rows }, () => Array(cols).fill(Infinity));
    const parentMap = Array.from({ length: rows }, () => Array(cols).fill(null));
    self.postMessage({ id, empireId, costMap, parentMap });
    return;
  }

  
  // Seed Dijkstra
  const startIdx = capital.y * cols + capital.x;
  dist[startIdx] = 0;
  parentX[startIdx] = -1;          // ← add
parentY[startIdx] = -1;          // ← add

  const pq = new MinHeap();
  pq.push({ idx: startIdx, cost: 0 });

  // Main Dijkstra
  while (pq.size) {
    const { idx, cost } = pq.pop();
    if (visited[idx]) continue;
    visited[idx] = 1;
    const x = idx % cols;
    const y = (idx / cols) | 0;

    for (let [dx, dy] of DIAGONALS) {
      const nx = x + dx, ny = y + dy;
      if (nx < 0 || nx >= cols || ny < 0 || ny >= rows) continue;
      const terr = terrains[ny][nx];
const baseRaw = travelSpeeds[terr];
const base = (baseRaw > 0 && Number.isFinite(baseRaw)) ? baseRaw : 1;
const step = (dx && dy) ? base * Math.SQRT2 : base;
      let newCost = cost + step;
      const nIdx = ny * cols + nx;
      
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
        pq.push({ idx: nIdx, cost: newCost });
      }
    }
  }

  
  // Build 2D maps
  const costMap = [];
  const parentMap = [];
for (let ry = 0; ry < rows; ry++) {
  const rowCost = new Array(cols);
  const rowPar  = new Array(cols);
  for (let cx = 0; cx < cols; cx++) {
    const idx = ry * cols + cx;
    rowCost[cx] = dist[idx];
    rowPar[cx]  = (parentX[idx] === -1) ? null : { x: parentX[idx], y: parentY[idx] };
  }
  costMap.push(rowCost);
  parentMap.push(rowPar);
}

  // Send result back to main thread
  self.postMessage({ id, empireId, costMap, parentMap });
};