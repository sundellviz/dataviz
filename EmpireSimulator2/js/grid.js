// js/grid.js

// dimensions and default cell size
const COLS = 50;
const ROWS = 50;

// the set of possible terrains
const TERRAIN = {
  PLAIN:    { id: 0, color: '#A8D5BA', speed: 0.3 },
  DESERT:   { id: 1, color: '#E4C97E', speed: 1.0 },
  WATER:    { id: 2, color: '#4A90E2', speed: 0.4 },
  MOUNTAIN: { id: 3, color: '#6E6E6E', speed: 1.0 },
  FOREST:   { id: 4, color: '#2E8B57', speed: 0.8 },
  SHRUB:    { id: 4, color: '#9c8d62', speed: 1.0 },
  RIVER:    { id: 5, color: '#5BC0EB', speed: 0.2 },
  ICE:      { id: 6, color: '#EEEEEE', speed: 1.0 }
};

// Insert this right below:
const TERRAIN_VARIANTS = {
  PLAIN:  ['#DCE775','#D4E157','#F6EE9C'],
  DESERT: ['#FFB74D','#FFA726','#FFCC80'],
  WATER:  ['#42A5F5','#2196F3','#64B5F6'],
  MOUNTAIN: ['#8D6E63','#7D5A50','#A1887F'],
  FOREST: ['#388E3C','#2E7D32','#66BB6A'],
  SHRUB : ['#9c8d62','#ad9c6d','#807350'],
  RIVER:  ['#29B6F6','#03A9F4','#4FC3F7'],
  ICE:    ['#EEEEFF','#DDDDFF','#CCCCFF'],
};

class Grid {
  constructor(cols = COLS, rows = ROWS) {
    this.cols = cols;
    this.rows = rows;
    this.reset();
  }

  reset() {
    this.cells = Array.from({ length: this.rows }, () =>
      Array.from({ length: this.cols }, () => ({ terrain: 'PLAIN', owner: 0 }))
    );
  }




  /**
   * Populate the grid according to normalized weights.
   * Creates:
   *  1) Water blobs via a snake + blob top‑up
   *  2) River snakes through water
   *  3) Forest blobs
   *  4) Desert blobs
   *  5) Mountain blobs
   */
  randomize(weights) {
    const total = this.cols * this.rows;
    // compute target counts for each terrain
    const sumW = Object.values(weights).reduce((a, b) => a + (b || 0), 0);
    const targets = {
      PLAIN:    0,
      DESERT:   0,
      WATER:    0,
      MOUNTAIN: 0,
      FOREST:   0,
      SHRUB:    0,
      RIVER:    0,
      ICE:      0    // ← add ICE here
    };
    for (let k in targets) {
      targets[k] = Math.floor(((weights[k] || 0) / sumW) * total);
    }

    // start all PLAIN
    this.reset();

    // helper to grow clustered blobs
    const growBlob = (type, count) => {
      if (count <= 0) return;
      const frontier = [];
      const mark = (x, y) => {
        this.cells[y][x].terrain = type;
        frontier.push({ x, y });
      };
      // seeding: ~2% of target count
      let seeds = Math.max(1, Math.floor(count * 0.02));
      while (seeds--) {
        const x = Math.floor(Math.random() * this.cols);
        const y = Math.floor(Math.random() * this.rows);
        if (this.cells[y][x].terrain === 'PLAIN') mark(x, y);
      }
      let placed = frontier.length;
      const dirs4 = [
        { dx: 1, dy: 0 }, { dx: -1, dy: 0 },
        { dx: 0, dy: 1 }, { dx: 0, dy: -1 }
      ];
      while (placed < count && frontier.length) {
        const idx = Math.floor(Math.random() * frontier.length);
        const { x, y } = frontier.splice(idx, 1)[0];
        // shuffle directions
        dirs4.sort(() => Math.random() - 0.5).forEach(d => {
          if (placed >= count) return;
          const nx = x + d.dx, ny = y + d.dy;
          if (
            nx >= 0 && nx < this.cols &&
            ny >= 0 && ny < this.rows &&
            this.cells[ny][nx].terrain === 'PLAIN'
          ) {
            mark(nx, ny);
            placed++;
          }
        });
      }
    };

    // 1) Water blobs via a “snake” + blob top‑up
    const directions8 = [
      { dx: 1, dy: 0 }, { dx: -1, dy: 0 },
      { dx: 0, dy: 1 }, { dx: 0, dy: -1 },
      { dx: 1, dy: 1 }, { dx: 1, dy: -1 },
      { dx: -1, dy: 1 }, { dx: -1, dy: -1 }
    ];
    let waterPlaced = 0, attempts = 0, maxAttempts = total * 5;
    while (waterPlaced < targets.WATER && attempts++ < maxAttempts) {
      // random edge seed
      let x, y;
      const side = Math.floor(Math.random() * 4);
      if (side === 0) { x = 0; y = Math.random()*this.rows|0; }
      else if (side === 1) { x = this.cols-1; y = Math.random()*this.rows|0; }
      else if (side === 2) { x = Math.random()*this.cols|0; y = 0; }
      else { x = Math.random()*this.cols|0; y = this.rows-1; }
      if (['DESERT','MOUNTAIN'].includes(this.cells[y][x].terrain)) continue;

      let prev = null;
      while (waterPlaced < targets.WATER) {
        if (['PLAIN','RIVER'].includes(this.cells[y][x].terrain)) {
          this.cells[y][x].terrain = 'WATER';
          waterPlaced++;
        }
        // carve a width‑3 snake
        if (prev) {
          const perp = [
            { dx: -prev.dy, dy: prev.dx },
            { dx:  prev.dy, dy: -prev.dx }
          ];
          perp.forEach(d => {
            const wx = x + d.dx, wy = y + d.dy;
            if (
              wx>=0&&wx<this.cols&&wy>=0&&wy<this.rows &&
              !['DESERT','MOUNTAIN'].includes(this.cells[wy][wx].terrain)
            ) {
              this.cells[wy][wx].terrain = 'WATER';
              waterPlaced++;
            }
          });
        }
        // pick next step
        const valids = directions8.filter(d => {
          const nx = x+d.dx, ny = y+d.dy;
          return (
            nx>=0&&nx<this.cols&&ny>=0&&ny<this.rows &&
            !(prev && d.dx===-prev.dx && d.dy===-prev.dy) &&
            !['DESERT','MOUNTAIN'].includes(this.cells[ny][nx].terrain)
          );
        });
        if (!valids.length) break;
        const dir = valids[Math.random()*valids.length|0];
        prev = dir;
        x += dir.dx; y += dir.dy;
      }
    }
    // top‑up any missing water with blobs
    const actualWater = this.cells.flat().filter(c => c.terrain==='WATER').length;
    const remW = targets.WATER - actualWater;
    if (remW > 0) growBlob('WATER', remW);

    // 2) Rivers as thin snakes through water
    let riverPlaced = 0, rAttempts = 0;
    while (riverPlaced < targets.RIVER && rAttempts++ < maxAttempts) {
      // pick random water cell as head
      const waters = [];
      this.cells.forEach((row,y) => row.forEach((c,x) => {
        if (c.terrain==='WATER') waters.push({x,y});
      }));
      if (!waters.length) break;
      let {x,y} = waters[Math.random()*waters.length|0];
      let prev2 = null;
      while (riverPlaced < targets.RIVER) {
        this.cells[y][x].terrain = 'RIVER';
        riverPlaced++;
        const valids2 = directions8.filter(d => {
          const nx = x+d.dx, ny = y+d.dy;
          if (nx<0||nx>=this.cols||ny<0||ny>=this.rows) return false;
          if (prev2 && d.dx===-prev2.dx && d.dy===-prev2.dy) return false;
          if (this.cells[ny][nx].terrain!=='PLAIN') return false;
          // avoid branching
          const neighbors = directions8.reduce((cnt,o) => {
            const ox=nx+o.dx, oy=ny+o.dy;
            return cnt + ((ox>=0&&ox<this.cols&&oy>=0&&oy<this.rows&&this.cells[oy][ox].terrain==='RIVER')?1:0);
          }, 0);
          return neighbors <= 1;
        });
        if (!valids2.length) break;
        prev2 = valids2[Math.random()*valids2.length|0];
        x += prev2.dx; y += prev2.dy;
      }
    }

    // 3) Forest, Shrub and Ice blobs
    growBlob('FOREST', targets.FOREST);
    growBlob('SHRUB', targets.SHRUB);
    growBlob('ICE', targets.ICE);

    // 4) Desert & Mountain blobs
    growBlob('DESERT', targets.DESERT);
    growBlob('MOUNTAIN', targets.MOUNTAIN);
  }
}

// === Land Value Layer (attach to Grid after the class definition) ===============

// '0'..'9' => 0..9, 'a'..'z' => 10..35, 'A'..'Z' => 36..61
function charToVal(ch) {
  if (ch >= '0' && ch <= '9') return ch.charCodeAt(0) - 48;
  if (ch >= 'a' && ch <= 'z') return 10 + (ch.charCodeAt(0) - 97);
  if (ch >= 'A' && ch <= 'Z') return 36 + (ch.charCodeAt(0) - 65);
  return 0;
}
function valToChar(v) {
  v = Math.max(0, Math.min(61, Math.floor(v)));
  if (v <= 9)  return String.fromCharCode(48 + v);
  if (v <= 35) return String.fromCharCode(97 + (v - 10));
  return String.fromCharCode(65 + (v - 36));
}

// Create / reset the layer with a default value (0..61)
Grid.prototype.initValueLayer = function(defaultVal = 1) {
  this.valueLayer = Array.from({ length: this.rows }, () =>
    new Array(this.cols).fill(Math.max(0, Math.min(61, Math.floor(defaultVal))))
  );
};

// Safe accessors
Grid.prototype.getValueAt = function(x, y) {
  if (!this.valueLayer) return 0;
  if (y < 0 || y >= this.rows || x < 0 || x >= this.cols) return 0;
  return this.valueLayer[y][x] ?? 0;
};
Grid.prototype.setValueAt = function(x, y, v) {
  if (!this.valueLayer) return;
  if (y < 0 || y >= this.rows || x < 0 || x >= this.cols) return;
  this.valueLayer[y][x] = Math.max(0, Math.min(61, Math.floor(v)));
};

// Import a .txt value layer.
// Supports EITHER:
//  (A) one-char-per-cell, no spaces (old behavior), or
//  (B) space-delimited tokens per row (what R writes with sep=" ").
Grid.prototype.importValueLayerFromText = function(txt) {
  // Normalize line endings & strip BOM
  const clean = txt.replace(/^\uFEFF/, '').replace(/\r/g, '');
  const lines = clean.split('\n').filter(l => l.trim().length > 0);

  if (lines.length !== this.rows) {
    alert(`Value file rows ${lines.length} do not match grid rows ${this.rows}.`);
    return false;
  }

  // If the first non-empty line has whitespace inside, treat as space-delimited
  const isDelimited = /\s/.test(lines[0].trim());

  this.initValueLayer(0);

  if (isDelimited) {
    // SPACE-DELIMITED: each row should have exactly this.cols tokens
    for (let y = 0; y < this.rows; y++) {
      const tokens = lines[y].trim().split(/\s+/); // handles multiple spaces/tabs
      if (tokens.length !== this.cols) {
        alert(`Value file columns (row ${y}) ${tokens.length} do not match grid cols ${this.cols}.`);
        return false;
      }
      for (let x = 0; x < this.cols; x++) {
        const tok = tokens[x];

        // Accept either single-char codes or numeric codes like "0..60".
        // - If 1 char -> use your existing charToVal()
        // - If number -> use it directly (or adapt if you map differently)
        const n = Number(tok);
        this.valueLayer[y][x] =
          tok.length === 1 && !Number.isFinite(n) ? charToVal(tok)
        : Number.isFinite(n) ? n
        : charToVal(tok[0]); // last-resort fallback
      }
    }
  } else {
    // ONE-STRING-PER-ROW (no spaces): length must equal this.cols
    if (lines.some(l => l.length !== this.cols)) {
      alert(`Value file columns ${lines[0]?.length ?? 0} do not match grid cols ${this.cols}.`);
      return false;
    }
    for (let y = 0; y < this.rows; y++) {
      const row = lines[y];
      for (let x = 0; x < this.cols; x++) {
        this.valueLayer[y][x] = charToVal(row[x]);
      }
    }
  }

  return true;
};

// Export current layer to .txt
Grid.prototype.exportValueLayerToText = function() {
  if (!this.valueLayer) return '';
  let out = '';
  for (let y = 0; y < this.rows; y++) {
    let line = '';
    for (let x = 0; x < this.cols; x++) line += valToChar(this.valueLayer[y][x]);
    out += line + (y < this.rows - 1 ? '\n' : '');
  }
  return out;
};

// Optional: if you want reset() to also clear land value, uncomment:
// const _reset = Grid.prototype.reset;
// Grid.prototype.reset = function() {
//   _reset.call(this);
//   this.initValueLayer(0);
// };
