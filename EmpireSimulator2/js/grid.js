// js/grid.js

// dimensions and default cell size
const COLS = 50;
const ROWS = 50;

// the set of possible terrains
const TERRAIN = {
  PLAIN:    { id: 0, color: '#A8D5BA', speed: 0.3 },
  DESERT:   { id: 1, color: '#E4C97E', speed: 1.0 },
  WATER:    { id: 2, color: '#74B9FF', speed: 0.4 },
  OCEAN:    { id: 8, color: '#1E5AA8', speed: 0.5 },
  MOUNTAIN: { id: 3, color: '#6E6E6E', speed: 1.0 },
  FOREST:   { id: 4, color: '#2E8B57', speed: 0.8 },
  SHRUB:    { id: 5, color: '#9c8d62', speed: 1.0 },
  RIVER:    { id: 6, color: '#5BC0EB', speed: 0.2 },
  ICE:      { id: 7, color: '#EEEEEE', speed: 1.0 }
};

// Insert this right below:
const TERRAIN_VARIANTS = {
  PLAIN:  ['#DCE775','#D4E157','#F6EE9C'],
  DESERT: ['#FFB74D','#FFA726','#FFCC80'],
  WATER:  ['#8CCBFF','#74B9FF','#5AAEFF'],
  OCEAN:  ['#1E5AA8','#1A4F93','#2367BE'],
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
Grid.prototype.initValueLayer = function(defaultVal = 11) {
  this.valueLayer = Array.from({ length: this.rows }, () =>
    new Array(this.cols).fill(Math.max(0, Math.min(61, Math.floor(defaultVal))))
  );
};

// Safe accessors
Grid.prototype.getValueAt = function(x, y) {
  if (!this.valueLayer) return 11;
  if (y < 0 || y >= this.rows || x < 0 || x >= this.cols) return 0;
  return this.valueLayer[y][x] ?? 0;
};
Grid.prototype.setValueAt = function(x, y, v) {
  if (!this.valueLayer) return;
  if (y < 0 || y >= this.rows || x < 0 || x >= this.cols) return;
  this.valueLayer[y][x] = Math.max(0, Math.min(61, Math.floor(v)));
  // Increment revision counter for cache invalidation
  window._valueLayerRev = (window._valueLayerRev || 0) + 1;
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

  // Invalidate cache
  window._valueLayerRev = (window._valueLayerRev || 0) + 1;
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

// Expose to global scope
window.Grid = Grid;
window.TERRAIN = TERRAIN;
window.TERRAIN_VARIANTS = TERRAIN_VARIANTS;
