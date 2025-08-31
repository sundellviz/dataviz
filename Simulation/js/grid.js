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

    // 3) Forest blobs
    growBlob('FOREST', targets.FOREST);
    growBlob('ICE', targets.ICE);

    // 4) Desert & Mountain blobs
    growBlob('DESERT', targets.DESERT);
    growBlob('MOUNTAIN', targets.MOUNTAIN);
  }
}
