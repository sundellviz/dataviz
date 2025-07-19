// ── Globals ──────────────────────────────────────────────────────────────────
let fps = 60;
const boardsize    = 1000;
const setnumber    = 100;
const numclaws     = 2;
const numlimbs     = 6;
const numshells    = 2;
const nummaxparts  = numclaws + numlimbs + numshells + 1;
const creaturesize = 5;
let speedMultiplier = 1;

let bodyPartsCount    = 10; 

let animationId = null;   // will hold the current RAF handle


// ── Slider hookup (after DOM loaded) ────────────────────────────────────────
const partsSlider  = document.getElementById('partsSlider');
const partsDisplay = document.getElementById('partsDisplay');

partsSlider.addEventListener('input', e => {
  bodyPartsCount = Number(e.target.value);
  partsDisplay.textContent = bodyPartsCount;
});



///// CREATURE CLASS
////////////////////

class Creature {
    // Define the body parts as static members of the class
    static HEART = 'heart';
    static CLAW = 'claw';
    static LIMB = 'limb';
    static SHELL = 'shell'; // new body part

    constructor(x, y, hearts = 0, claws = numclaws, limbs = numlimbs, shells = numshells) { // added shells parameter
        this.x = x;
        this.y = y;
        this.bodyParts = [
            { type: Creature.HEART, count: hearts },
            { type: Creature.LIMB, count: limbs },
            { type: Creature.CLAW, count: claws },
            { type: Creature.SHELL, count: shells } // added shells to the bodyParts
        ];
        this.shape = this.generateShape();
            this.totalRotation = 0; // Add a new property to keep track of the total rotation

    }

    generateShape() {
  const shape = [];
  const directions = [
    { dx: -1, dy: 0 },  { dx: 1, dy: 0 },
    { dx: 0, dy: -1 },  { dx: 0, dy: 1 },
    { dx: -1, dy: -1 }, { dx: 1, dy: -1 },
    { dx: -1, dy: 1 },  { dx: 1, dy: 1 }
  ];
  let x = 0, y = 0;

  // 1) heart at (0,0)
  shape.push({ x, y, type: Creature.HEART });

  // 2) build a pool of exactly `bodyPartsCount` random types
  const types = [Creature.LIMB, Creature.CLAW, Creature.SHELL];
  let pool = [];
  for (let i = 0; i < bodyPartsCount; i++) {
    pool.push(types[Math.floor(Math.random() * types.length)]);
  }

  // 3) shuffle it
  pool = this.shuffleArray(pool);

  // 4) place each part in a random free direction
  for (let type of pool) {
    let dir;
    do {
      dir = directions[Math.floor(Math.random() * directions.length)];
      x  += dir.dx;
      y  += dir.dy;
    } while (this.isOccupied(shape, x, y));
    shape.push({ x, y, type });
  }

  return shape;
}

shuffleArray(array) {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [array[i], array[j]] = [array[j], array[i]];
    }
    return array;
}

    isOccupied(shape, x, y) {
        return shape.some(part => part.x === x && part.y === y);
    }

    move(canvasWidth, canvasHeight) {
        const size = creaturesize; // Assuming the size of each part is 5x5
        const maxParts = nummaxparts; // maximum number of parts a creature can have
        const maxCreatureSize = maxParts * size;



        // If the creature is not currently moving, choose a new direction, rotation and duration
        if (!this.moving) {
            this.moving = true;
            this.direction = { dx: Math.random() - 0.5, dy: Math.random() - 0.5 };
            this.rotation = (Math.random() * 0.0 - 0.0) * Math.PI; // Rotate up to half a circle
            //this.rotation = 0;
            this.moveDuration = Math.floor(Math.random() * 1000);
            //this.moveDuration = 5;
        }

        // If the total rotation is greater than 2*PI (a full circle), reset it
        if (this.totalRotation >= 2 * Math.PI) {
            this.totalRotation -= 2 * Math.PI;
        }

        // Update the creature's position
        this.x += this.direction.dx * speedMultiplier;
this.y += this.direction.dy * speedMultiplier;
        


        // Bounce off the edges of the canvas
        if (this.x < maxCreatureSize || this.x > canvasWidth - maxCreatureSize) {
            this.direction.dx *= -1;
        }
        if (this.x < 100) {
            this.direction.dx *= -1;
        }


        if (this.y < maxCreatureSize || this.y > canvasHeight - maxCreatureSize) {
            this.direction.dy *= -1;
        }

        // Decrease the move duration, and stop moving if it reaches 0
        this.moveDuration--;
        if (this.moveDuration <= 0) {
            this.moving = false;
        }
    }

    draw(ctx) {
        ctx.save(); // Save the current state of the context
        ctx.translate(this.x, this.y); // Move the context to the creature's position
        ctx.rotate(this.totalRotation); // Rotate the context

        // Draw the parts of the creature
        this.shape.forEach(part => {
            switch (part.type) {
                case Creature.HEART:
                    ctx.fillStyle = 'red';
                    break;
                case Creature.CLAW:
                    ctx.fillStyle = 'blue';
                    break;
                case Creature.LIMB:
                    ctx.fillStyle = 'white';
                    break;
                case Creature.SHELL: // added shell case
                    ctx.fillStyle = 'green';
                    break;
            }

            // Assume each part is a 5x5 square
            const size = creaturesize;
            ctx.fillRect(part.x * size, part.y * size, size, size);
        });

        ctx.restore(); // Restore the context to its previous state
    }

    collidesWith(other) {
        const size = creaturesize; // Assuming the size of each part is 5x5
        for (let part1 of this.shape) {
            for (let part2 of other.shape) {
                // Calculate the positions of the parts in the world
                let pos1 = { x: this.x + part1.x * size, y: this.y + part1.y * size };
                let pos2 = { x: other.x + part2.x * size, y: other.y + part2.y * size };

                // Check if the parts collide
                if (Math.abs(pos1.x - pos2.x) < size && Math.abs(pos1.y - pos2.y) < size) {
                    return { part1, part2 };
                }
            }
        }
        return false;
    }

    removeUnconnectedParts() {
        // Depth-first search to find all connected parts
        let visited = new Set();
        let stack = [this.shape.find(part => part.type === Creature.HEART)];

        while (stack.length > 0) {
            let part = stack.pop();
            visited.add(part);

            // Check all parts within one unit of this part
            for (let other of this.shape) {
                if (!visited.has(other)) {
                    let dx = Math.abs(part.x - other.x);
                    let dy = Math.abs(part.y - other.y);

                    // If the other part is within one unit, add it to the stack
                    if (dx <= 1 && dy <= 1) {
                        stack.push(other);
                    }
                }
            }
        }

        // Remove all parts that weren't visited
        this.shape = this.shape.filter(part => visited.has(part));
    }

    transform(victorShape) {
        // Clone the shape
        this.shape = JSON.parse(JSON.stringify(victorShape));
    }

}

// COMPARE SHAPES
function toCanonicalForm(shape) {
    // Find the heart
    let heart = shape.find(part => part.type === Creature.HEART);

    // Translate the shape so that the heart is at the origin, and sort the parts
    let translatedShape = shape.map(part => ({ ...part, x: part.x - heart.x, y: part.y - heart.y }));
    translatedShape.sort((part1, part2) => part1.type + part1.x + part1.y - (part2.type + part2.x + part2.y));

    return translatedShape;
}

function shapesAreIdentical(shape1, shape2) {
    // Convert the shapes to their canonical forms
    shape1 = toCanonicalForm(shape1);
    shape2 = toCanonicalForm(shape2);

    if (shape1.length !== shape2.length) {
        return false;
    }

    // Sort the parts by type and coordinates for comparison
    shape1.sort((part1, part2) => part1.type + part1.x + part1.y - (part2.type + part2.x + part2.y));
    shape2.sort((part1, part2) => part1.type + part1.x + part1.y - (part2.type + part2.x + part2.y));

    // Compare the sorted shapes part by part
    for (let i = 0; i < shape1.length; i++) {
        if (shape1[i].type !== shape2[i].type || shape1[i].x !== shape2[i].x || shape1[i].y !== shape2[i].y) {
            return false;
        }
    }

    // If all parts are the same, the shapes are identical
    return true;
}



//////////////// COLLISIONS


function handleCollisions(creatures) {
    for (let i = 0; i < creatures.length; i++) {
        for (let j = i + 1; j < creatures.length; j++) {
            let collision = creatures[i].collidesWith(creatures[j]);
            if (collision) {

                // If the creatures have identical shapes, they do not destroy each other's limbs
                if (shapesAreIdentical(creatures[i].shape, creatures[j].shape)) {
                    // // Bounce the creatures
                    // let temp = creatures[i].direction;
                    // creatures[i].direction = creatures[j].direction;
                    // creatures[j].direction = temp;
                    continue;
                }

                //if (collision.part1.type === Creature.SHELL || collision.part2.type === Creature.SHELL) {
                //    // Bounce the creatures
                //    let temp = creatures[i].direction;
                //    creatures[i].direction = creatures[j].direction;
                //    creatures[j].direction = temp;
                //    continue;
                //}

                // Handle the collision based on the types of the parts
                if (collision.part1.type === Creature.CLAW && collision.part2.type === Creature.HEART) {
                    // Transform the creature with the heart into the creature with the claw
                    creatures[j].transform(creatures[i].shape);
                } else if (collision.part1.type === Creature.HEART && collision.part2.type === Creature.CLAW) {
                    // Transform the creature with the heart into the creature with the claw
                    creatures[i].transform(creatures[j].shape);
                } else if (collision.part1.type === Creature.CLAW && collision.part2.type === Creature.LIMB) {
                    // Destroy the limb and remove unconnected parts
                    creatures[j].shape.splice(creatures[j].shape.indexOf(collision.part2), 1);
                    creatures[j].removeUnconnectedParts();
                } else if (collision.part1.type === Creature.LIMB && collision.part2.type === Creature.CLAW) {
                    // Destroy the limb and remove unconnected parts
                    creatures[i].shape.splice(creatures[i].shape.indexOf(collision.part1), 1);
                    creatures[i].removeUnconnectedParts();
                } else {
                    // Bounce the creatures
                    let temp = creatures[i].direction;
                    creatures[i].direction = creatures[j].direction;
                    creatures[j].direction = temp;
                }
            }
        }
    }
}



function createCreatures(numCreatures, canvasWidth, canvasHeight) {
    const creatures = [];
    const size = creaturesize; // Assuming the size of each part is 5x5
    const maxParts = nummaxparts; // maximum number of parts a creature can have

    for (let i = 0; i < numCreatures; i++) {
        let x, y, overlaps;
        do {
            overlaps = false;
            x = Math.random() * (canvasWidth - 50 - maxParts * size * 2) + maxParts * size + 50;
            y = Math.random() * (canvasHeight - maxParts * size * 2) + maxParts * size;

            // Check if new creature overlaps with any existing creature
            for (let creature of creatures) {
                if (Math.abs(creature.x - x) < maxParts * size && Math.abs(creature.y - y) < maxParts * size) {
                    overlaps = true;
                    break;
                }
            }
        } while (overlaps);

        creatures.push(new Creature(x, y));
    }

    return creatures;
}



// Scoreboard
function calculateScoreBoard(creatures) {
    const scoreBoard = new Map();

    creatures.forEach(creature => {
        const shape = JSON.stringify(toCanonicalForm(creature.shape));

        if (scoreBoard.has(shape)) {
            scoreBoard.set(shape, scoreBoard.get(shape) + 1);
        } else {
            scoreBoard.set(shape, 1);
        }
    });

    return scoreBoard;
}




function main() {
  const canvas = document.getElementById('gameCanvas');
  canvas.width  = boardsize + 400;
  canvas.height = boardsize;
  const ctx = canvas.getContext('2d');

    // === NEW: create a vertical gradient from lighter blue (top) to darker blue (bottom)
  const bgGradient = ctx.createLinearGradient(0, 0, 0, canvas.height);
  bgGradient.addColorStop(0, '#003366'); // top – slightly lighter blue
  bgGradient.addColorStop(1, '#000022'); // bottom – deep, dark blue

  const creatures = createCreatures(setnumber, canvas.width, canvas.height);

  // timers
  let lastSimTime   = 0;
  let lastScoreTime = 0;

  // cache of the most‐recently computed scoreboard
  let sortedScores = [];

  function update(timestamp) {
    // --- 1) Simulation & draw @ ~fps ---
    if (timestamp - lastSimTime >= 1000 / fps) {
      'ctx.clearRect(0, 0, canvas.width, canvas.height);'
            ctx.fillStyle = bgGradient;
      ctx.fillRect  (0, 0, canvas.width, canvas.height);

      creatures.forEach(c => {
        c.move(canvas.width, canvas.height);
        c.draw(ctx);
      });

      handleCollisions(creatures);

      lastSimTime = timestamp;
    }

    // --- 2) Recompute scoreboard data once per second ---
    if (timestamp - lastScoreTime >= 1000) {
      const scoreBoard = calculateScoreBoard(creatures);
      sortedScores = Array.from(scoreBoard.entries())
                          .sort((a, b) => b[1] - a[1]);
      lastScoreTime = timestamp;
    }

    // --- 3) Always render the (cached) scoreboard ---
    ctx.save();
    ctx.fillStyle = 'white';
    ctx.font      = '20px Arial';
    ctx.fillText('Top 5 shapes:', 10, 30);

    sortedScores.slice(0, 5).forEach(([shapeJSON, count], i) => {
      ctx.fillStyle = 'white';
      ctx.fillText(`${count}`, 10, 60 + i * 50);
      const shape = JSON.parse(shapeJSON);
      shape.forEach(part => {
        switch (part.type) {
          case Creature.HEART: ctx.fillStyle = 'red';   break;
          case Creature.CLAW:  ctx.fillStyle = 'blue';  break;
          case Creature.LIMB:  ctx.fillStyle = 'white'; break;
          case Creature.SHELL: ctx.fillStyle = 'green'; break;
        }
        ctx.fillRect(
          50 + part.x * creaturesize,
          50 + i * 50 + part.y * creaturesize,
          creaturesize, creaturesize
        );
      });
    });
    ctx.restore();

    ' // --- 4) Schedule next frame (always!) --- '
    ' requestAnimationFrame(update); '

    // schedule next frame and keep the handle
    animationId = requestAnimationFrame(update);

  }

  '// kick off the loop '
  ' requestAnimationFrame(update); '

    // start the loop
  animationId = requestAnimationFrame(update);
}

// ── Wire up your speed buttons just once ─────────────────────────────────────
document.getElementById('speedUpButton')
        .addEventListener('click', () => speedMultiplier *= 1.1);

document.getElementById('slowDownButton')
        .addEventListener('click', () => speedMultiplier /= 1.1);

document.getElementById('resetButton')
  .addEventListener('click', () => {
    // 1) stop the old loop
    if (animationId !== null) {
      cancelAnimationFrame(animationId);
    }
    // 2) restart with a fresh population
    main();
  });

// ── Start the sim ──────────────────────────────────────────────────────────
main();