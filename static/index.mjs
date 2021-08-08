import * as rts from './rts.mjs';
import module from './wasm-game-of-life.wasm.mjs';
import req from './wasm-game-of-life.req.mjs';

const CELL_SIZE = 7; // px
const GRID_COLOR = "#CCCCCC";
const DEAD_COLOR = "#FFFFFF";
const ALIVE_COLOR = "#000000";


const fps = new class {
  constructor() {
    this.fps = document.getElementById("fps");
    this.frames = [];
    this.lastFrameTimeStamp = performance.now();
  }

  render() {
    const now = performance.now();
    const delta = now - this.lastFrameTimeStamp;
    this.lastFrameTimeStamp = now;
    const fps = 1 / delta * 1000;

    this.frames.push(fps);
    if (this.frames.length > 100) {
      this.frames.shift();
    }

    let min = Infinity;
    let max = -Infinity;
    let sum = 0;
    for (let i = 0; i < this.frames.length; i++) {
      sum += this.frames[i];
      min = Math.min(this.frames[i], min);
      max = Math.max(this.frames[i], max);
    }
    let mean = sum / this.frames.length;

    this.fps.textContent = `
Frames per Second:
         latest = ${Math.round(fps)}
avg of last 100 = ${Math.round(mean)}
min of last 100 = ${Math.round(min)}
max of last 100 = ${Math.round(max)}
`.trim();
  }
};


async function handleModule(m) {
  const asterius = await rts.newAsteriusInstance(Object.assign(req, { module: m }));

  let universe = await asterius.exports.randomUniverse();

  const height = await asterius.exports.getHeight();
  const width = await asterius.exports.getWidth();

  const canvas = document.getElementById("game-of-life-canvas");
  canvas.height = (CELL_SIZE + 1) * height + 1;
  canvas.width = (CELL_SIZE + 1) * width + 1;
  const ctx = canvas.getContext('2d');

  const drawGrid = () => {
    ctx.beginPath();
    ctx.strokeStyle = GRID_COLOR;

    for (let i = 0; i <= width; i++) {
      ctx.moveTo(i * (CELL_SIZE + 1) + 1, 0);
      ctx.lineTo(i * (CELL_SIZE + 1) + 1, (CELL_SIZE + 1) * height + 1);
    }

    for (let j = 0; j <= height; j++) {
      ctx.moveTo(0,                           j * (CELL_SIZE + 1) + 1);
      ctx.lineTo((CELL_SIZE + 1) * width + 1, j * (CELL_SIZE + 1) + 1);
    }

    ctx.stroke();
  };

  const drawCells = () => {

    ctx.beginPath();

    for (let row = 0; row < height; row++) {
      for (let col = 0; col < width; col++) {
        const idx = row * width + col;

        ctx.fillStyle = universe[idx] === '0'
          ? DEAD_COLOR
          : ALIVE_COLOR;

        ctx.fillRect(
          col * (CELL_SIZE + 1) + 1,
          row * (CELL_SIZE + 1) + 1,
          CELL_SIZE,
          CELL_SIZE
        );
      }
    }

    ctx.stroke();
  };

  canvas.addEventListener("click", async event => {
    if (!isPlaying) {
      const boundingRect = canvas.getBoundingClientRect();

      const scaleX = canvas.width / boundingRect.width;
      const scaleY = canvas.height / boundingRect.height;

      const canvasLeft = (event.clientX - boundingRect.left) * scaleX;
      const canvasTop = (event.clientY - boundingRect.top) * scaleY;

      const row = Math.min(Math.floor(canvasTop / (CELL_SIZE + 1)), height - 1);
      const col = Math.min(Math.floor(canvasLeft / (CELL_SIZE + 1)), width - 1);

      universe = await asterius.exports.toggleCell(row, col, universe);

      drawGrid();
      drawCells();
    }
  });

  const renderLoop = async () => {
    universe = await asterius.exports.tickUniverse(universe);
    drawGrid();
    drawCells();
    if (isPlaying) {
      fps.render();
      requestAnimationFrame(renderLoop);
    }
  };

  let isPlaying = true;

  const play = () => {
    playPauseButton.textContent = '⏸';
    isPlaying = true;
    drawGrid();
    drawCells();
    requestAnimationFrame(renderLoop);
  };

  const pause = () => {
    playPauseButton.textContent = '▶️';
    isPlaying = false;
  };

  const playPauseButton = document.getElementById("play-pause");
  playPauseButton.addEventListener("click", event => {
    if (isPlaying) {
      pause();
    } else {
      play();
    }
  });

  play();
}

module.then(handleModule);
