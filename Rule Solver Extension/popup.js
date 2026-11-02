"use strict";

function gcd(left, right) {
  let a = left < 0n ? -left : left;
  let b = right < 0n ? -right : right;
  while (b !== 0n) {
    [a, b] = [b, a % b];
  }
  return a || 1n;
}

function rational(numerator, denominator = 1n) {
  if (denominator === 0n) {
    throw new Error("A fraction cannot have a denominator of zero.");
  }
  const sign = denominator < 0n ? -1n : 1n;
  const divisor = gcd(numerator, denominator);
  return {
    n: (numerator / divisor) * sign,
    d: (denominator / divisor) * sign,
  };
}

function parseValue(text) {
  const value = text.trim();
  const fraction = value.match(/^([+-]?\d+)\s*\/\s*([+-]?\d+)$/);
  if (fraction) {
    return rational(BigInt(fraction[1]), BigInt(fraction[2]));
  }

  const decimal = value.match(/^([+-]?)(\d*)(?:\.(\d*))?(?:e([+-]?\d+))?$/i);
  if (!decimal || (!decimal[2] && !decimal[3])) {
    throw new Error(`"${value}" is not a valid number or fraction.`);
  }

  const sign = decimal[1] === "-" ? -1n : 1n;
  const whole = decimal[2] || "0";
  const fractional = decimal[3] || "";
  const exponent = Number(decimal[4] || 0);
  if (!Number.isSafeInteger(exponent) || Math.abs(exponent) > 1000) {
    throw new Error(`"${value}" has an unsupported exponent.`);
  }
  let numerator = BigInt(whole + fractional) * sign;
  let scale = fractional.length - exponent;
  if (scale < 0) {
    numerator *= 10n ** BigInt(-scale);
    scale = 0;
  }
  return rational(numerator, 10n ** BigInt(scale));
}

function add(a, b) {
  return rational(a.n * b.d + b.n * a.d, a.d * b.d);
}

function subtract(a, b) {
  return rational(a.n * b.d - b.n * a.d, a.d * b.d);
}

function multiply(a, b) {
  return rational(a.n * b.n, a.d * b.d);
}

function divide(a, b) {
  if (b.n === 0n) {
    throw new Error("Cannot divide by zero.");
  }
  return rational(a.n * b.d, a.d * b.n);
}

function isZero(value) {
  return value.n === 0n;
}

function parseList(text, label) {
  const parts = text.split(",").map((part) => part.trim());
  if (parts.some((part) => part === "")) {
    throw new Error(`${label} must contain values separated by commas, without empty entries.`);
  }
  return parts.map(parseValue);
}

function addLinear(left, right) {
  return { a: add(left.a, right.a), b: add(left.b, right.b) };
}

function subtractLinear(left, right) {
  return { a: subtract(left.a, right.a), b: subtract(left.b, right.b) };
}

function multiplyLinear(left, right) {
  if (!isZero(left.a) && !isZero(right.a)) {
    throw new Error("This solver handles linear equations only; multiplying variables makes a nonlinear equation.");
  }
  if (isZero(left.a)) {
    return { a: multiply(left.b, right.a), b: multiply(left.b, right.b) };
  }
  return { a: multiply(right.b, left.a), b: multiply(right.b, left.b) };
}

function divideLinear(left, right) {
  if (!isZero(right.a)) {
    throw new Error("Cannot divide by an expression containing the variable.");
  }
  return { a: divide(left.a, right.b), b: divide(left.b, right.b) };
}

function parseLinearExpression(source) {
  let position = 0;
  let variable = null;
  const zero = rational(0n);
  const one = rational(1n);

  const skipSpaces = () => {
    while (/\s/.test(source[position] || "")) {
      position += 1;
    }
  };
  const consume = (character) => {
    skipSpaces();
    if (source[position] !== character) {
      return false;
    }
    position += 1;
    return true;
  };

  function parsePrimary() {
    skipSpaces();
    if (consume("(")) {
      const value = parseExpression();
      if (!consume(")")) {
        throw new Error("Missing closing parenthesis in equation.");
      }
      return value;
    }

    const number = source.slice(position).match(/^(?:\d+(?:\.\d*)?|\.\d+)/);
    if (number) {
      position += number[0].length;
      return { a: zero, b: parseValue(number[0]) };
    }

    const symbol = source.slice(position).match(/^[a-z]/i);
    if (symbol) {
      const found = symbol[0].toLowerCase();
      if (variable !== null && variable !== found) {
        throw new Error("Use only one variable in a linear equation.");
      }
      variable = found;
      position += symbol[0].length;
      return { a: one, b: zero };
    }

    throw new Error(`Expected a number, variable, or parenthesis near "${source.slice(position)}".`);
  }

  function parseUnary() {
    if (consume("+")) {
      return parseUnary();
    }
    if (consume("-")) {
      const value = parseUnary();
      return { a: rational(-value.a.n, value.a.d), b: rational(-value.b.n, value.b.d) };
    }
    return parsePrimary();
  }

  function parseTerm() {
    let value = parseUnary();
    while (true) {
      if (consume("*")) {
        value = multiplyLinear(value, parseUnary());
      } else if (consume("/")) {
        value = divideLinear(value, parseUnary());
      } else {
        skipSpaces();
        if (/[0-9.(a-z]/i.test(source[position] || "")) {
          value = multiplyLinear(value, parseUnary());
        } else {
          return value;
        }
      }
    }
  }

  function parseExpression() {
    let value = parseTerm();
    while (true) {
      if (consume("+")) {
        value = addLinear(value, parseTerm());
      } else if (consume("-")) {
        value = subtractLinear(value, parseTerm());
      } else {
        return value;
      }
    }
  }

  if (!source.trim()) {
    throw new Error("Enter an equation.");
  }
  const result = parseExpression();
  skipSpaces();
  if (position !== source.length) {
    throw new Error(`Unexpected character "${source[position]}" in equation.`);
  }
  return { ...result, variable };
}

function solveLinearEquation(source) {
  const sides = source.split("=");
  if (sides.length !== 2 || !sides[0].trim() || !sides[1].trim()) {
    throw new Error("Enter one equation with an equals sign, such as 23a/35 = 200.");
  }

  const left = parseLinearExpression(sides[0]);
  const right = parseLinearExpression(sides[1]);
  if (left.variable && right.variable && left.variable !== right.variable) {
    throw new Error("Use the same variable on both sides of the equation.");
  }

  const variable = left.variable || right.variable || "x";
  const coefficient = subtract(left.a, right.a);
  const constant = subtract(right.b, left.b);
  if (isZero(coefficient)) {
    return {
      variable,
      answer: isZero(constant) ? "Every value is a solution." : "There is no solution.",
    };
  }
  return { variable, answer: `${variable} = ${formatDisplayFraction(divide(constant, coefficient))}` };
}

function calculateExpression(source) {
  let position = 0;
  const skipSpaces = () => {
    while (/\s/.test(source[position] || "")) {
      position += 1;
    }
  };
  const consume = (character) => {
    skipSpaces();
    if (source[position] !== character) {
      return false;
    }
    position += 1;
    return true;
  };

  function parsePrimary() {
    skipSpaces();
    if (consume("(")) {
      const value = parseExpression();
      if (!consume(")")) {
        throw new Error("Missing closing parenthesis.");
      }
      return value;
    }
    const match = source.slice(position).match(/^(?:\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?/i);
    if (!match) {
      throw new Error(`Expected a number near "${source.slice(position)}".`);
    }
    position += match[0].length;
    const value = Number(match[0]);
    if (!Number.isFinite(value)) {
      throw new Error("The number is outside the calculator's supported range.");
    }
    return value;
  }

  function parsePower() {
    const base = parsePrimary();
    if (consume("^")) {
      const value = base ** parseUnary();
      if (!Number.isFinite(value)) {
        throw new Error("That power does not have a finite real result.");
      }
      return value;
    }
    return base;
  }

  function parseUnary() {
    if (consume("+")) {
      return parseUnary();
    }
    if (consume("-")) {
      return -parseUnary();
    }
    return parsePower();
  }

  function parseTerm() {
    let value = parseUnary();
    while (true) {
      if (consume("*")) {
        value *= parseUnary();
      } else if (consume("/")) {
        const divisor = parseUnary();
        if (divisor === 0) {
          throw new Error("Cannot divide by zero.");
        }
        value /= divisor;
      } else {
        return value;
      }
      if (!Number.isFinite(value)) {
        throw new Error("The result is outside the calculator's supported range.");
      }
    }
  }

  function parseExpression() {
    let value = parseTerm();
    while (true) {
      if (consume("+")) {
        value += parseTerm();
      } else if (consume("-")) {
        value -= parseTerm();
      } else {
        return value;
      }
      if (!Number.isFinite(value)) {
        throw new Error("The result is outside the calculator's supported range.");
      }
    }
  }

  if (!source.trim()) {
    throw new Error("Enter a math expression.");
  }
  const result = parseExpression();
  skipSpaces();
  if (position !== source.length) {
    throw new Error(`Unexpected character "${source[position]}".`);
  }
  return result;
}

function findPolynomialRule(inputs, outputs) {
  if (inputs.length !== outputs.length) {
    throw new Error("Enter the same number of input and output values.");
  }
  if (inputs.length === 0) {
    throw new Error("Enter at least one input/output pair.");
  }

  const points = new Map();
  for (let index = 0; index < inputs.length; index += 1) {
    const key = formatFraction(inputs[index]);
    if (points.has(key) && !isZero(subtract(points.get(key), outputs[index]))) {
      throw new Error(`The input ${key} has different outputs, so these pairs do not describe a function.`);
    }
    points.set(key, outputs[index]);
  }

  const pairs = [...points.entries()].map(([key, output]) => [parseValue(key), output]);
  for (let degree = 0; degree < pairs.length; degree += 1) {
    const matrix = pairs.map(([x, y]) => {
      const row = [];
      let power = rational(1n);
      for (let column = 0; column <= degree; column += 1) {
        row.push(power);
        power = multiply(power, x);
      }
      row.push(y);
      return row;
    });

    let pivotRow = 0;
    const pivotColumns = [];
    for (let column = 0; column <= degree; column += 1) {
      let pivot = pivotRow;
      while (pivot < matrix.length && isZero(matrix[pivot][column])) {
        pivot += 1;
      }
      if (pivot === matrix.length) {
        continue;
      }

      [matrix[pivotRow], matrix[pivot]] = [matrix[pivot], matrix[pivotRow]];
      const pivotValue = matrix[pivotRow][column];
      matrix[pivotRow] = matrix[pivotRow].map((value) => divide(value, pivotValue));

      for (let row = 0; row < matrix.length; row += 1) {
        if (row === pivotRow || isZero(matrix[row][column])) {
          continue;
        }
        const factor = matrix[row][column];
        matrix[row] = matrix[row].map((value, index) =>
          subtract(value, multiply(factor, matrix[pivotRow][index])),
        );
      }

      pivotColumns.push(column);
      pivotRow += 1;
    }

    const inconsistent = matrix.some(
      (row) => row.slice(0, -1).every(isZero) && !isZero(row[row.length - 1]),
    );
    if (inconsistent) {
      continue;
    }

    const coefficients = Array.from({ length: degree + 1 }, () => rational(0n));
    pivotColumns.forEach((column, row) => {
      coefficients[column] = matrix[row][matrix[row].length - 1];
    });
    return coefficients;
  }

  throw new Error("Could not find a polynomial rule for these pairs.");
}

function formatFraction(value) {
  return value.d === 1n ? value.n.toString() : `${value.n}/${value.d}`;
}

function formatDisplayFraction(value) {
  const groupedNumerator = value.n.toLocaleString("en-US");
  if (value.d === 1n) {
    return groupedNumerator;
  }
  return `${groupedNumerator}/${value.d.toLocaleString("en-US")}`;
}

function formatRule(coefficients) {
  const terms = [];
  for (let power = coefficients.length - 1; power >= 0; power -= 1) {
    const coefficient = coefficients[power];
    if (isZero(coefficient)) {
      continue;
    }

    const negative = coefficient.n < 0n;
    const magnitude = rational(negative ? -coefficient.n : coefficient.n, coefficient.d);
    let term;
    if (power === 0) {
      term = formatDisplayFraction(magnitude);
    } else {
      const variable = power === 1 ? "x" : `x^${power}`;
      term = magnitude.n === magnitude.d ? variable : `${formatDisplayFraction(magnitude)}${variable}`;
    }

    if (terms.length === 0) {
      terms.push(negative ? `-${term}` : term);
    } else {
      terms.push(`${negative ? "-" : "+"} ${term}`);
    }
  }
  return terms.join(" ") || "0";
}

function showResult(inputs, outputs, coefficients) {
  const tableBody = document.getElementById("pairs");
  tableBody.replaceChildren();
  inputs.forEach((input, index) => {
    const row = document.createElement("tr");
    const inputCell = document.createElement("td");
    const outputCell = document.createElement("td");
    inputCell.textContent = formatDisplayFraction(input);
    outputCell.textContent = formatDisplayFraction(outputs[index]);
    row.append(inputCell, outputCell);
    tableBody.append(row);
  });
  document.getElementById("rule").textContent = `y = ${formatRule(coefficients)}`;
  document.getElementById("rule-error").hidden = true;
  document.getElementById("rule-result").hidden = false;
}

function showToolError(tool, error) {
  document.getElementById(`${tool}-result`).hidden = true;
  const errorElement = document.getElementById(`${tool}-error`);
  errorElement.textContent = error.message;
  errorElement.hidden = false;
}

function showToolResult(tool, message) {
  const resultElement = document.getElementById(`${tool}-result`);
  resultElement.textContent = message;
  resultElement.hidden = false;
  document.getElementById(`${tool}-error`).hidden = true;
}

function formatNumber(value) {
  if (!Number.isFinite(value)) {
    throw new Error("The result is outside the calculator's supported range.");
  }
  return new Intl.NumberFormat("en-US", { maximumSignificantDigits: 12 })
    .format(Number(value.toPrecision(12)));
}

function readDimension(id, label) {
  const value = document.getElementById(id).valueAsNumber;
  if (!Number.isFinite(value) || value < 0) {
    throw new Error(`${label} must be a nonnegative number.`);
  }
  return value;
}

function calculateGeometry(shape) {
  if (shape === "rectangle") {
    const length = readDimension("rectangle-length", "Length");
    const width = readDimension("rectangle-width", "Width");
    return `Area: ${formatNumber(length * width)}\nPerimeter: ${formatNumber(2 * (length + width))}`;
  }
  if (shape === "square") {
    const side = readDimension("square-side", "Side length");
    return `Area: ${formatNumber(side ** 2)}\nPerimeter: ${formatNumber(4 * side)}`;
  }
  if (shape === "circle") {
    const radius = readDimension("circle-radius", "Radius");
    return `Area: ${formatNumber(Math.PI * radius ** 2)} (${formatNumber(radius ** 2)}π)\nCircumference: ${formatNumber(2 * Math.PI * radius)} (${formatNumber(2 * radius)}π)`;
  }
  if (shape === "triangle") {
    const base = readDimension("triangle-base", "Base");
    const height = readDimension("triangle-height", "Height");
    const sideA = readDimension("triangle-side-a", "Side 1");
    const sideB = readDimension("triangle-side-b", "Side 2");
    if (base === 0 || sideA === 0 || sideB === 0 ||
        base + sideA <= sideB || base + sideB <= sideA || sideA + sideB <= base) {
      throw new Error("The three side lengths must form a valid triangle.");
    }
    return `Area: ${formatNumber(base * height / 2)}\nPerimeter: ${formatNumber(base + sideA + sideB)}`;
  }
  if (shape === "cube") {
    const side = readDimension("cube-side", "Side length");
    return `Surface area: ${formatNumber(6 * side ** 2)}\nVolume: ${formatNumber(side ** 3)}`;
  }

  const radius = readDimension("sphere-radius", "Radius");
  return `Surface area: ${formatNumber(4 * Math.PI * radius ** 2)} (${formatNumber(4 * radius ** 2)}π)\nVolume: ${formatNumber(4 * Math.PI * radius ** 3 / 3)} (${formatNumber(4 * radius ** 3 / 3)}π)`;
}

function simplifySquareRoot(value) {
  let remaining = BigInt(value);
  if (remaining === 0n) {
    return "0";
  }
  let outside = 1n;
  let inside = 1n;
  for (let factor = 2n; factor * factor <= remaining; factor += factor === 2n ? 1n : 2n) {
    let count = 0;
    while (remaining % factor === 0n) {
      remaining /= factor;
      count += 1;
    }
    if (count > 0) {
      outside *= factor ** BigInt(Math.floor(count / 2));
      if (count % 2 === 1) {
        inside *= factor;
      }
    }
  }

  if (remaining > 1n) {
    inside *= remaining;
  }
  if (outside === 1n) {
    return inside === 1n ? "1" : `√${inside.toLocaleString("en-US")}`;
  }
  const groupedOutside = outside.toLocaleString("en-US");
  return inside === 1n ? groupedOutside : `${groupedOutside}√${inside.toLocaleString("en-US")}`;
}

const GAME_SIZE = 16;
const GAME_HEIGHT = 8;
const GAME_EYE_HEIGHT = 1.65;
const GAME_FOV = Math.PI / 2.8;
const GAME_TEXTURE_SIZE = 16;
const GAME_GRAVITY = 18;
const GAME_JUMP_SPEED = 7;
const GAME_TEXTURE_PALETTES = {
  grassTop: [[82, 157, 59], [96, 177, 68], [113, 188, 78], [72, 143, 52]],
  grassSide: [[119, 91, 57], [133, 101, 62], [105, 78, 50], [91, 166, 60], [106, 181, 67]],
  dirt: [[119, 82, 50], [135, 96, 58], [149, 108, 66], [105, 72, 46]],
  stone: [[112, 117, 124], [128, 132, 138], [144, 148, 153], [97, 103, 111]],
};

function makeBlockTexture(palette, seed, grassCap = false) {
  const pixels = new Array(GAME_TEXTURE_SIZE * GAME_TEXTURE_SIZE);
  for (let y = 0; y < GAME_TEXTURE_SIZE; y += 1) {
    for (let x = 0; x < GAME_TEXTURE_SIZE; x += 1) {
      let hash = Math.imul(x + 1, 374761393) + Math.imul(y + seed + 1, 668265263);
      hash = Math.imul(hash ^ (hash >>> 13), 1274126177);
      hash = (hash ^ (hash >>> 16)) >>> 0;
      const paletteIndex = grassCap && y < 3
        ? 3 + hash % 2
        : grassCap
          ? hash % 3
          : hash % palette.length;
      pixels[y * GAME_TEXTURE_SIZE + x] = palette[paletteIndex];
    }
  }
  return pixels;
}

const GAME_BLOCK_TEXTURES = {
  grassTop: makeBlockTexture(GAME_TEXTURE_PALETTES.grassTop, 7),
  grassSide: makeBlockTexture(GAME_TEXTURE_PALETTES.grassSide, 17, true),
  dirt: makeBlockTexture(GAME_TEXTURE_PALETTES.dirt, 29),
  stone: makeBlockTexture(GAME_TEXTURE_PALETTES.stone, 43),
};

let gameWorld = new Map();
let gameCanvas = null;
let gameContext = null;
let gameImage = null;
let gameRunning = false;
let gameFrame = 0;
let gameLastFrame = 0;
let gameLastUpdate = 0;
let gameFpsFrames = 0;
let gameFpsStart = 0;
let gameDirty = true;
let gameRayDirections = null;
let gameRayCamera = "";
let gameYaw = 0;
let gamePitch = 0;
let gamePlayer = { x: 8.5, z: 8.5, y: 0 };
let gameVerticalVelocity = 0;
const gameMaterials = ["grass", "dirt", "stone"];
const gameInventory = { grass: 10, dirt: 10, stone: 10 };
const gameHotbar = ["grass", "dirt", "stone", null, null, null, null, null, null];
let gameSelectedSlot = 0;
let gameInventoryOpen = false;
const gameKeys = new Set();
const gameHeldButtons = new Map();

function markGameDirty() {
  gameDirty = true;
}

function updateGameInventory() {
  const hotbar = document.getElementById("game-hotbar");
  const inventoryHotbar = document.getElementById("inventory-hotbar");
  const inventory = document.getElementById("inventory-grid");
  hotbar.replaceChildren();
  inventoryHotbar.replaceChildren();
  inventory.replaceChildren();

  gameHotbar.forEach((material, index) => {
    hotbar.append(createInventorySlot(material, index, "hotbar-slot"));
    inventoryHotbar.append(createInventorySlot(material, index, "hotbar-slot"));
  });
  for (let index = 0; index < 27; index += 1) {
    inventory.append(createInventorySlot(gameMaterials[index] || null, index, "inventory-slot"));
  }
}

function createInventorySlot(material, index, slotClass) {
  const slot = document.createElement("button");
  slot.type = "button";
  slot.className = `inventory-slot ${slotClass}`;
  slot.dataset.slot = String(index);
  slot.setAttribute("aria-pressed", String(slotClass === "hotbar-slot" && index === gameSelectedSlot));
  if (slotClass === "hotbar-slot" && index === gameSelectedSlot) {
    slot.classList.add("selected");
  }

  if (slotClass === "hotbar-slot") {
    const shortcut = document.createElement("span");
    shortcut.className = "inventory-name";
    shortcut.textContent = String(index + 1);
    slot.append(shortcut);
  }

  if (material) {
    slot.dataset.material = material;
    slot.setAttribute("aria-label", `${material}, ${gameInventory[material]} blocks, slot ${index + 1}`);
    const swatch = document.createElement("span");
    swatch.className = `inventory-swatch ${material}-swatch`;
    swatch.setAttribute("aria-hidden", "true");
    const count = document.createElement("span");
    count.className = "inventory-count";
    count.textContent = gameInventory[material] > 1 ? gameInventory[material].toLocaleString("en-US") : "";
    slot.append(swatch, count);
  } else {
    slot.classList.add("empty");
    slot.setAttribute("aria-label", `Empty slot ${index + 1}`);
    slot.disabled = true;
  }

  if (slotClass === "inventory-slot" && material) {
    slot.addEventListener("click", () => selectGameSlot(index));
  } else if (slotClass === "hotbar-slot" && material) {
    slot.addEventListener("click", () => selectGameSlot(index));
  }
  return slot;
}

function selectGameSlot(index) {
  if (!Number.isInteger(index) || index < 0 || index >= gameHotbar.length || !gameHotbar[index]) {
    return;
  }
  gameSelectedSlot = index;
  updateGameInventory();
}

function toggleGameInventory(open = !gameInventoryOpen) {
  if (open && !gameInventoryOpen) {
    gameInventoryRestorePointerLock = document.pointerLockElement === gameCanvas;
  }
  gameInventoryOpen = open;
  document.getElementById("game-inventory-panel").hidden = !open;
  gameKeys.clear();
  if (open && document.pointerLockElement === gameCanvas) {
    document.exitPointerLock();
  }
  setGameMessage(open ? "Inventory open. Press E or Escape to close." : "Inventory closed.");
  markGameDirty();
  if (!open && gameInventoryRestorePointerLock) {
    gameInventoryRestorePointerLock = false;
    requestGamePointerLock();
  }
}

function gameKey(x, y, z) {
  return `${x},${y},${z}`;
}

function gameTerrainHeight(x, z) {
  const wave = Math.sin(x * 0.7) + Math.cos(z * 0.61) + Math.sin((x + z) * 0.43);
  return Math.max(1, Math.min(4, 2 + Math.floor((wave + 1.5) / 1.5)));
}

function resetGameWorld() {
  gameWorld = new Map();
  for (let x = 0; x < GAME_SIZE; x += 1) {
    for (let z = 0; z < GAME_SIZE; z += 1) {
      const height = gameTerrainHeight(x, z);
      for (let y = 0; y < height; y += 1) {
        const material = y === height - 1 ? "grass" : y === 0 ? "stone" : "dirt";
        gameWorld.set(gameKey(x, y, z), material);
      }
    }
  }
  const spawnHeight = gameColumnTop(8, 8) + 1;
  gamePlayer = { x: 8.5, z: 8.5, y: spawnHeight + GAME_EYE_HEIGHT };
  gameVerticalVelocity = 0;
  gameInventory.grass = 10;
  gameInventory.dirt = 10;
  gameInventory.stone = 10;
  gameHotbar.splice(0, gameHotbar.length, "grass", "dirt", "stone", null, null, null, null, null, null);
  gameSelectedSlot = 0;
  gameInventoryOpen = false;
  document.getElementById("game-inventory-panel").hidden = true;
  updateGameInventory();
  gameYaw = 0;
  gamePitch = -0.65;
  gameRayCamera = "";
  markGameDirty();
  updateGameStatus();
}

function gameColumnTop(x, z) {
  for (let y = GAME_HEIGHT - 1; y >= 0; y -= 1) {
    if (gameWorld.has(gameKey(x, y, z))) {
      return y;
    }
  }
  return -1;
}

function gameRaycast(directionX, directionY, directionZ, maxDistance = 32) {
  let cellX = Math.floor(gamePlayer.x);
  let cellY = Math.floor(gamePlayer.y);
  let cellZ = Math.floor(gamePlayer.z);
  const stepX = Math.sign(directionX);
  const stepY = Math.sign(directionY);
  const stepZ = Math.sign(directionZ);
  const deltaX = stepX === 0 ? Infinity : Math.abs(1 / directionX);
  const deltaY = stepY === 0 ? Infinity : Math.abs(1 / directionY);
  const deltaZ = stepZ === 0 ? Infinity : Math.abs(1 / directionZ);
  let sideX = stepX === 0 ? Infinity : stepX > 0 ? (cellX + 1 - gamePlayer.x) * deltaX : (gamePlayer.x - cellX) * deltaX;
  let sideY = stepY === 0 ? Infinity : stepY > 0 ? (cellY + 1 - gamePlayer.y) * deltaY : (gamePlayer.y - cellY) * deltaY;
  let sideZ = stepZ === 0 ? Infinity : stepZ > 0 ? (cellZ + 1 - gamePlayer.z) * deltaZ : (gamePlayer.z - cellZ) * deltaZ;
  let distance = 0;
  let normal = { x: 0, y: 0, z: 0 };

  for (let step = 0; step < 80 && distance <= maxDistance; step += 1) {
    if (cellX < 0 || cellX >= GAME_SIZE || cellZ < 0 || cellZ >= GAME_SIZE || cellY < 0 || cellY >= GAME_HEIGHT) {
      return null;
    }
    const material = gameWorld.get(gameKey(cellX, cellY, cellZ));
    if (material) {
      return {
        x: cellX,
        y: cellY,
        z: cellZ,
        material,
        distance,
        normal,
        point: {
          x: gamePlayer.x + directionX * distance,
          y: gamePlayer.y + directionY * distance,
          z: gamePlayer.z + directionZ * distance,
        },
      };
    }

    if (sideX < sideY && sideX < sideZ) {
      distance = sideX;
      sideX += deltaX;
      cellX += stepX;
      normal = { x: -stepX, y: 0, z: 0 };
    } else if (sideY < sideZ) {
      distance = sideY;
      sideY += deltaY;
      cellY += stepY;
      normal = { x: 0, y: -stepY, z: 0 };
    } else {
      distance = sideZ;
      sideZ += deltaZ;
      cellZ += stepZ;
      normal = { x: 0, y: 0, z: -stepZ };
    }
  }
  return null;
}

function sampleGameBlockTexture(hit) {
  const horizontal = hit.normal.x !== 0 ? hit.point.z : hit.point.x;
  const vertical = hit.normal.y !== 0 ? hit.point.z : hit.point.y;
  const localU = horizontal - Math.floor(horizontal);
  const localV = vertical - Math.floor(vertical);
  let textureX = Math.min(15, Math.floor(localU * GAME_TEXTURE_SIZE));
  let textureY = Math.min(15, Math.floor(localV * GAME_TEXTURE_SIZE));

  const blockSeed = (
    Math.imul(hit.x + 1, 73856093) ^
    Math.imul(hit.y + 1, 19349663) ^
    Math.imul(hit.z + 1, 83492791)
  ) >>> 0;
  if (blockSeed & 1) {
    textureX = 15 - textureX;
  }
  if (blockSeed & 2) {
    textureY = 15 - textureY;
  }
  const rotation = (blockSeed >>> 2) & 3;
  if (rotation === 1) {
    [textureX, textureY] = [textureY, 15 - textureX];
  } else if (rotation === 2) {
    textureX = 15 - textureX;
    textureY = 15 - textureY;
  } else if (rotation === 3) {
    [textureX, textureY] = [15 - textureY, textureX];
  }

  let texture;
  if (hit.material === "grass") {
    texture = hit.normal.y > 0
      ? GAME_BLOCK_TEXTURES.grassTop
      : hit.normal.y < 0
        ? GAME_BLOCK_TEXTURES.dirt
        : GAME_BLOCK_TEXTURES.grassSide;
  } else {
    texture = GAME_BLOCK_TEXTURES[hit.material] || GAME_BLOCK_TEXTURES.stone;
  }

  return {
    color: texture[textureY * GAME_TEXTURE_SIZE + textureX],
    edge: localU < 1 / GAME_TEXTURE_SIZE || localU > 1 - 1 / GAME_TEXTURE_SIZE ||
      localV < 1 / GAME_TEXTURE_SIZE || localV > 1 - 1 / GAME_TEXTURE_SIZE,
  };
}

function gameViewDirection(screenX, screenY) {
  const aspect = gameCanvas.width / gameCanvas.height;
  const horizontal = (screenX * 2 - 1) * aspect * Math.tan(GAME_FOV / 2);
  const vertical = (1 - screenY * 2) * Math.tan(GAME_FOV / 2);
  const forward = {
    x: Math.sin(gameYaw) * Math.cos(gamePitch),
    y: Math.sin(gamePitch),
    z: Math.cos(gameYaw) * Math.cos(gamePitch),
  };
  const right = { x: Math.cos(gameYaw), y: 0, z: -Math.sin(gameYaw) };
  const up = {
    x: -Math.sin(gameYaw) * Math.sin(gamePitch),
    y: Math.cos(gamePitch),
    z: -Math.cos(gameYaw) * Math.sin(gamePitch),
  };
  const x = forward.x + right.x * horizontal + up.x * vertical;
  const y = forward.y + right.y * horizontal + up.y * vertical;
  const z = forward.z + right.z * horizontal + up.z * vertical;
  const length = Math.hypot(x, y, z);
  return { x: x / length, y: y / length, z: z / length };
}

function updateGameRayDirections() {
  const camera = `${gameYaw}:${gamePitch}:${gameCanvas.width}:${gameCanvas.height}`;
  if (camera === gameRayCamera && gameRayDirections) {
    return;
  }
  const width = gameCanvas.width;
  const height = gameCanvas.height;
  const aspect = width / height;
  const yawSin = Math.sin(gameYaw);
  const yawCos = Math.cos(gameYaw);
  const pitchSin = Math.sin(gamePitch);
  const pitchCos = Math.cos(gamePitch);
  const horizontalScale = aspect * Math.tan(GAME_FOV / 2);
  const verticalScale = Math.tan(GAME_FOV / 2);
  gameRayDirections = new Float32Array(width * height * 3);

  for (let py = 0; py < height; py += 1) {
    const vertical = (1 - (py + 0.5) / height * 2) * verticalScale;
    for (let px = 0; px < width; px += 1) {
      const horizontal = ((px + 0.5) / width * 2 - 1) * horizontalScale;
      const forwardX = yawSin * pitchCos;
      const forwardY = pitchSin;
      const forwardZ = yawCos * pitchCos;
      const rightX = yawCos;
      const rightZ = -yawSin;
      const upX = -yawSin * pitchSin;
      const upY = pitchCos;
      const upZ = -yawCos * pitchSin;
      const x = forwardX + rightX * horizontal + upX * vertical;
      const y = forwardY + upY * vertical;
      const z = forwardZ + rightZ * horizontal + upZ * vertical;
      const length = Math.hypot(x, y, z);
      const index = (py * width + px) * 3;
      gameRayDirections[index] = x / length;
      gameRayDirections[index + 1] = y / length;
      gameRayDirections[index + 2] = z / length;
    }
  }
  gameRayCamera = camera;
}

function renderGame() {
  const width = gameCanvas.width;
  const height = gameCanvas.height;
  const pixels = gameImage.data;
  const skyTop = [103, 176, 230];
  const skyHorizon = [191, 224, 245];
  updateGameRayDirections();

  for (let py = 0; py < height; py += 1) {
    for (let px = 0; px < width; px += 1) {
      const index = (py * width + px) * 4;
      const horizonBlend = Math.min(1, py / height * 1.5);
      let red = skyTop[0] + (skyHorizon[0] - skyTop[0]) * horizonBlend;
      let green = skyTop[1] + (skyHorizon[1] - skyTop[1]) * horizonBlend;
      let blue = skyTop[2] + (skyHorizon[2] - skyTop[2]) * horizonBlend;
      const rayIndex = (py * width + px) * 3;
      const hit = gameRaycast(
        gameRayDirections[rayIndex],
        gameRayDirections[rayIndex + 1],
        gameRayDirections[rayIndex + 2],
      );

      if (hit) {
        const sampled = sampleGameBlockTexture(hit);
        const sideShade = hit.normal.y > 0 ? 1 : hit.normal.y < 0 ? 0.55 : 0.78;
        const edgeShade = sampled.edge ? 0.72 : 1;
        const shade = Math.max(0.35, 1 - hit.distance * 0.018) * sideShade * edgeShade;
        const fog = Math.max(0, Math.min(0.55, (hit.distance - 12) / 32));
        red = sampled.color[0] * shade * (1 - fog) + skyHorizon[0] * fog;
        green = sampled.color[1] * shade * (1 - fog) + skyHorizon[1] * fog;
        blue = sampled.color[2] * shade * (1 - fog) + skyHorizon[2] * fog;
      }

      pixels[index] = red;
      pixels[index + 1] = green;
      pixels[index + 2] = blue;
      pixels[index + 3] = 255;
    }
  }
  gameContext.putImageData(gameImage, 0, 0);
  gameDirty = false;
}

function updateGameStatus() {
  const direction = gameViewDirection(0.5, 0.5);
  const hit = gameRaycast(direction.x, direction.y, direction.z, 6);
  document.getElementById("game-block").textContent = hit
    ? `Looking at: ${hit.material[0].toUpperCase()}${hit.material.slice(1)}`
    : "Looking at: —";
  document.getElementById("game-position").textContent =
    `Position: ${Math.floor(gamePlayer.x)}, ${Math.floor(gamePlayer.z)}`;
  document.getElementById("game-height").textContent =
    `Height: ${formatNumber(Math.max(0, gamePlayer.y - GAME_EYE_HEIGHT))}`;
}

function setGameMessage(message) {
  document.getElementById("game-message").textContent = message;
}

function mineGameBlock() {
  const direction = gameViewDirection(0.5, 0.5);
  const hit = gameRaycast(direction.x, direction.y, direction.z, 6);
  if (!hit) {
    setGameMessage("No block in reach.");
    return;
  }
  if (hit.y === 0) {
    setGameMessage("The bottom stone layer cannot be broken.");
    return;
  }
  gameWorld.delete(gameKey(hit.x, hit.y, hit.z));
  gameInventory[hit.material] += 1;
  updateGameInventory();
  setGameMessage(`Broke ${hit.material} block.`);
  markGameDirty();
  renderGame();
  updateGameStatus();
}

function placeGameBlock() {
  const direction = gameViewDirection(0.5, 0.5);
  const hit = gameRaycast(direction.x, direction.y, direction.z, 6);
  if (!hit) {
    setGameMessage("Aim at a block within reach first.");
    return;
  }
  const x = hit.x + hit.normal.x;
  const y = hit.y + hit.normal.y;
  const z = hit.z + hit.normal.z;
  if (x < 0 || x >= GAME_SIZE || z < 0 || z >= GAME_SIZE || y < 0 || y >= GAME_HEIGHT) {
    setGameMessage("There is no room to place a block there.");
    return;
  }
  if (Math.floor(gamePlayer.x) === x && Math.floor(gamePlayer.z) === z &&
      y >= Math.floor(gamePlayer.y - GAME_EYE_HEIGHT) && y <= Math.floor(gamePlayer.y)) {
    setGameMessage("You cannot place a block inside yourself.");
    return;
  }
  const key = gameKey(x, y, z);
  if (!gameWorld.has(key)) {
    const material = gameHotbar[gameSelectedSlot];
    if (!material) {
      setGameMessage("Select a block in your hotbar first.");
      return;
    }
    if (gameInventory[material] <= 0) {
      setGameMessage(`No ${material} blocks left in your inventory.`);
      return;
    }
    gameWorld.set(key, material);
    gameInventory[material] -= 1;
    updateGameInventory();
    setGameMessage(`Placed ${material} block.`);
    markGameDirty();
    renderGame();
    updateGameStatus();
  } else {
    setGameMessage("That space is already occupied.");
  }
}

function jumpGamePlayer() {
  const floor = gameColumnTop(Math.floor(gamePlayer.x), Math.floor(gamePlayer.z)) + 1;
  const feet = gamePlayer.y - GAME_EYE_HEIGHT;
  if (Math.abs(feet - floor) > 0.06 || gameVerticalVelocity > 0) {
    return;
  }
  gameVerticalVelocity = GAME_JUMP_SPEED;
  setGameMessage("Jump!");
}

function updateGameVerticalPosition(deltaSeconds) {
  const columnX = Math.floor(gamePlayer.x);
  const columnZ = Math.floor(gamePlayer.z);
  const floor = gameColumnTop(columnX, columnZ) + 1;
  let feet = gamePlayer.y - GAME_EYE_HEIGHT;

  if (feet <= floor && gameVerticalVelocity <= 0) {
    feet = floor;
    gameVerticalVelocity = 0;
  } else {
    gameVerticalVelocity -= GAME_GRAVITY * deltaSeconds;
    feet += gameVerticalVelocity * deltaSeconds;
    if (feet <= floor) {
      feet = floor;
      gameVerticalVelocity = 0;
    }
  }
  gamePlayer.y = feet + GAME_EYE_HEIGHT;
  return Math.abs(gameVerticalVelocity) > 0 || feet > floor;
}

function moveGamePlayer(deltaSeconds) {
  if (gameInventoryOpen) {
    return false;
  }
  const forward = gameKeys.has("w") || gameKeys.has("arrowup");
  const backward = gameKeys.has("s") || gameKeys.has("arrowdown");
  const left = gameKeys.has("a") || gameKeys.has("arrowleft");
  const right = gameKeys.has("d") || gameKeys.has("arrowright");
  const forwardAmount = Number(forward) - Number(backward);
  const sideAmount = Number(right) - Number(left);
  if (forwardAmount === 0 && sideAmount === 0) {
    return false;
  }

  const length = Math.hypot(forwardAmount, sideAmount);
  const speed = 3.2 * deltaSeconds / length;
  const dx = (Math.sin(gameYaw) * forwardAmount + Math.cos(gameYaw) * sideAmount) * speed;
  const dz = (Math.cos(gameYaw) * forwardAmount - Math.sin(gameYaw) * sideAmount) * speed;
  let moved = false;
  const currentFloor = gameColumnTop(Math.floor(gamePlayer.x), Math.floor(gamePlayer.z)) + 1;
  const grounded = Math.abs(gamePlayer.y - GAME_EYE_HEIGHT - currentFloor) < 0.06;
  const canEnter = (x, z) => {
    const columnX = Math.floor(x);
    const columnZ = Math.floor(z);
    if (columnX < 0 || columnX >= GAME_SIZE || columnZ < 0 || columnZ >= GAME_SIZE) {
      return false;
    }
    const nextFloor = gameColumnTop(columnX, columnZ) + 1;
    return nextFloor <= currentFloor + 1;
  };

  const nextX = gamePlayer.x + dx;
  if (nextX !== gamePlayer.x && canEnter(nextX, gamePlayer.z)) {
    gamePlayer.x = nextX;
    moved = true;
  }
  const nextZ = gamePlayer.z + dz;
  if (nextZ !== gamePlayer.z && canEnter(gamePlayer.x, nextZ)) {
    gamePlayer.z = nextZ;
    moved = true;
  }

  if (grounded) {
    const newFloor = gameColumnTop(Math.floor(gamePlayer.x), Math.floor(gamePlayer.z)) + 1;
    if (newFloor > currentFloor) {
      gamePlayer.y = newFloor + GAME_EYE_HEIGHT;
    }
  }
  if (moved) {
    markGameDirty();
  }
  return moved;
}

function gameLoop(timestamp) {
  if (!gameRunning) {
    return;
  }
  const deltaSeconds = gameLastFrame ? Math.min((timestamp - gameLastFrame) / 1000, 0.05) : 0;
  gameLastFrame = timestamp;
  gameFpsFrames += 1;
  if (timestamp - gameFpsStart >= 500) {
    const fps = Math.round(gameFpsFrames * 1000 / (timestamp - gameFpsStart));
    document.getElementById("game-fps").textContent = `FPS: ${fps}`;
    gameFpsFrames = 0;
    gameFpsStart = timestamp;
  }
  const moved = moveGamePlayer(deltaSeconds);
  const inAir = updateGameVerticalPosition(deltaSeconds);
  if (inAir) {
    markGameDirty();
  }
  if (moved || inAir || timestamp - gameLastUpdate > 150) {
    updateGameStatus();
    gameLastUpdate = timestamp;
  }
  if (gameDirty) {
    renderGame();
  }
  gameFrame = requestAnimationFrame(gameLoop);
}

function startGame() {
  if (!gameCanvas) {
    gameCanvas = document.getElementById("game-canvas");
    gameContext = gameCanvas.getContext("2d", { alpha: false });
    gameImage = gameContext.createImageData(gameCanvas.width, gameCanvas.height);
    resetGameWorld();
  }
  if (!gameRunning) {
    gameRunning = true;
    gameLastFrame = 0;
    gameFpsFrames = 0;
    gameFpsStart = 0;
    markGameDirty();
    gameFrame = requestAnimationFrame(gameLoop);
  }
}

function stopGame() {
  gameRunning = false;
  if (gameFrame) {
    cancelAnimationFrame(gameFrame);
  }
  gameKeys.clear();
  gameInventoryRestorePointerLock = false;
  if (document.pointerLockElement === gameCanvas) {
    document.exitPointerLock();
  }
}

function handleGameKey(event, pressed) {
  if (document.getElementById("panel-game").hidden) {
    return;
  }
  if (event.target instanceof HTMLElement &&
      ["INPUT", "SELECT", "TEXTAREA"].includes(event.target.tagName)) {
    return;
  }
  const key = event.key.toLowerCase();
  if (key === "e") {
    event.preventDefault();
    if (pressed && !event.repeat) {
      toggleGameInventory();
    }
    return;
  }
  if (gameInventoryOpen) {
    if (key === "escape" && pressed) {
      event.preventDefault();
      toggleGameInventory(false);
    }
    return;
  }
  if (event.code === "Space") {
    event.preventDefault();
    if (pressed && !event.repeat) {
      jumpGamePlayer();
    }
    if (key === "escape" && document.pointerLockElement === gameCanvas) {
      document.exitPointerLock();
    }
    return;
  }
  if (key === "escape" && document.pointerLockElement === gameCanvas) {
    document.exitPointerLock();
    return;
  }
  if (["w", "a", "s", "d", "arrowup", "arrowdown", "arrowleft", "arrowright"].includes(key)) {
    event.preventDefault();
    if (pressed) {
      gameKeys.add(key);
    } else {
      gameKeys.delete(key);
    }
  } else if (pressed && /^[1-9]$/.test(key)) {
    event.preventDefault();
    selectGameSlot(Number(key) - 1);
  }
}

function applyAppearance(theme, accent) {
  document.body.dataset.theme = theme;
  document.body.style.setProperty("--accent", accent);

  const color = accent.slice(1);
  const red = Number.parseInt(color.slice(0, 2), 16) / 255;
  const green = Number.parseInt(color.slice(2, 4), 16) / 255;
  const blue = Number.parseInt(color.slice(4, 6), 16) / 255;
  const luminance = 0.2126 * red + 0.7152 * green + 0.0722 * blue;
  document.body.style.setProperty("--accent-ink", luminance > 0.45 ? "#172033" : "#ffffff");
}

function saveAppearance() {
  const theme = document.getElementById("theme-choice").value;
  const accent = document.getElementById("accent-choice").value;
  localStorage.setItem("math-toolbox-theme", theme);
  localStorage.setItem("math-toolbox-accent", accent);
  applyAppearance(theme, accent);
}

const storedTheme = localStorage.getItem("math-toolbox-theme");
const storedAccent = localStorage.getItem("math-toolbox-accent");
const savedTheme = storedTheme === "dark" ? "dark" : "light";
const savedAccent = /^#[\da-f]{6}$/i.test(storedAccent || "") ? storedAccent : "#315de8";
document.getElementById("theme-choice").value = savedTheme;
document.getElementById("accent-choice").value = savedAccent;
applyAppearance(savedTheme, savedAccent);
document.getElementById("theme-choice").addEventListener("change", saveAppearance);
document.getElementById("accent-choice").addEventListener("input", saveAppearance);
document.getElementById("reset-style").addEventListener("click", () => {
  document.getElementById("theme-choice").value = "light";
  document.getElementById("accent-choice").value = "#315de8";
  saveAppearance();
});

document.querySelectorAll("[data-tab]").forEach((tab) => {
  tab.addEventListener("click", () => {
    document.body.dataset.gameActive = String(tab.dataset.tab === "game");
    document.querySelectorAll("[data-tab]").forEach((otherTab) => {
      const selected = otherTab === tab;
      otherTab.classList.toggle("active", selected);
      otherTab.setAttribute("aria-selected", String(selected));
      otherTab.tabIndex = selected ? 0 : -1;
      document.getElementById(otherTab.getAttribute("aria-controls")).hidden = !selected;
    });
    if (tab.dataset.tab === "game") {
      startGame();
    } else {
      stopGame();
    }
  });
});

let gameDragPoint = null;
let gameInventoryRestorePointerLock = false;

function requestGamePointerLock() {
  if (!gameCanvas || gameInventoryOpen || document.pointerLockElement === gameCanvas ||
      typeof gameCanvas.requestPointerLock !== "function") {
    return;
  }
  try {
    const request = gameCanvas.requestPointerLock();
    if (request && typeof request.catch === "function") {
      request.catch((error) => {
        setGameMessage(`Could not capture mouse: ${error.message}`);
      });
    }
  } catch (error) {
    setGameMessage(`Could not capture mouse: ${error.message}`);
  }
}

document.getElementById("game-canvas").addEventListener("click", (event) => {
  if (event.target === gameCanvas && !gameInventoryOpen) {
    requestGamePointerLock();
  }
});

document.addEventListener("pointerlockchange", () => {
  const locked = document.pointerLockElement === gameCanvas;
  document.body.dataset.pointerLocked = String(locked);
  if (!locked) {
    gameDragPoint = null;
    if (gameCanvas && !gameInventoryOpen && !document.getElementById("panel-game").hidden) {
      setGameMessage("Mouse released. Click the scene to capture it again.");
    }
  } else {
    setGameMessage("Mouse captured. Press E for inventory or Escape to release.");
  }
});
document.addEventListener("pointerlockerror", () => {
  setGameMessage("Mouse capture was blocked. Drag the scene to look around.");
});
document.getElementById("game-canvas").addEventListener("pointerdown", (event) => {
  if (document.pointerLockElement === event.currentTarget) {
    if (!gameInventoryOpen && event.button === 0) {
      mineGameBlock();
    } else if (!gameInventoryOpen && event.button === 2) {
      placeGameBlock();
    }
    return;
  }
  if (gameInventoryOpen) {
    return;
  }
  gameDragPoint = { x: event.clientX, y: event.clientY };
  event.currentTarget.focus();
});
document.getElementById("game-canvas").addEventListener("contextmenu", (event) => {
  event.preventDefault();
});
document.getElementById("game-canvas").addEventListener("pointermove", (event) => {
  if (document.pointerLockElement === event.currentTarget) {
    gameYaw += event.movementX * 0.006;
    gamePitch = Math.max(-1.52, Math.min(1.52, gamePitch - event.movementY * 0.006));
    markGameDirty();
    updateGameStatus();
    return;
  }
  if (!gameDragPoint) {
    return;
  }
  const deltaX = event.clientX - gameDragPoint.x;
  const deltaY = event.clientY - gameDragPoint.y;
  gameYaw += deltaX * 0.012;
  gamePitch = Math.max(-1.52, Math.min(1.52, gamePitch - deltaY * 0.01));
  markGameDirty();
  gameDragPoint = { x: event.clientX, y: event.clientY };
  updateGameStatus();
});
document.getElementById("game-canvas").addEventListener("pointerup", () => {
  gameDragPoint = null;
});
document.getElementById("game-canvas").addEventListener("pointercancel", () => {
  gameDragPoint = null;
});

document.getElementById("game-mine").addEventListener("click", mineGameBlock);
document.getElementById("game-place").addEventListener("click", placeGameBlock);
document.getElementById("game-jump").addEventListener("click", jumpGamePlayer);
document.getElementById("inventory-close").addEventListener("click", () => toggleGameInventory(false));
document.getElementById("game-release-mouse").addEventListener("click", () => {
  if (document.pointerLockElement === gameCanvas) {
    document.exitPointerLock();
  }
});
document.querySelectorAll("[data-move]").forEach((button) => {
  const keyForMove = { forward: "w", back: "s", left: "a", right: "d" }[button.dataset.move];
  button.addEventListener("click", () => {
    gameKeys.add(keyForMove);
    moveGamePlayer(0.2);
    gameKeys.delete(keyForMove);
    updateGameStatus();
    renderGame();
  });
  button.addEventListener("pointerdown", (event) => {
    event.preventDefault();
    gameKeys.add(keyForMove);
    gameHeldButtons.set(event.pointerId, keyForMove);
    button.setPointerCapture(event.pointerId);
  });
  const release = (event) => {
    const key = gameHeldButtons.get(event.pointerId);
    if (key) {
      gameKeys.delete(key);
      gameHeldButtons.delete(event.pointerId);
    }
  };
  button.addEventListener("pointerup", release);
  button.addEventListener("pointercancel", release);
});
document.querySelectorAll("[data-turn]").forEach((button) => {
  button.addEventListener("click", () => {
    gameYaw += button.dataset.turn === "left" ? -0.3 : 0.3;
    markGameDirty();
    renderGame();
    updateGameStatus();
  });
});
document.addEventListener("keydown", (event) => handleGameKey(event, true));
document.addEventListener("keyup", (event) => handleGameKey(event, false));
window.addEventListener("blur", () => {
  gameKeys.clear();
  gameHeldButtons.clear();
});

document.getElementById("calculator-form").addEventListener("submit", (event) => {
  event.preventDefault();
  try {
    const result = calculateExpression(document.getElementById("expression").value);
    showToolResult("calculator", `= ${formatNumber(result)}`);
  } catch (error) {
    showToolError("calculator", error);
  }
});

document.getElementById("fraction-form").addEventListener("submit", (event) => {
  event.preventDefault();
  try {
    const left = parseValue(document.getElementById("fraction-left").value);
    const right = parseValue(document.getElementById("fraction-right").value);
    const operation = document.getElementById("fraction-operation").value;
    let result;
    if (operation === "add") {
      result = add(left, right);
    } else if (operation === "subtract") {
      result = subtract(left, right);
    } else if (operation === "multiply") {
      result = multiply(left, right);
    } else {
      result = divide(left, right);
    }
    const symbols = { add: "+", subtract: "−", multiply: "×", divide: "÷" };
    showToolResult(
      "fraction",
        `${formatDisplayFraction(left)} ${symbols[operation]} ${formatDisplayFraction(right)} = ${formatDisplayFraction(result)} (${formatNumber(Number(result.n) / Number(result.d))})`,
    );
  } catch (error) {
    showToolError("fraction", error);
  }
});

document.getElementById("algebra-form").addEventListener("submit", (event) => {
  event.preventDefault();
  try {
    const result = solveLinearEquation(document.getElementById("algebra-equation").value);
    showToolResult("algebra", result.answer);
  } catch (error) {
    showToolError("algebra", error);
  }
});

document.getElementById("geometry-shape").addEventListener("change", (event) => {
  document.querySelectorAll("[data-shape-fields]").forEach((fields) => {
    fields.hidden = fields.dataset.shapeFields !== event.target.value;
  });
  document.getElementById("geometry-result").hidden = true;
  document.getElementById("geometry-error").hidden = true;
});

document.getElementById("geometry-form").addEventListener("submit", (event) => {
  event.preventDefault();
  try {
    const shape = document.getElementById("geometry-shape").value;
    showToolResult("geometry", calculateGeometry(shape));
  } catch (error) {
    showToolError("geometry", error);
  }
});

document.getElementById("roots-form").addEventListener("submit", (event) => {
  event.preventDefault();
  try {
    const rawValue = document.getElementById("radicand").value.trim();
    const value = Number(rawValue);
    if (!rawValue || !Number.isFinite(value)) {
      throw new Error("Enter a valid number.");
    }
    if (value < 0) {
      throw new Error("This tool finds real square roots; negative numbers do not have real square roots.");
    }
    const approximation = formatNumber(Math.sqrt(value));
    if (Number.isSafeInteger(value) && value <= 1_000_000_000_000) {
      showToolResult(
        "roots",
        `√${formatNumber(value)} = ${simplifySquareRoot(value)}${Number.isInteger(Math.sqrt(value)) ? "" : ` ≈ ${approximation}`}`,
      );
    } else {
      showToolResult("roots", `√${formatNumber(value)} ≈ ${approximation}`);
    }
  } catch (error) {
    showToolError("roots", error);
  }
});

document.getElementById("solver-form").addEventListener("submit", (event) => {
  event.preventDefault();
  try {
    const inputs = parseList(document.getElementById("inputs").value, "Inputs");
    const outputs = parseList(document.getElementById("outputs").value, "Outputs");
    showResult(inputs, outputs, findPolynomialRule(inputs, outputs));
  } catch (error) {
    document.getElementById("rule-result").hidden = true;
    const errorElement = document.getElementById("rule-error");
    errorElement.textContent = error.message;
    errorElement.hidden = false;
  }
});
