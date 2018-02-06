function manhattanDistance(x1, x2, y1, y2) {
  return abs(x2 - x1) + abs(y2 - y1)
}

function getCoords(num) {
  let exp = 0

}

function getDistanceBetweenNums(num1, num2) {
  const coords1 = getCoords(num1)
  const coords2 = getCoords(num2)
  return manhattanDistance(...coords1, ...coords2)
}