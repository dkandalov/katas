describe("Game of life", function() {
  it("empty universe stays empty", function() {
    var cells = [];
    expect(nextGeneration(cells)).toEqual([]);
  });
  it("underpopulated dies when no neighbors", function() {
    var cells = [[1,1]];
    expect(nextGeneration(cells)).toEqual([]);
  });
  //it("cell with enough neighbours survives", function() {
  //  var cells = [[0,1], [1,1], [1, 2]];
  //  expect(nextGeneration(cells)).toEqual([[0,1], [1,1], [1, 2]]);
  //});
   /*
  it("find cell neighbours", function() {
    var aCell = cell(0,0);
    var allCells = [cell(0,0)];
    var expectedNeighbours = [];
    expect(findNeighboursOf(aCell, allCells)).toEqual(expectedNeighbours);

    aCell = cell(0,0);
    var anotherCell = cell(0,1);
    allCells = [aCell, anotherCell];
    var expectedNeighbours = [anotherCell];
    expect(findNeighboursOf(aCell, allCells)).toEqual(expectedNeighbours);
  })*/
  it("find cell neighbours", function() {
    aCell = cell(0,0);
    var anotherCell = cell(0,1);
    var cell3 = cell(1,0);
    allCells = [aCell, anotherCell,cell3];
    var expectedNeighbours = [anotherCell,cell3];
    expect(findNeighboursOf(aCell, allCells)).toEqual(expectedNeighbours);
  });
});

function cell(x, y) {
  return {x: x, y: y};
}

function nextGeneration(cells) {
  return [];
}

function findNeighboursOf(cell, allCells) {
  var neighbours = [];
  allCells.forEach(function(aCell) {
    if (_.isEqual(cell, aCell)) return; // cell is not a neighbour of itself
    if (cell.x == aCell.x - 1 || cell.x == aCell.x + 1 ||
        cell.y == aCell.y - 1 || cell.y == aCell.y + 1) {
      neighbours.push(aCell);       i wasn't ertina'
    }
  });
  return neighbours;
}