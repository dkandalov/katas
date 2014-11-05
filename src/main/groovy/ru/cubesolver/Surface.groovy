package ru.cubesolver

class Surface {
  static String _ = " "
  static String x = "x"

  final List<List> data
  final int lastIndex


  static surface(String s) {
    def rows = s.trim().split("\n").collect {
      it.trim().chars.collect{ it == "x" ? x : _ }
    }
    new Surface(rows)
  }

  static surface(List... rows) {
    new Surface(rows)
  }

  Surface(List... rows) {
    this(rows.toList())
  }

  Surface(List<List> data) {
    this.data = data
    this.lastIndex = data[0].size() - 1
  }

  List topSide() {
    data[0]
  }

  List bottomSide() {
    data[lastIndex]
  }

  List rightSide() {
    (0..lastIndex).collect{ data[it][lastIndex] }
  }

  List leftSide() {
    (0..lastIndex).collect{ data[it][0] }
  }

  def getTopRight() {
    topSide()[lastIndex]
  }

  def getBottomRight() {
    bottomSide()[lastIndex]
  }

  def getTopLeft() {
    topSide()[0]
  }

  def getBottomLeft() {
    bottomSide()[0]
  }

  int size() {
    lastIndex + 1
  }

  Surface rotateRight() {
    def newData =
        (0..lastIndex).collect { column ->
          (lastIndex..0).collect { row ->
            data[row][column]
          }
        }
    new Surface(newData)
  }

  Surface horizontalFlip() {
    def newData =
        (0..lastIndex).collect { row ->
          (lastIndex..0).collect { column ->
            data[row][column]
          }
        }
    new Surface(newData)
  }

  List<Surface> rotations() {
    def rotate = { Surface aSurface ->
      def result = [aSurface]
      (0..2).each {
        aSurface = aSurface.rotateRight()
        result << aSurface
      }
      result
    }
    rotate(this) + rotate(this.horizontalFlip())
  }

  LinkedHashSet<Surface> combinations(LinkedHashSet<Surface> result = []) {
    positions().each { row, column ->
      if (isValidUpdate(row, column)) {
        def updateSurface = copy()
        updateSurface.data[row][column] = x
        if (!result.contains(updateSurface)) {
          result.add(updateSurface)
          updateSurface.combinations(result)
        }
      }
    }
    result
  }

  private boolean isValidUpdate(int row, int column) {
    if (row == 0 && column == 0) topSide()[1] == x || leftSide()[1] == x
    else if (row == 0 && column == lastIndex) topSide()[lastIndex - 1] == x || rightSide()[1] == x
    else if (row == lastIndex && column == lastIndex) bottomSide()[lastIndex - 1] == x || rightSide()[lastIndex - 1] == x
    else if (row == lastIndex && column == 0) bottomSide()[1] == x || leftSide()[lastIndex - 1] == x
    else data[row][column] != x
  }

  private Surface copy() {
    new Surface(data.collect{ it.clone() }.toList())
  }

  private positions(int row = 0, int column = -1) {
    new Iterator() {
      @Override Object next() {
        column++
        if (column > lastIndex) {
          row++
          column = 0
        }
        [row, column]
      }

      @Override boolean hasNext() {
        row != lastIndex || column != lastIndex
      }
    }
  }

  @Override String toString() {
    data.collect{
      it.collect { it == x ? "x" : "-" }.join("")
    }.join("\n")
  }

  @Override boolean equals(o) {
    if (this.is(o)) return true
    if (getClass() != o.class) return false

    Surface surface = (Surface) o

    if (data != surface.data) return false

    return true
  }

  @Override int hashCode() {
    return (data != null ? data.hashCode() : 0)
  }

  static boolean areConnectible(List side1, List side2) {
    int centerIndex1 = side1.size().intdiv(2)
    int centerIndex2 = (side1.size() - 1).intdiv(2)
    [side1, side2].transpose().every { it[0] == _ || it[1] == _ } &&
    (side1[centerIndex1] == x || side2[centerIndex1] == x) &&
    (side1[centerIndex2] == x || side2[centerIndex2] == x)
  }
}
