package katas.kotlin.leetcode.max_points_on_a_line

import datsok.shouldEqual
import org.junit.jupiter.api.Test
import kotlin.collections.component1
import kotlin.collections.component2
import kotlin.math.absoluteValue


//
// https://leetcode.com/problems/max-points-on-a-line ❌
//
// Given n points on a 2D plane, find the maximum number of points that lie on the same straight line.
//
// Example 1
// Input: [[1,1],[2,2],[3,3]]
// Output: 3
// Explanation:
// ^
// |
// |        o
// |     o
// |  o
// +------------->
// 0  1  2  3  4
//
// Example 2
// Input: [[1,1],[3,2],[5,3],[4,1],[2,3],[1,4]]
// Output: 4
// Explanation:
// ^
// |
// |  o
// |     o        o
// |        o
// |  o        o
// +------------------->
// 0  1  2  3  4  5  6



fun maxPoints(points: Array<IntArray>): Int {
    return lineWithMaxPoints(points.map { Point(it[0], it[1]) }).size
}

private fun lineWithMaxPoints(points: List<Point>): List<Point> {
    if (points.size <= 2) return points
    return sequence {
        points.forEachIndexed { i1, point1 ->
            points.drop(i1 + 1).forEachIndexed { i2, point2 ->
                val line = Line()
                line.add(point1)
                line.add(point2)
                points.drop(i1 + 1 + i2 + 1).forEach { point3 ->
                    line.add(point3)
                }
                yield(line)
            }
        }
    }.maxByOrNull { it.size }!!.points
}

data class Point(val x: Int, val y: Int) {
    override fun toString() = "[$x,$y]"
}

data class Line(val points: ArrayList<Point> = ArrayList()) {
    val uniquePoints: HashSet<Point> = HashSet()
    val size: Int get() = points.size

    fun add(point: Point) {
        if (uniquePoints.size <= 1 || isOnTheLine(point)) {
            uniquePoints.add(point)
            points.add(point)
        }
    }

    private fun isOnTheLine(p3: Point): Boolean {
        val (p1, p2) = uniquePoints.take(2)
        if (p1 == p3 || p2 == p3) return true
        if (p1.y == p2.y && p2.y == p3.y) return true
        if (p1.x == p2.x) return p1.x == p3.x

        val a = (p1.y - p2.y).toDouble() / (p1.x - p2.x)
        val b = p1.y - a * p1.x
        val b3 = p3.y - a * p3.x
        return (b - b3).absoluteValue < 0.000_000_001
    }
}

val Int.absoluteValue: Int get() = Math.abs(this)
val Double.absoluteValue: Double get() = Math.abs(this)

class Tests {
    @Test fun `some examples`() {
        lineWithMaxPoints(listOf(Point(1, 1), Point(2, 2), Point(3, 3))) shouldEqual
            listOf(Point(1, 1), Point(2, 2), Point(3, 3))

        lineWithMaxPoints(listOf(Point(1, 1), Point(3, 2), Point(5, 3), Point(4, 1), Point(2, 3), Point(1, 4))) shouldEqual
            listOf(Point(3, 2), Point(4, 1), Point(2, 3), Point(1, 4))

        lineWithMaxPoints(listOf(Point(0, 0), Point(1, 1), Point(1, -1))) shouldEqual
            listOf(Point(0, 0), Point(1, 1))

        lineWithMaxPoints(listOf(Point(1, 1), Point(1, 1), Point(2, 2), Point(2, 2))) shouldEqual
            listOf(Point(1, 1), Point(1, 1), Point(2, 2), Point(2, 2))
    }

    @Test fun `failing example`() {
        val points = (0..120).map { Point(-10 + it, 87 - it) }.shuffled()
        lineWithMaxPoints(points).size shouldEqual 121


        lineWithMaxPoints("""[[40,-23],[9,138],[429,115],[50,-17],[-3,80],[-10,33],[5,-21],[-3,80],[-6,-65],
            [-18,26],[-6,-65],[5,72],[0,77],[-9,86],[10,-2],[-8,85],[21,130],[18,-6],[-18,26],[-1,-15],[10,-2],[8,69],
            [-4,63],[0,3],[-4,40],[-7,84],[-8,7],[30,154],[16,-5],[6,90],[18,-6],[5,77],[-4,77],[7,-13],[-1,-45],[16,-5],
            [-9,86],[-16,11],[-7,84],[1,76],[3,77],[10,67],[1,-37],[-10,-81],[4,-11],[-20,13],[-10,77],[6,-17],[-27,2],[-10,-81],
            [10,-1],[-9,1],[-8,43],[2,2],[2,-21],[3,82],[8,-1],[10,-1],[-9,1],[-12,42],[16,-5],[-5,-61],[20,-7],[9,-35],[10,6],
            [12,106],[5,-21],[-5,82],[6,71],[-15,34],[-10,87],[-14,-12],[12,106],[-5,82],[-46,-45],[-4,63],[16,-5],[4,1],[-3,-53],
            [0,-17],[9,98],[-18,26],[-9,86],[2,77],[-2,-49],[1,76],[-3,-38],[-8,7],[-17,-37],[5,72],[10,-37],[-4,-57],[-3,-53],[3,74],
            [-3,-11],[-8,7],[1,88],[-12,42],[1,-37],[2,77],[-6,77],[5,72],[-4,-57],[-18,-33],[-12,42],[-9,86],[2,77],[-8,77],[-3,77],
            [9,-42],[16,41],[-29,-37],[0,-41],[-21,18],[-27,-34],[0,77],[3,74],[-7,-69],[-21,18],[27,146],[-20,13],[21,130],[-6,-65],
            [14,-4],[0,3],[9,-5],[6,-29],[-2,73],[-1,-15],[1,76],[-4,77],[6,-29]]""".trimIndent().toPoints()).size shouldEqual 25
    }

    @Test fun `points on a horizontal line`() {
        lineWithMaxPoints(listOf(Point(1, 3), Point(2, 3), Point(10, 3))) shouldEqual
            listOf(Point(1, 3), Point(2, 3), Point(10, 3))
    }

    @Test fun `points on a vertical line`() {
        lineWithMaxPoints(listOf(Point(3, 1), Point(3, 2), Point(3, 10))) shouldEqual
            listOf(Point(3, 1), Point(3, 2), Point(3, 10))
    }

    @Test fun `points on parallel lines`() {
        lineWithMaxPoints(listOf(Point(5, 5), Point(10, 10), Point(6, 4), Point(11, 9))) shouldEqual
            listOf(Point(5, 5), Point(10, 10))
    }

    @Test fun `duplicate points`() {
        lineWithMaxPoints(listOf(Point(1, 1), Point(1, 1), Point(2, 3))) shouldEqual
            listOf(Point(1, 1), Point(1, 1), Point(2, 3))
    }

    @Test fun `two almost the same points far from (0,0)`() {
        lineWithMaxPoints(listOf(Point(0, 0), Point(94_911_150, 94_911_151), Point(94_911_151, 94_911_152))) shouldEqual
            listOf(Point(0, 0), Point(94_911_150, 94_911_151))
    }

    private fun String.toPoints(): List<Point> {
        return replace(Regex("\\s"), "").replace("[[", "").replace("]]", "").split("],[")
            .map { it.split(",") }.map { Point(it[0].toInt(), it[1].toInt()) }
    }
}
