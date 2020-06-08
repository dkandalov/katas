package katas.kotlin.leetcode.k_closest_points_to_origin

import datsok.shouldEqual
import org.junit.Test
import kotlin.math.sqrt

class KClosestPointsToOriginTests {
    @Test fun examples() {
        kClosest(listOf(Point(1, 1), Point(2, 2)), k = 2) shouldEqual setOf(Point(1, 1), Point(2, 2))
        kClosest(listOf(Point(3, 3), Point(2, 2), Point(1, 1)), k = 2) shouldEqual setOf(Point(1, 1), Point(2, 2))
        kClosest(listOf(Point(4, 4), Point(3, 3), Point(2, 2), Point(1, 1)), k = 2) shouldEqual setOf(Point(1, 1), Point(2, 2))

        val hundredPoints = (1..100).map { Point(it, it) }.shuffled()
        kClosest(hundredPoints, k = 3) shouldEqual setOf(Point(1, 1), Point(2, 2), Point(3, 3))

        kClosest(listOf(Point(1, 3), Point(-2, 2)), k = 1) shouldEqual setOf(Point(-2, 2))
        kClosest(listOf(Point(3, 3), Point(5, -1), Point(-2, 4)), k = 2) shouldEqual setOf(Point(3, 3), Point(-2, 4))
    }
}

private fun kClosest(points: List<Point>, k: Int): Set<Point> {
    require(k <= points.size)

    var from = 0
    var to = points.size
    val pivot = points.first()
    val (left, right) = points.partition { it.distanceToOrigin < pivot.distanceToOrigin }
//    if (left.size < k)

    return points.sortedBy { it.distanceToOrigin }.take(k).toSet()
}

private fun kClosest_(points: List<Point>, k: Int) = points.sortedBy { it.distanceToOrigin }.take(k)

private data class Point(val x: Int, val y: Int) {
    val distanceToOrigin: Double = sqrt(x.toDouble() * x + y * y)
}