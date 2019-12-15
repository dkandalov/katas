package katas.kotlin.adventofcode.day3

import java.io.*

fun main() {
    val lines = File("src/katas/kotlin/adventofcode/day3/input.txt").readLines()
    val positions1 = movesToPositions(moves = lines[0].split(","))
    val positions2 = movesToPositions(moves = lines[1].split(","))

    println(countStepsToFirstIntersection(positions1, positions2))
}

fun countStepsToFirstIntersection(positions1: MutableList<Position>, positions2: MutableList<Position>): Int {
    positions1.forEachIndexed { i1, p1 ->
        positions2.forEachIndexed { i2, p2 ->
            if (p1 != Position(0, 0) && p1 == p2) return i1 + i2
        }
    }
    error("Found no intersections")
}