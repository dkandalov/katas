package katas.kotlin.adventofcode.day3

import java.io.*
import kotlin.math.*

fun main() {
    val lines = File("src/katas/kotlin/adventofcode/day3/input.txt").readLines()
    val positions1 = movesToPositions(moves = lines[0].split(","))
    val positions2 = movesToPositions(moves = lines[1].split(","))
    println(positions1.size)
    println(positions2.size)

    val intersections = positions1.toSet().intersect(positions2.toSet()) - Position(0, 0)
    val minPosition = intersections.minBy(::manhattanDistance)!!
    println(manhattanDistance(minPosition))
}

private fun manhattanDistance(it: Position) = it.x.absoluteValue + it.y.absoluteValue

fun movesToPositions(moves: List<String>): MutableList<Position> {
    val positions = mutableListOf(Position(0, 0))
    moves.forEach { move ->
        val shift = move.drop(1).toInt()
        val last = positions.last()
        val elements = when (move.first()) {
            'R'  -> (last.x..last.x + shift).map { Position(it, last.y) }
            'L'  -> (last.x - shift..last.x).reversed().map { Position(it, last.y) }
            'U'  -> (last.y - shift..last.y).reversed().map { Position(last.x, it) }
            'D'  -> (last.y..last.y + shift).map { Position(last.x, it) }
            else -> error("")
        }
        positions.addAll(elements.drop(1))
    }
    return positions
}

data class Position(val x: Int, val y: Int)