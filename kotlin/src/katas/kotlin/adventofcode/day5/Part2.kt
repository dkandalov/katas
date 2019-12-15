package katas.kotlin.adventofcode.day5

import java.io.*

fun main() {
    val text = File("src/katas/kotlin/adventofcode/day5/input.txt").readText()
    val program = text.split(",").map(String::toInt).toMutableList()
    val io = object: IO {
        override fun read() = 5
        override fun write(value: Int) = println(value)
    }
    execute(program, io)
}
