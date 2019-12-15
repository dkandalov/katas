package katas.kotlin.adventofcode.day2

import java.io.*

fun main() {
    val text = File("src/katas/kotlin/adventofcode/day2/input.txt").readText()
    val program = text.split(",").map(String::toInt).toMutableList()
    (0..99).forEach { i ->
        (0..99).forEach { j ->
            val result = execute(ArrayList(program), programInput1 = i, programInput2 = j)
            if (result == 19690720) {
                println(i)
                println(j)
                return
            }
        }
    }
}
