package katas.kotlin.adventofcode.day7

import katas.kotlin.adventofcode.day5.*
import nonstdlib.*
import java.io.*
import java.util.*

fun main() {
    val program = File("src/katas/kotlin/adventofcode/day7/input.txt")
        .readText().split(",").map(String::toInt).toMutableList()

    val maxOutput = (0..4).permutations()
        .map { (phase1, phase2, phase3, phase4, phase5) ->
            val output1 = Amp(input = 0, phase = phase1).run(program)
            val output2 = Amp(input = output1, phase = phase2).run(program)
            val output3 = Amp(input = output2, phase = phase3).run(program)
            val output4 = Amp(input = output3, phase = phase4).run(program)
            val output5 = Amp(input = output4, phase = phase5).run(program)
            output5
        }
        .maxOrNull()

    println(maxOutput)
}

class Amp(val input: Int, val phase: Int) {
    fun run(program: MutableList<Int>): Int {
        val inputs = LinkedList<Int>().apply {
            add(phase)
            add(input)
        }
        var output: Int? = null
        execute(program, object: IO {
            override fun read() = inputs.removeFirst()
            override fun write(value: Int) {
                output = value
            }
        })
        return output ?: error("")
    }
}