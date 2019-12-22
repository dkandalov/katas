package katas.kotlin.adventofcode.day7

import katas.kotlin.adventofcode.day5.*
import kotlincommon.*
import java.io.*

fun main() {
    val program = File("src/katas/kotlin/adventofcode/day7/input.txt")
        .readText().split(",").map(String::toInt).toMutableList()

    val maxOutput = (5..9).permutations()
        .map { (phase1, phase2, phase3, phase4, phase5) ->
            var output5: Sequence<Int> = emptySequence()
            val mixedInput = sequence {
                yield(0)
                output5.forEach { yield(it) }
            }
            val output1 = SequencingAmp(input = mixedInput, phase = phase1).run(program)
            val output2 = SequencingAmp(input = output1, phase = phase2).run(program)
            val output3 = SequencingAmp(input = output2, phase = phase3).run(program)
            val output4 = SequencingAmp(input = output3, phase = phase4).run(program)
            output5 = SequencingAmp(input = output4, phase = phase5).run(program)
            output5.last()
        }
        .max()

    println(maxOutput) // 76211147
}

class SequencingAmp(val input: Sequence<Int>, val phase: Int) {
    fun run(program: MutableList<Int>): Sequence<Int> = sequence {
        val allInputs = (sequenceOf(phase) + input).iterator()
        execute(
            program,
            read = { allInputs.next() },
            write = { yield(it) }
        )
    }
}


