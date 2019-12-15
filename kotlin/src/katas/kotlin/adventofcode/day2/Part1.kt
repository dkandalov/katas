package katas.kotlin.adventofcode.day2

import kotlincommon.*
import java.io.*

fun main() {
    val text = File("src/katas/kotlin/adventofcode/day2/input.txt").readText()
    val program = text.split(",").map(String::toInt).toMutableList()
    println(execute(ArrayList(program), programInput1 = 12, programInput2 = 2))
}

fun execute(program: MutableList<Int>, programInput1: Int, programInput2: Int): Int {
    program[1] = programInput1
    program[2] = programInput2
    val commands = toCommands(program)
    for (it in commands) {
        if (it is Halt) break
        it.printed().execute(program)
    }
    return program[0]
}

private fun toCommands(tokens: List<Int>, shift: Int = 0): List<Command> {
    if (shift >= tokens.size) return emptyList()
    return when (tokens[shift]) {
        1 -> listOf(Add(
            input1 = tokens[shift + 1],
            input2 = tokens[shift + 2],
            output = tokens[shift + 3]
        )) + toCommands(tokens, shift + 4)
        2 -> listOf(Multiply(
            input1 = tokens[shift + 1],
            input2 = tokens[shift + 2],
            output = tokens[shift + 3]
        )) + toCommands(tokens, shift + 4)
        99 -> listOf(Halt) + toCommands(tokens, shift + 1)
        else -> error("Unexpected command ${tokens[shift]}")
    }
}

interface Command {
    fun execute(program: MutableList<Int>)
}

data class Add(val input1: Int, val input2: Int, val output: Int): Command {
    override fun execute(program: MutableList<Int>) {
        program[output] = program[input1] + program[input2]
    }
}

data class Multiply(val input1: Int, val input2: Int, val output: Int): Command {
    override fun execute(program: MutableList<Int>) {
        program[output] = program[input1] * program[input2]
    }
}

object Halt: Command {
    override fun execute(program: MutableList<Int>) {
        error("")
    }

    override fun toString() = "Halt"
}