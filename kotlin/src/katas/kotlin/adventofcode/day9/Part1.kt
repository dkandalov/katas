package katas.kotlin.adventofcode.day9

import katas.kotlin.adventofcode.day9.ParamMode.*
import java.io.*
import java.util.*

fun main() {
    val text = File("src/katas/kotlin/adventofcode/day9/input.txt").readText()
    val program = text.split(",").map(String::toLong)
    execute(program, read = { 1 }).forEach { println(it) }
}

fun execute(program: List<Long>, read: () -> Long): List<Long> {
    val programAsMap = program
        .mapIndexed { index, value -> index.toLong() to value }
        .toMap(TreeMap())
    return sequence { execute(programAsMap, read, { yield(it) }) }.toList()
}

private inline fun execute(program: TreeMap<Long, Long>, read: () -> Long, write: (Long) -> Unit) {
    var instructionPointer = 0L
    var relativeBaseOffset = 0L

    while (instructionPointer < program.size) {
        val opCode = program[instructionPointer].toString().takeLast(2).toInt()
        val paramModes = program[instructionPointer].toString().dropLast(2)
            .map { it.toString().toInt() }
            .let { List(3 - it.size) { 0 } + it }
            .map {
                when (it) {
                    0    -> PositionMode
                    1    -> ImmediateMode
                    2    -> RelativeMode
                    else -> error("")
                }
            }
            .reversed()

        val param = { index: Int -> paramModes[index - 1].read(instructionPointer + index, program, relativeBaseOffset) }
        val paramValue = { index: Int -> ImmediateMode.read(instructionPointer + index, program, relativeBaseOffset) }

        when (opCode) {
            1    -> {
                program[paramValue(3)] = param(1) + param(2)
                instructionPointer += 4
            }
            2    -> {
                program[paramValue(3)] = param(1) * param(2)
                instructionPointer += 4
            }
            3    -> {
                program[paramValue(1)] = read()
                instructionPointer += 2
            }
            4    -> {
                write(param(1))
                instructionPointer += 2
            }
            5    -> instructionPointer = if (param(1) != 0L) param(2) else instructionPointer + 3
            6    -> instructionPointer = if (param(1) == 0L) param(2) else instructionPointer + 3
            7    -> {
                program[paramValue(3)] = if (param(1) < param(2)) 1L else 0L
                instructionPointer += 4
            }
            8    -> {
                program[paramValue(3)] = if (param(1) == param(2)) 1L else 0L
                instructionPointer += 4
            }
            9    -> {
                relativeBaseOffset += paramValue(1)
                instructionPointer += 2
            }
            99   -> return
            else -> error("Unexpected opCode ${program[instructionPointer]} at index $instructionPointer")
        }
    }
}

enum class ParamMode {
    PositionMode, ImmediateMode, RelativeMode;

    fun read(index: Long, program: TreeMap<Long, Long>, relativeBaseOffset: Long): Long {
        require(index >= 0)
        val absoluteIndex = when (this) {
            PositionMode  -> program.getOrDefault(index, 0)
            ImmediateMode -> index
            RelativeMode  -> relativeBaseOffset + program.getOrDefault(index, 0)
        }
        require(absoluteIndex >= 0)
        return program.getOrDefault(absoluteIndex, 0)
    }
}
