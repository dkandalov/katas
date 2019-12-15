package katas.kotlin.adventofcode.day5

import katas.kotlin.adventofcode.day5.ParamMode.*
import java.io.*

fun main() {
    val text = File("src/katas/kotlin/adventofcode/day5/input.txt").readText()
    val program = text.split(",").map(String::toInt).toMutableList()
    val io = object: IO {
        override fun read() = 1
        override fun write(value: Int) = println(value)
    }
    execute(program, io)
}


fun execute(program: MutableList<Int>, io: IO) {
    var address = 0
    while (address < program.size) {
        val opCode = program[address].toString().takeLast(2).toInt()
        val paramModes = program[address].toString().dropLast(2)
            .map { it.toString().toInt() }
            .let { List(3 - it.size) { 0 } + it }
            .map { if (it == 0) PositionMode else ImmediateMode }
            .reversed()

        fun param(index: Int) = paramModes[index - 1].read(address + index, program)
        fun paramValue(index: Int) = ImmediateMode.read(address + index, program)

        when (opCode) {
            1    -> {
                program[paramValue(3)] = param(1) + param(2)
                address += 4
            }
            2    -> {
                program[paramValue(3)] = param(1) * param(2)
                address += 4
            }
            3    -> {
                program[paramValue(1)] = io.read()
                address += 2
            }
            4    -> {
                io.write(param(1))
                address += 2
            }
            5    -> address = if (param(1) != 0) param(2) else address + 3
            6    -> address = if (param(1) == 0) param(2) else address + 3
            7    -> {
                program[paramValue(3)] = if (param(1) < param(2)) 1 else 0
                address += 4
            }
            8    -> {
                program[paramValue(3)] = if (param(1) == param(2)) 1 else 0
                address += 4
            }
            99   -> return
            else -> error("Unexpected opCode ${program[address]} at index $address")
        }
    }
}

enum class ParamMode {
    PositionMode, ImmediateMode;

    fun read(index: Int, program: List<Int>): Int =
        when (this) {
            PositionMode  -> program[program[index]]
            ImmediateMode -> program[index]
        }
}

interface IO {
    fun read(): Int
    fun write(value: Int)
}