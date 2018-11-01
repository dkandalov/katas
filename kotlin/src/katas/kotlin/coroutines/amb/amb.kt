package katas.kotlin.coroutines.amb

import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import java.util.*
import kotlin.coroutines.Continuation
import kotlin.coroutines.createCoroutine
import kotlin.coroutines.intrinsics.suspendCoroutineUninterceptedOrReturn
import kotlin.coroutines.resume

fun main(args: Array<String>) {
    ResumableFunction.create {
        val value = amb(mutableListOf(1, 2, 3), "value")
        val value2 = amb(mutableListOf(1, 2, 3), "value2")

//        println("value = $value; value2 = $value2")
        assertCondition { value + value2 == 5 }

        println("! " + value + " " + value2)
    }
}

private class ResumableFunction {
    val continuations: LinkedList<Continuation<Unit>> = LinkedList()

    inline fun assertCondition(f: () -> Boolean) {
        if (!f()) fail()
    }

    fun fail() {
        continuations.removeLast().resume(Unit)
        throw Exit()
    }

    suspend fun amb(choices: MutableList<Int>, name: String = ""): Int {
        lateinit var c: Continuation<Unit>
        suspendCoroutineUninterceptedOrReturn { it: Continuation<Unit> -> c = it; Unit }
        continuations.addLast(c)

        if (choices.isEmpty()) {
            continuations.removeLast()
            if (continuations.isNotEmpty()) {
                fail()
            } else {
                throw ExhaustedAllChoices()
            }
        }
        println("$name = ${choices.first()}")
        return choices.removeAt(0)
    }

    companion object {
        fun create(block: suspend ResumableFunction.() -> Unit): ResumableFunction {
            val f = ResumableFunction()
            try {
                block.createCoroutine(f, completion = EmptyContinuation).resume(Unit)
            } catch (ignored: Exit) {
            }
            return f
        }
    }

    class ExhaustedAllChoices: Throwable()
    private class Exit: Throwable()
}
