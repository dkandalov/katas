package katas.kotlin.coroutines.amb

import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.createCoroutine
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun main(args: Array<String>) {
    var savedC: Continuation<Unit>? = null
    ResumableFunction.create {

        suspendCoroutineOrReturn { c: Continuation<Unit> ->
            println("")
        }
    }
/*
    ResumableFunction.create {
        val value = amb(mutableListOf(1, 2, 3))
        println("value = $value")
        assertCondition { value == 2 }
        println("! " + value)
    }
*/
}

private class ResumableFunction {
    var c: Continuation<Unit>? = null

    fun assertCondition(f: () -> Boolean) {
        if (!f()) fail()
    }

    fun fail() {
        c?.resume(Unit)
    }

    suspend fun amb(choices: MutableList<Int>): Int {
        suspendCoroutineOrReturn { it: Continuation<Unit> ->
            c = it
            Unit
        }
        return choices.removeAt(0)
    }

    companion object {
        fun create(block: suspend ResumableFunction.() -> Unit): ResumableFunction {
            val f = ResumableFunction()
            block.createCoroutine(f, completion = EmptyContinuation).resume(Unit)
            return f
        }
    }
}
