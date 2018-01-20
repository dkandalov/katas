package katas.kotlin.coroutines.steps

import kotlincommon.printed
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.CoroutineContext
import kotlin.coroutines.experimental.EmptyCoroutineContext
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.createCoroutineUnchecked
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun main(args: Array<String>) {
    Step2.run()
}

object Step2 {
    fun run() {
        create {
            "c begin".printed()

            var y = yield(1)
            println("c $y")

            y = yield(2)
            println("c $y")

            "c end".printed()
        }
        resume(-1).printed{ "m $it" }
        resume(-2).printed{ "m $it" }
        resume(-3).printed{ "m $it" }
        resume(-4).printed{ "m $it" } // continues from "println(3)" again 😱
    }

    var c: Continuation<Unit>? = null
    var coInput: Int? = null
    var coOutput: Int? = null

    fun create(block: suspend () -> Unit): Continuation<Unit> {
        val continuation = MyContinuation()
        c = block.createCoroutineUnchecked(continuation)
        return c!!
    }

    suspend fun yield(value: Int): Int? {
        suspendCoroutineOrReturn { it: Continuation<Unit> ->
            c = it
            coOutput = value
            COROUTINE_SUSPENDED
        }
        return coInput
    }

    fun resume(value: Int? = null): Int? {
        val cNN = c ?: return coOutput
        coInput = value
        cNN.resume(Unit)
        return coOutput
    }

    class MyContinuation: Continuation<Unit> {
        override val context: CoroutineContext = EmptyCoroutineContext
        override fun resume(value: Unit) {}
        override fun resumeWithException(exception: Throwable) = throw exception
    }
}
