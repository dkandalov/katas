package katas.kotlin.coroutines.steps

import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.CoroutineContext
import kotlin.coroutines.experimental.EmptyCoroutineContext
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.createCoroutineUnchecked
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun main(args: Array<String>) {
    Step1.run()
}

object Step1 {
    fun run() {
        val continuation = cc {
            println(1)
            yeeld()
            println(2)
            yeeld()
            println(3)
        }
        continuation.resume(Unit)
        c!!.resume(Unit)
        c!!.resume(Unit)
        c!!.resume(Unit) // continues from "println(3)" again 😱
    }

    var c: Continuation<Unit>? = null

    fun cc(block: suspend () -> Unit): Continuation<Unit> {
        val continuation = MyContinuation()
        return block.createCoroutineUnchecked(continuation)
    }

    suspend fun yeeld() {
        suspendCoroutineOrReturn { it: Continuation<Unit> ->
            c = it
            COROUTINE_SUSPENDED
        }
    }

    class MyContinuation: Continuation<Unit> {
        override val context: CoroutineContext = EmptyCoroutineContext
        override fun resume(value: Unit) {}
        override fun resumeWithException(exception: Throwable) = throw exception
    }
}
