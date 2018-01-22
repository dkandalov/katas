@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package katas.kotlin.coroutines.steps

import katas.kotlin.coroutines.steps.Step2.YieldingFunction.Companion.create
import kotlincommon.printed
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.CoroutineContext
import kotlin.coroutines.experimental.EmptyCoroutineContext
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.createCoroutineUnchecked
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun main(args: Array<String>) {
    val f = create<String> {
        "coroutine: begin".printed()

        val y1 = yield("1")
        println("coroutine: $y1")
        val y2 = yield("2")
        println("coroutine: $y2")

        "coroutine: end".printed()
    }
    println("main: ${f.resume("a")}")
    println("main: ${f.resume("b")}")
    println("main: ${f.resume("c")}")
    println("main: ${f.resume("d")}")
}

object Step2 {
    class YieldingFunction<T> {
        private var c: Continuation<Unit>? = null
        private var coInput: T? = null
        private var coOutput: T? = null

        suspend fun yield(value: T): T? {
            suspendCoroutineOrReturn { it: Continuation<Unit> ->
                c = it
                coOutput = value
                COROUTINE_SUSPENDED
            }
            return coInput
        }

        fun resume(value: T? = null): T? {
            val notNullContinuation = c ?: return coOutput
            coInput = value
            notNullContinuation.resume(Unit)
            return coOutput
        }

        companion object {
            fun <T> create(block: suspend YieldingFunction<T>.() -> Unit): YieldingFunction<T> {
                val continuation = MyContinuation()
                return YieldingFunction<T>().apply {
                    c = block.createCoroutineUnchecked(this, continuation)
                }
            }
        }
    }

    class MyContinuation: Continuation<Unit> {
        override val context: CoroutineContext = EmptyCoroutineContext
        override fun resume(value: Unit) {}
        override fun resumeWithException(exception: Throwable) = throw exception
    }
}
