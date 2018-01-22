@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package katas.kotlin.coroutines.steps

import katas.kotlin.coroutines.steps.Step1.YieldingFunction.Companion.create
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.CoroutineContext
import kotlin.coroutines.experimental.EmptyCoroutineContext
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.createCoroutineUnchecked
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun main(args: Array<String>) {
    val f = create {
        println(1)
        yield()
        println(2)
        yield()
        println(3)
    }
    f.resume()
    f.resume()
    f.resume()
    f.resume() // continues from yield() just before "println(3)"
}

object Step1 {

    class YieldingFunction {
        private var c: Continuation<Unit>? = null

        fun resume() {
            c?.resume(Unit)
        }

        suspend fun yield() {
            suspendCoroutineOrReturn { it: Continuation<Unit> ->
                c = it
                COROUTINE_SUSPENDED
            }
        }

        companion object {
            fun create(block: suspend YieldingFunction.() -> Unit): YieldingFunction {
                val continuation = MyContinuation()
                return YieldingFunction().apply {
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
