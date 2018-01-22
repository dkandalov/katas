@file:Suppress("EXPERIMENTAL_FEATURE_WARNING", "PackageDirectoryMismatch")

package katas.kotlin.coroutines.steps.step1

import katas.kotlin.coroutines.steps.step1.YieldingFunction.Companion.create
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.CoroutineContext
import kotlin.coroutines.experimental.EmptyCoroutineContext
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.createCoroutineUnchecked
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun main(args: Array<String>) {
    val f = create {
        println(1)
        yield() // 1
        println(2)
        yield() // 2
        println(3)
    }
    f.resume() // starts "f"
    f.resume() // continues from "yield 1"
    f.resume() // continues from "yield 2"
    f.resume() // continues from "yield 2" (again)
}

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
