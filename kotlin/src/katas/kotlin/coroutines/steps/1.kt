@file:Suppress("EXPERIMENTAL_FEATURE_WARNING", "PackageDirectoryMismatch")

package katas.kotlin.coroutines.steps.step1

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import kotlin.coroutines.*
import kotlin.coroutines.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.intrinsics.suspendCoroutineUninterceptedOrReturn

/**
 * Yield coroutine execution
 */
fun main(args: Array<String>) {
    val events = ArrayList<String>()
    fun log(it: Any?) = events.add(it.toString())

    val f = ResumableFunction.create {
        log(1)
        yield()
        log(3)
        yield()
        log(5)
    }
    log(0)
    f.resume()
    log(2)
    f.resume()
    log(4)
    f.resume()
    log(6)

    assertThat(events, equalTo(listOf("0", "1", "2", "3", "4", "5", "6")))
}

private class ResumableFunction {
    private var c: Continuation<Unit>? = null

    fun resume() {
        c?.resume(Unit)
    }

    suspend fun yield() {
        return suspendCoroutineUninterceptedOrReturn { continuation: Continuation<Unit> ->
            c = continuation
            COROUTINE_SUSPENDED
        }
    }

    companion object {
        fun create(block: suspend ResumableFunction.() -> Unit): ResumableFunction {
            val f = ResumableFunction()
            f.c = block.createCoroutine(f, completion = EmptyContinuation)
            return f
        }
    }
}

object EmptyContinuation: Continuation<Unit> {
    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resumeWith(result: Result<Unit>) {}
}
