@file:Suppress("EXPERIMENTAL_FEATURE_WARNING", "PackageDirectoryMismatch")

package katas.kotlin.coroutines.steps.step2

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import kotlin.coroutines.*
import kotlin.coroutines.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.intrinsics.suspendCoroutineUninterceptedOrReturn

/**
 * Send/receive values from coroutine
 */
fun main() {
    val events = ArrayList<String>()
    fun log(it: Any?) = events.add(it.toString())

    val f = YieldingFunction.create<String> {
        log(1)
        log("f=" + yield("b"))
        log(3)
        log("f=" + yield("d"))
        log(5)
    }
    log(0)
    log("main=" + f.resume("a")) // starts "f" (resume argument is ignored)
    log(2)
    log("main=" + f.resume("c")) // continues from "yield 3"
    log(4)
    log("main=" + f.resume("e")) // continues from "yield 5"
    log(6)
    log(f.resume("f"))
    log(f.resume("g"))

    assertThat(events, equalTo(listOf(
        "0",
        "1",
        "main=b",
        "2",
        "f=c",
        "3",
        "main=d",
        "4",
        "f=e",
        "5",
        "main=null",
        "6",
        "null",
        "null"
    )))
}

private suspend fun sf(yf: YieldingFunction<String>) {
    println("f=" + yf.yield("xxx"))
}

private class YieldingFunction<T> {
    private var start: Continuation<Unit>? = null
    private var c: Continuation<T>? = null
    private var yieldedValue: T? = null

    fun resume(n: T): T? {
        if (start != null) {
            start!!.resume(Unit)
            start = null
        }
        else {
            val cont = c
            c = null
            yieldedValue = null
            cont?.resume(n)
        }
        return yieldedValue
    }

    suspend fun yield(n: T): T {
        return suspendCoroutineUninterceptedOrReturn { it: Continuation<T> ->
            yieldedValue = n
            c = it
            COROUTINE_SUSPENDED
        }
    }

    companion object {
        fun <T> create(block: suspend YieldingFunction<T>.() -> Unit): YieldingFunction<T> {
            val f = YieldingFunction<T>()
            f.start = block.createCoroutine(f, completion = EmptyContinuation)
            return f
        }
    }
}

private object EmptyContinuation: Continuation<Unit> {
    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resumeWith(result: Result<Unit>) {}
}
