@file:Suppress("EXPERIMENTAL_FEATURE_WARNING", "PackageDirectoryMismatch")

package katas.kotlin.coroutines.steps.step3

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.CoroutineContext
import kotlin.coroutines.experimental.EmptyCoroutineContext
import kotlin.coroutines.experimental.createCoroutine
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

val events = ArrayList<String>()
fun log(it: Any?) = events.add(it.toString())

fun main(args: Array<String>) {
    val f = ResumableFunction.create {
        log(1)
        foo(this)
        log(3)
    }
    log(0)
    f.resume()
    log(2)
    f.resume()
    log(4)

    assertThat(events, equalTo(listOf(
        "0",
        "1",
        "started foo",
        "stacktrace:\n" +
            "katas.kotlin.coroutines.steps.step3._3Kt.foo(3.kt)\n" +
            "katas.kotlin.coroutines.steps.step3._3Kt\$main\$f\$1.doResume(3.kt)\n" +
            "kotlin.coroutines.experimental.jvm.internal.CoroutineImpl.resume(CoroutineImpl.kt)\n" +
            "kotlin.coroutines.experimental.SafeContinuation.resume(SafeContinuationJvm.kt)\n" +
            "katas.kotlin.coroutines.steps.step3.ResumableFunction.resume(3.kt)\n" +
            "katas.kotlin.coroutines.steps.step3._3Kt.main(3.kt)",
        "2",
        "finished foo",
        "3",
        "4"
    )))
}

private suspend fun foo(f: ResumableFunction) {
    log("started foo")
    log("stacktrace:\n" + Exception().stackTrace.map{ it.toString().replace(Regex(":\\d+"), "")}.joinToString("\n"))
    f.yield()
    log("finished foo")
}

private class ResumableFunction {
    private var c: Continuation<Unit>? = null

    fun resume() {
        val cont = c
        c = null
        cont?.resume(Unit)
    }

    suspend fun yield() {
        return suspendCoroutineOrReturn { it: Continuation<Unit> ->
            c = it
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

private object EmptyContinuation: Continuation<Unit> {
    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resume(value: Unit) {}
    override fun resumeWithException(exception: Throwable) = throw exception
}
