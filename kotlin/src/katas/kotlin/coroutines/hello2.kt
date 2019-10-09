package katas.kotlin.coroutines

import kotlin.coroutines.*
import kotlin.coroutines.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.intrinsics.createCoroutineUnintercepted
import kotlin.coroutines.intrinsics.suspendCoroutineUninterceptedOrReturn

object Hello2 {
    class MyContinuation: Continuation<Unit> {
        override val context: CoroutineContext = EmptyCoroutineContext
        override fun resumeWith(result: Result<Unit>) {}
    }

    fun createCoroutine(block: suspend () -> Unit): Continuation<Unit> {
        return block.createCoroutineUnintercepted(MyContinuation())
    }

    fun startCoroutine(block: suspend () -> Unit) {
        block.startCoroutine(MyContinuation())
    }

    suspend fun noop() {
        println("noop suspend")
    }

    suspend fun delay(timeMillis: Long): Unit = suspendCoroutine { continuation ->
        Thread({
            Thread.sleep(timeMillis)
            continuation.resume(Unit)
        }).start()
    }

    var c: Continuation<Int>? = null

    fun cc(block: suspend () -> Unit): Continuation<Unit> {
        val continuation = MyContinuation()
        block.createCoroutineUnintercepted(continuation)
        return continuation
    }

    private suspend fun yeeld() {
        suspendCoroutineUninterceptedOrReturn { it: Continuation<Int> ->
            c = it
            COROUTINE_SUSPENDED
        }
    }

    fun main() {
        startCoroutine {
            println("begin")
            noop()
//            val r1 = yeeld()
//            println(r1)
            println("end")
        }
        println("---")

//        sequence {
//            yield(1)
//        }

//        launchCoroutine {
//            println("Hello coroutine world (${currentThread().name})")
//            noop()
//            println("finished suspend")
//            delay(200)
//            println("finished delay 1 (${currentThread().name})")
//            delay(200)
//            println("finished delay 2 (${currentThread().name})")
//        }
//        // suspend() <-- compilation error
//        println("finished main()")
    }
}

fun main() {
    Hello2.main()
}
