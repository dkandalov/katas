package katas.kotlin.coroutines

import kotlin.coroutines.experimental.*
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.createCoroutineUnchecked
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

object Hello2 {
    class MyContinuation: Continuation<Unit> {
        override val context: CoroutineContext = EmptyCoroutineContext
        override fun resume(value: Unit) {}
        override fun resumeWithException(exception: Throwable) = throw exception
    }

    fun createCoroutine(block: suspend () -> Unit): Continuation<Unit> {
        return block.createCoroutineUnchecked(MyContinuation())
    }

    fun launchCoroutine(block: suspend () -> Unit) {
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
        block.createCoroutineUnchecked(continuation)
        return continuation
    }

    private suspend fun yeeld() {
        suspendCoroutineOrReturn { it: Continuation<Int> ->
            c = it
            COROUTINE_SUSPENDED
        }
    }

    fun main() {
        val coroutine = cc {
            println("begin")
            noop()
            println("1")
            val r1 = yeeld()
            println(r1)
            println("end")
        }
        coroutine.resume(Unit)
        coroutine.resume(Unit)
//        coroutine.resume(Unit)
//        coroutine.resume(Unit)

        buildSequence {
            yield(1)
        }

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

fun main(args: Array<String>) {
    Hello2.main()
}
