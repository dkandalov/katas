package katas.kotlin.coroutines

import java.lang.Thread.currentThread
import kotlin.coroutines.*
import kotlin.coroutines.resume

object Hello3 {
    class MyContinuation<in T>(override val context: MyContext): Continuation<T> {
        override fun resumeWith(result: Result<T>) {
            println("resumed")
        }
    }

    class MyContext: AbstractCoroutineContextElement(ContinuationInterceptor), ContinuationInterceptor {
        override fun <T> interceptContinuation(continuation: Continuation<T>): Continuation<T> {
            println("interceptContinuation ${Integer.toHexString(continuation.hashCode())}")
            return continuation
        }
    }

    fun launchCoroutine(continuation: MyContinuation<Unit>, block: suspend () -> Unit) {
        block.startCoroutine(continuation)
    }

    suspend fun delay(timeMillis: Long): Unit = suspendCoroutine { continuation: Continuation<Unit> ->
        Thread({
           Thread.sleep(timeMillis)
           println(continuation)
           continuation.resume(Unit)
        }).start()
    }

    fun main() {
        val continuation = MyContinuation<Unit>(MyContext())
        println(continuation)
        launchCoroutine(continuation) {
            println("Hello coroutine world (${currentThread().name})")
            delay(200)
            println("finished delay 1 (${currentThread().name})")
            delay(200)
            println("finished delay 2 (${currentThread().name})")
        }
        println("finished main()")
    }
}

fun main() {
    Hello3.main()
}
