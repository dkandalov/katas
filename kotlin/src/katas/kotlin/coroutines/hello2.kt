package katas.kotlin.coroutines

import java.lang.Thread.currentThread
import kotlin.coroutines.AbstractCoroutineContextElement
import kotlin.coroutines.Continuation
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.startCoroutine
import kotlin.coroutines.suspendCoroutine

object Hello2 {
    class MyContinuation<Unit>(override val context: CoroutineContext): Continuation<Unit> {
        override fun resume(value: Unit) {}
        override fun resumeWithException(exception: Throwable) = TODO()
    }

    class MyContext: AbstractCoroutineContextElement(Key) {
        companion object Key : CoroutineContext.Key<MyContext>
    }

    fun launchCoroutine(context: CoroutineContext, block: suspend () -> Unit) {
        block.startCoroutine(MyContinuation(context))
    }

    suspend fun suspend() = println("noop suspend")

    suspend fun delay(timeMillis: Long): Unit = suspendCoroutine { continuation ->
        Thread({
            Thread.sleep(timeMillis)
            continuation.resume(Unit)
        }).start()
    }

    fun main() {
        launchCoroutine(MyContext()) {
            println("Hello coroutine world (${currentThread().name})")
            suspend()
            println("finished suspend")
            delay(200)
            println("finished delay 1 (${currentThread().name})")
            delay(200)
            println("finished delay 2 (${currentThread().name})")
        }
        // suspend() <-- compilation error
        println("finished main()")
    }
}

fun main(args: Array<String>) {
    Hello2.main()
}
