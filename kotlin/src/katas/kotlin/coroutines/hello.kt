package katas.kotlin.coroutines

import kotlin.coroutines.experimental.*

object Hello {
    class MyContinuation<Unit>(override val context: CoroutineContext): Continuation<Unit> {
        override fun resume(value: Unit) {}
        override fun resumeWithException(exception: Throwable) = TODO()
    }

    class MyContext: AbstractCoroutineContextElement(MyContext) {
        companion object Key : CoroutineContext.Key<MyContext>
    }

    fun launchCoroutine(context: CoroutineContext, block: suspend () -> Unit) {
        block.startCoroutine(MyContinuation(context))
    }

    fun main() {
        val block: suspend () -> Unit = {
            println("Hello coroutine world (${Thread.currentThread().name})")
        } 
        block.startCoroutine(MyContinuation(EmptyCoroutineContext))
    }
}

fun main(args: Array<String>) {
    Hello.main()
}
