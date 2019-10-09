package katas.kotlin.coroutines

import kotlin.coroutines.*
import kotlin.coroutines.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.intrinsics.suspendCoroutineUninterceptedOrReturn

object Hello {

    fun launchCoroutine(context: CoroutineContext, coroutineScope: suspend () -> Unit) {
        coroutineScope.startCoroutine(MyContinuation(context))
    }

    fun main() {
        var savedContinuation: Continuation<Unit>? = null

        val coroutineScope: suspend () -> Unit = {
            println("Start of scope")
            suspendCoroutineUninterceptedOrReturn { continuation: Continuation<Unit> ->
                savedContinuation = continuation
                println("Suspended")
                COROUTINE_SUSPENDED
            }
            println("End of scope")
        }
        println("Start")
//        coroutineScope.startCoroutineUninterceptedOrReturn(MyContinuation(EmptyCoroutineContext))
        coroutineScope.startCoroutine(MyContinuation(MyEmptyCoroutineContext))

        println("About to resume")
        savedContinuation!!.resume(Unit)
//        savedContinuation!!.context.printed()
        println("About to resume again")
        savedContinuation!!.resume(Unit)
        println("About to resume again 2")
        savedContinuation!!.resume(Unit)

        println("End")
    }
}

class MyContinuation<Unit>(override val context: CoroutineContext): Continuation<Unit> {
    override fun resumeWith(result: Result<Unit>) {}
}

object MyEmptyCoroutineContext : CoroutineContext {
    @Suppress("UNCHECKED_CAST")
    override fun <E : CoroutineContext.Element> get(key: CoroutineContext.Key<E>): E? =
        if (key === ContinuationInterceptor.Key) (MyInterceptor as E?) else null
    override fun <R> fold(initial: R, operation: (R, CoroutineContext.Element) -> R): R = initial
    override fun plus(context: CoroutineContext): CoroutineContext = context
    override fun minusKey(key: CoroutineContext.Key<*>): CoroutineContext = this
    override fun hashCode(): Int = 0
    override fun toString(): String = "EmptyCoroutineContext"
}

object MyInterceptor : ContinuationInterceptor, AbstractCoroutineContextElement(Key) {
    object Key : CoroutineContext.Key<MyInterceptor>
    override fun <T> interceptContinuation(continuation: Continuation<T>) = continuation
}

fun main() {
    Hello.main()
}
