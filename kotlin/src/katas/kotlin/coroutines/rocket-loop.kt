
import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import kotlin.coroutines.Continuation
import kotlin.coroutines.createCoroutine
import kotlin.coroutines.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.intrinsics.suspendCoroutineUninterceptedOrReturn
import kotlin.coroutines.resume

fun create(block: suspend Unit.() -> Unit) {
    block.createCoroutine(Unit, completion = EmptyContinuation).resume(Unit)
}

fun main(args: Array<String>) {
    var count = 0
    var savedC: Continuation<Unit>? = null
    create {
        println("init")
        suspendCoroutineUninterceptedOrReturn { c: Continuation<Unit> ->
            savedC = c
            COROUTINE_SUSPENDED
        }
        println("foo")
        if (count < 5) {
            println("🚀 $count")
            count += 1
            suspendCoroutineUninterceptedOrReturn { _: Continuation<Unit> ->
                COROUTINE_SUSPENDED
            }
        }
        println("done")
    }
    savedC?.resume(Unit)
    savedC?.resume(Unit)
    savedC?.resume(Unit)
    savedC?.resume(Unit)
}
