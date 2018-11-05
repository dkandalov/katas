
import katas.kotlin.coroutines.MyContinuation
import katas.kotlin.coroutines.MyEmptyCoroutineContext
import kotlin.coroutines.Continuation
import kotlin.coroutines.createCoroutine
import kotlin.coroutines.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.intrinsics.suspendCoroutineUninterceptedOrReturn
import kotlin.coroutines.resume

fun createCoroutine(block: suspend Unit.() -> Unit) {
    block.createCoroutine(Unit, completion = MyContinuation(MyEmptyCoroutineContext)).resume(Unit)
}

fun main(args: Array<String>) {
    var count = 0
    var savedContinuation: Continuation<Int>? = null
    createCoroutine {
        println("init")
        println(100 + suspendCoroutineUninterceptedOrReturn { continuation: Continuation<Int> ->
            savedContinuation = continuation
            continuation.resume(100)
            println("🐶")
        })
        if (count < 5) {
            println("🚀 $count")
            count += 1
            savedContinuation?.resume(count) // recursive
        }
        println("done")
    }
    // savedContinuation?.resume(count)
}
