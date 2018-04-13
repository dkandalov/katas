
import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.createCoroutine
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun create(block: suspend Unit.() -> Unit) {
    block.createCoroutine(Unit, completion = EmptyContinuation).resume(Unit)
}

fun main(args: Array<String>) {
    var count = 0
    var savedC: Continuation<Unit>? = null
    create {
        println("init")
        suspendCoroutineOrReturn { c: Continuation<Unit> ->
            savedC = c
            null
        }
        println("foo")
        if (count < 5) {
            println("🚀 $count")
            count += 1
            savedC?.resume(Unit)
            // need to return here, otherwise the rest of the function will run after continuation
            return@create
        }
        println("done")
    }
}
