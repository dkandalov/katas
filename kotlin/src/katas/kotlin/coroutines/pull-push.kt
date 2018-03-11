@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package katas.kotlin.coroutines

import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import kotlincommon.printed
import org.junit.Test
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.createCoroutine
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

class PP {
    @Test fun `aaa`() {
        val listener = object : Listener {
            override fun onRead(n: Int) {
                TODO("not implemented")
            }
        }
        build(listener) {
            "start".printed()
            read().printed()
            read().printed()
            "end".printed()
        }

        listener.onRead(1)
        listener.onRead(2)
        listener.onRead(3)
    }

    private var start: Continuation<Unit>? = null
    private var c: Continuation<Int>? = null

    fun build(listener: Listener, callback: suspend Listener.() -> Unit) {
        start = callback.createCoroutine(listener, EmptyContinuation)
    }

    suspend fun read(): Int {
        return suspendCoroutineOrReturn { it: Continuation<Int> ->

            COROUTINE_SUSPENDED
        }
    }

    interface Listener {
        fun onRead(n: Int)
    }
}

class PullPushPlayground {
    @Test fun `pulled reader`() {
        val reader = Reader()
        val writer = Writer()
        while (reader.hasData()) {
            writer.write(reader.read())
        }
    }

    @Test fun `pushed writer`() {
        val writer = object: Writer() {
            override fun write(n: Int) {
            }
        }
        writer.write(1)
        writer.write(2)
        writer.write(3)
    }

    private class Reader {
        fun read(): Int {
            return 1
        }

        fun hasData(): Boolean {
            TODO("not implemented")
        }
    }

    private open class Writer {
        open fun write(n: Int) {}
    }
}

// See http://csl.stanford.edu/~christos/pldi2010.fit/meijer.duality.pdf
// http://delivery.acm.org/10.1145/2170000/2169076/p20-meijer.pdf
class DualityPlayground {

    @Test fun `enumarable print`() {
        val enumerator: IEnumerator = object: IEnumerator {
            override var current = 0
            override fun moveNext(): Boolean {
                return if (current < 5) {
                    current += 1
                    true
                } else {
                    false
                }
            }

            override fun dispose() {}
        }
        val enumerable = object: IEnumerable {
            override val enumerator: IEnumerator = enumerator
        }

        enumerable.enumerator.let {
            while (it.moveNext()) {
                it.current.printed()
            }
            it.dispose()
        }
    }

    @Test fun `observable print`() {
        val observable = object: IObservable {
            override fun subscribe(observer: IObserver): IDisposable {
                1.rangeTo(5).forEach { i ->
                    observer.onNext(i)
                }
                observer.onCompleted()
                return object : IDisposable {
                    override fun dispose() {}
                }
            }
        }
        observable.subscribe(object: IObserver {
            override fun onNext(n: Int) { n.printed() }
            override fun onCompleted() {}
            override fun onError(e: Exception) {}
        })
    }


    private interface IEnumerable {
        val enumerator: IEnumerator
    }

    private interface IEnumerator: IDisposable {
        fun moveNext(): Boolean
        val current: Int
    }

    private interface IObservable {
        fun subscribe(observer: IObserver): IDisposable
    }

    private interface IObserver {
        fun onCompleted()
        fun onError(e: Exception)
        fun onNext(n: Int)
    }

    private interface IDisposable {
        fun dispose()
    }
}
