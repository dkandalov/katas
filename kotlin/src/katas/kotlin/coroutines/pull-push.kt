@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package katas.kotlin.coroutines

import katas.kotlin.coroutines.PP.CoDataSource.Companion.build
import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import kotlincommon.printed
import org.junit.Test
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Future
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.createCoroutine
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

class PP {
    private interface DataSource {
        // Blocking here means waiting for an "IO thread" to finish reading data
        fun blockingRead(): Int
        // Listener will be called from "IO thread" as soon as it has read the data
        fun asyncRead(listener: (Int) -> Unit)
        fun asyncRead(): Future<Int>
    }

    private class DataSourceList(val data: MutableList<Int>): DataSource {
        override fun blockingRead(): Int {
            return data.removeAt(0)
        }
        override fun asyncRead(listener: (Int) -> Unit) {
            if (data.isNotEmpty()) {
                listener(blockingRead())
            }
        }
        override fun asyncRead(): Future<Int> {
            return CompletableFuture.completedFuture(blockingRead())
        }
    }

    private val source = DataSourceList(listOf(1, 2, 3).toMutableList())

    @Test fun `pull read (sequential)`() {
        // Client thread (which also becomes "IO-thread") waits for each read.
        source.blockingRead().printed()
        source.blockingRead().printed()
        source.blockingRead().printed()
    }

    @Test fun `pull read (sequential with futures)`() {
        // Client thread only sets up future. Another "IO-thread" waits for each read.
        val future = CompletableFuture
            .runAsync { source.blockingRead().printed() }
            .thenRun { source.blockingRead().printed() }
            .thenRun { source.blockingRead().printed() }
        // ...
        future.join()
    }

    @Test fun `push read (sequential)`() {
        source.asyncRead {
            it.printed()
            source.asyncRead {
                it.printed()
                source.asyncRead {
                    it.printed()
                    source.asyncRead { error("this is never called") }
                }
            }
        }
    }

    @Test fun `push read (sequential, as "pull" with coroutines)`() {
        build(source) {
            "start".printed()
            read().printed()
            read().printed()
            read().printed()
            // read().printed() // if uncommented "end" will never be printed
            "end".printed()
        }
    }

    @Test fun `push read summed`() {
        "start".printed()
        source.asyncRead {
            val n1 = it
            source.asyncRead {
                val n2 = it
                source.asyncRead {
                    val n3 = it
                    println("sum : ${n1 + n2 + n3}")
                }
            }
        }
        source.asyncRead { error("this is never called") }
        "end".printed()
    }

    @Test fun `pushed summed read (as "pull" with coroutines)`() {
        build(source) {
            "start".printed()
            println("sum : ${read() + read() + read()}")
            "end".printed()
        }
    }

    private class CoDataSource(private val dataSource: DataSource) {
        suspend fun read(): Int {
            return suspendCoroutineOrReturn { continuation: Continuation<Int> ->
                dataSource.asyncRead { continuation.resume(it) }
                COROUTINE_SUSPENDED
            }
        }

        companion object {
            fun build(dataSource: DataSource, callback: suspend CoDataSource.() -> Unit) {
                val result = CoDataSource(dataSource)
                callback.createCoroutine(result, completion = EmptyContinuation).resume(Unit)
            }
        }
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
                return object: IDisposable {
                    override fun dispose() {}
                }
            }
        }
        observable.subscribe(object: IObserver {
            override fun onNext(n: Int) {
                n.printed()
            }

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
