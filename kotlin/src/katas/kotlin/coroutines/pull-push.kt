@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package katas.kotlin.coroutines

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.coroutines.PP.CoDataSource.Companion.build
import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import kotlincommon.printed
import org.junit.Test
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors.newCachedThreadPool
import java.util.concurrent.Future
import java.util.function.Supplier
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.createCoroutine
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

class PP {

    @Test fun `pull read (sequential)`() {
        // Current thread (which becomes "IO thread") waits for each read.
        source.blockingRead().printed()
        source.blockingRead().printed()
        source.blockingRead().printed()
        expectOutput("1", "2", "3")
    }

    @Test fun `pull read and sum (sequential)`() {
        val n1 = source.blockingRead()
        val n2 = source.blockingRead()
        val n3 = source.blockingRead()
        (n1 + n2 + n3).printed()

        expectOutput("6")
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
        source.waitToBeEmpty()

        expectOutput("1", "2", "3")
    }

    @Test fun `push read and sum (sequential)`() {
        "start".printed()
        source.asyncRead {
            val n1 = it
            source.asyncRead {
                val n2 = it
                source.asyncRead {
                    val n3 = it
                    (n1 + n2 + n3).printed()
                }
            }
        }
        source.waitToBeEmpty()
        "end".printed()

        expectOutput("start", "6", "end")
    }

    @Test fun `push read and sum (parallel)`() {
        "start".printed()
        val values = CopyOnWriteArrayList<Int>()
        val latch = CountDownLatch(3)
        source.asyncRead { values.add(it); latch.countDown() }
        source.asyncRead { values.add(it); latch.countDown() }
        source.asyncRead { values.add(it); latch.countDown() }
        latch.await()
        values.sum().printed()
        "end".printed()

        expectOutput("start", "6", "end")
    }

    @Test fun `pull future read (sequential)`() {
        // Current thread only wires up execution. Another "IO-thread" waits for each read.
        val future = runAsyncFuture { source.blockingRead().printed() }
            .thenRun { source.blockingRead().printed() }
            .thenRun { source.blockingRead().printed() }
        future.join()

        expectOutput("1", "2", "3")
    }

    @Test fun `pull future read and sum (sequential)`() {
        // Current thread only wires up execution. Another "IO-thread" waits for each read.
        val future = supplyAsyncFuture { source.blockingRead() }
            .thenCompose { supplyAsyncFuture { source.blockingRead() + it } }
            .thenCompose { supplyAsyncFuture { source.blockingRead() + it } }
            .thenApply { it.printed() }
        future.join()

        expectOutput("6")
    }

    @Test fun `pull future read and sum (parallel)`() {
        val future = supplyAsyncFuture { source.blockingRead() }
            .thenCombineAsync(supplyAsyncFuture { source.blockingRead() }, { sum, n -> sum + n })
            .thenCombineAsync(supplyAsyncFuture { source.blockingRead() }, { sum, n -> sum + n })
            .thenApply { it.printed() }
        future.join()

        expectOutput("6")
    }

    @Test fun `push read (as "pull" with coroutines, sequential)`() {
        build(source) {
            read().printed()
            read().printed()
            read().printed()
            // read().printed() // if uncommented "end" will never be printed
        }
        source.waitToBeEmpty()

        expectOutput("1", "2", "3")
    }

    @Test fun `push read and sum (as "pull" with coroutines, sequential)`() {
        build(source) {
            val n1 = read()
            val n2 = read()
            val n3 = read()
            (n1 + n2 + n3).printed()
        }
        source.waitToBeEmpty()

        expectOutput("6")
    }

    @Test fun `push read and sum (as "pull" with coroutines, parallel)`() {
        build(source) {
            val values = parallelRead(count = 3)
            values.sum().printed()
        }
        source.waitToBeEmpty()

        expectOutput("6")
    }


    private interface DataSource {
        // "blocking" here means request data and wait for it to arrive
        fun blockingRead(): Int
        // "async" here means request data and call the listener as soon as the data has arrived
        fun asyncRead(listener: (Int) -> Unit)
        fun asyncRead(): Future<Int>
    }

    companion object {
        private val executor = newCachedThreadPool { runnable -> Thread(runnable).apply { name = "IO" } }
    }

    private class DataSourceList(data: List<Int>): DataSource {
        private val data = ArrayList(data)

        override fun blockingRead(): Int {
            return synchronized(data) {
                data.removeAt(0)
            }
        }

        override fun asyncRead(listener: (Int) -> Unit) {
            executor.submit {
                val value = synchronized(data) {
                    if (data.isNotEmpty()) blockingRead() else null
                }
                if (value != null) listener(value)
            }
        }

        override fun asyncRead(): Future<Int> {
            return CompletableFuture.supplyAsync(Supplier { blockingRead() }, executor)
        }

        fun waitToBeEmpty() {
            while (synchronized(data) { data.isNotEmpty() }) {
                Thread.sleep(20)
            }
        }
    }

    private val source = DataSourceList(listOf(1, 2, 3))


    private fun runAsyncFuture(f: () -> Unit): CompletableFuture<Void> =
        CompletableFuture.runAsync(Runnable { f() }, executor)

    private fun <T> supplyAsyncFuture(f: () -> T): CompletableFuture<T> =
        CompletableFuture.supplyAsync(Supplier { f() }, executor)


    private class CoDataSource(private val dataSource: DataSource) {
        suspend fun read(): Int {
            return suspendCoroutineOrReturn { continuation: Continuation<Int> ->
                dataSource.asyncRead { continuation.resume(it) }
                COROUTINE_SUSPENDED
            }
        }

        suspend fun parallelRead(count: Int): List<Int> {
            return suspendCoroutineOrReturn { continuation: Continuation<List<Int>> ->
                val values = CopyOnWriteArrayList<Int>()
                0.until(count).forEach {
                    dataSource.asyncRead {
                        values.add(it)
                        if (values.size == count) {
                            continuation.resume(values)
                        }
                    }
                }
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

    private val printedOutput = CopyOnWriteArrayList<String>()

    private fun <T : Any?> T.printed(): T {
        println(this.toString())
        printedOutput.add(this.toString())
        return this
    }

    private fun expectOutput(vararg expected: String) {
        assertThat(printedOutput, equalTo(expected.toList()))
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
