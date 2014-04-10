package ru.rx

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.Matchers
import org.junit.Test
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.JavaConversions._
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import rx.lang.scala.Observable

// TODO these tests are not deterministic (although might work most of the time)
// TODO these tests are ugly
class ObservableFun extends Matchers {
	private val events = new CopyOnWriteArrayList[Any]()
	private val someException = new IllegalStateException("no future, no past")

	@Test def successfulFuture() {
		val f: Future[String] = future { "future!" }
		f.onSuccess { case it => events.add(it) }

		events.toList should equal(Seq("future!"))
	}

	@Test def failedFuture() {
		val f = future { throw someException }
		f.onFailure { case e => events.add(e.toString) }

		Thread.sleep(10)
		events.toList should equal(Seq(someException.toString))
	}

	@Test def completedFuture() {
		val f = future{ "future!" }
		f.onComplete {
				case Success(it) => events.add(it)
				case Failure(e) => events.add(e.toString)
		}

		Thread.sleep(10)
		events.toList should equal(Seq("future!"))
	}

	@Test def failedProjectionFails() {
		val f = future{ "future!" }
		val failedFuture = f.failed
		failedFuture.onFailure {
			case e => events.add(e.toString)
		}

		Thread.sleep(10)
		events.toList should equal(Seq("java.util.NoSuchElementException: Future.failed not completed with a throwable."))
	}
	
	@Test def failedProjectionSucceeds() {
		val f = future{ throw someException }
		val failedFuture = f.failed
		failedFuture.onSuccess {
			case throwable => events.add(throwable.toString)
		}

		Thread.sleep(10)
		events.toList should equal(Seq(someException.toString))
	}

	@Test def mapFutureSuccess() {
		val f = future{ "future!" }.map(_ + " mapped")
		f.onSuccess { case it => events.add(it) }

		Thread.sleep(10)
		events.toList should equal(Seq("future! mapped"))
	}

	@Test def mapFutureFailure() {
		val f = future[String]{ throw someException }.map(_ + " mapped")
		f.onFailure { case e => events.add(e.toString) }

		Thread.sleep(10)
		events.toList should equal(Seq(someException.toString))
	}

	@Test(expected = classOf[IllegalStateException])
	def failingMapFuture() {
		val f = future[String]{ "future!" }.map{ throw someException }
		f.onFailure { case e => events.add(e.toString) }

		Thread.sleep(10)
	}

	@Test def flatMapFutureSuccess() {
		val f = future{ "future!" }.flatMap{it => future { it + " flat mapped" }}
		f.onSuccess { case it => events.add(it) }

		Thread.sleep(10)
		events.toList should equal(Seq("future! flat mapped"))
	}

	@Test def flatMapFutureFailure() {
		val f = future[String]{ throw someException }.flatMap{it => future { it + " flat mapped" }}
		f.onFailure { case e => events.add(e.toString) }

		Thread.sleep(10)
		events.toList should equal(Seq(someException.toString))
	}

	@Test def failingFlatMapFuture() {
		val f = future[String]{ "future!" }.flatMap{it => future { throw someException }}
		f.onFailure { case e => events.add(e.toString) }

		Thread.sleep(10)
		events.toList should equal(Seq(someException.toString))
	}

	@Test def awaitingFuture() {
		val f = future { "future!" }
		f.onSuccess { case it => events.add(it) }

		Await.result(f, 1 second) should equal("future!")
	}

	@Test def successfulPromise() {
		val promise = Promise[String]()
		promise.future.onSuccess{ case it => events add it }

		promise.complete(Success("future!"))

		Thread.sleep(10)
		events.toList should equal(Seq("future!"))
	}

	@Test def failedPromise() {
		val promise = Promise[String]()
		promise.future.onFailure{ case e => events add e.toString }

		promise.complete(Failure(someException))

		Thread.sleep(10)
		events.toList should equal(Seq(someException.toString))
	}

	@Test def tryingToCompletePromise() {
		val promise = Promise[String]()
		promise.tryComplete(Success("yay!")) should equal(true)
		promise.tryComplete(Success("yay!")) should equal(false)
	}

	@Test def completePromise_With_SuccessfulFuture() {
		val promise = Promise[String]()
		promise.future.onSuccess{ case it => events add it }

		promise.completeWith(future { "future!" })

		Thread.sleep(10)
		events.toList should equal(Seq("future!"))
	}

	@Test def completePromise_With_FailedFuture() {
		val promise = Promise[String]()
		promise.future.onFailure{ case e => events add e.toString }

		promise.completeWith(future { throw someException })

		Thread.sleep(10)
		events.toList should equal(Seq(someException.toString))
	}

	@Test def valuesFromObservable() {
		val observable = Observable.items(1, 2)
		observable.subscribe { it => events add it.toString }

		Thread.sleep(10)
		events.toList should equal(Seq("1", "2"))
	}

	@Test def mapObservable() {
		val observable = Observable.items(1, 2)
		observable.map{_ + 1}.subscribe { it => events add it.toString }

		Thread.sleep(10)
		events.toList should equal(Seq("2", "3"))
	}

	@Test def flatMapObservable() {
		def observeTwice[T](value: T): Observable[T] = Observable.items(value, value)

		val observable = Observable.items(1, 2)
		observable.flatMap(observeTwice).subscribe { it => events add it.toString }

		Thread.sleep(10)
		events.toList should equal(Seq("1", "1", "2", "2"))
	}

	@Test def scanObservable() {
		val observable = Observable.items(1, 2)
		observable.scan{(acc: Int, it: Int) => acc + it}.subscribe { it => events add it.toString }

		Thread.sleep(10)
		events.toList should equal(Seq("1", "3"))
	}

	@Test def groupObservable() {
		val observable = Observable.items(1, 2, 3, 4, 5)
		observable.groupBy(_ % 2 == 0).subscribe{ entry =>
			val prefix = if (entry._1) "even" else "odd"
			val subObservable = entry._2
			events add "observing " + prefix
			subObservable.subscribe{ events add prefix + _.toString }
		}

		Thread.sleep(10)
		events.toList should equal(Seq("observing odd", "odd1", "observing even", "even2", "odd3", "even4", "odd5"))
	}

	@Test def groupUtilObservable() {
		def isEven(n: Int) = n % 2 == 0
		def closeOnSecondEvent(even: Boolean, observable: Observable[Int]) = observable.buffer(2)

		val observable = Observable.items(1, 2, 3, 4, 5)
		observable.groupByUntil(isEven, closeOnSecondEvent).subscribe{ entry =>
			val prefix = if (entry._1) "even" else "odd"
			val subObservable = entry._2
			events add "observing " + prefix
			subObservable.subscribe{ events add prefix + _.toString }
		}

		Thread.sleep(10)
		events.toList should equal(Seq(
			"observing odd", "odd1",
			"observing even", "even2",
			"odd3", // second odd event
			"even4",
			"observing odd", // new odd observer
			"odd5"))
	}

	@Test def pivotObservable() {
		// TODO has different name in scala API?
	}

	@Test def bufferObservable() {
		val observable = Observable.items(1, 2, 3, 4, 5)
		observable.buffer(count = 2).subscribe{ it => events add it }

		Thread.sleep(10)
		events.toList should equal(Seq(Seq(1, 2), Seq(3, 4), Seq(5)))
	}

	@Test def bufferSkippingObservable() {
		val observable = Observable.items(1, 2, 3, 4, 5)
		observable.buffer(count = 2, skip = 3).subscribe{ it => events add it }

		Thread.sleep(10)
		events.toList should equal(Seq(Seq(1, 2), Seq(4, 5)))
	}

	@Test def bufferObservable_WithManualClosing() {
		val observable = Observable.items(1, 2, 3, 4, 5)
		def closeOnThree(): Observable[Int] = observable.filter(_ == 3)
//		observable.buffer[Int](closeOnThree).subscribe{ it => events add it } // TODO cannot be applied, wtf?
//
//		Thread.sleep(10)
//		events.toList should equal(Seq(Seq(1, 2), Seq(4, 5)))
	}
}