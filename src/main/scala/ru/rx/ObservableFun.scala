package ru.rx

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.Matchers
import org.junit.Test
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.JavaConversions._
import scala.util.{Failure, Success}

// TODO these tests are not deterministic (although might work most of the time)
// TODO these tests are ugly
class ObservableFun extends Matchers {
	private val events = new CopyOnWriteArrayList[String]()
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

}