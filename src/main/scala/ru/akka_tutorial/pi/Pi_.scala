package ru.akka_tutorial.pi

import akka.routing.CyclicIterator
import java.util.concurrent.{TimeUnit, SynchronousQueue}
import akka.actor.{PoisonPill, Actor}
import akka.actor.Actor._

/**
 * User: dima
 * Date: 07/04/2012
 */
object Pi_ {
  def main(args: Array[String]) {
    val resultQueue = new SynchronousQueue[Double]()
    val master = actorOf(new Master(4, 10000, 10000, resultQueue)).start()
    master ! CalculatePi
    println(resultQueue.take())
  }
}

sealed trait PiMessage
case object CalculatePi extends PiMessage
case class Task(from: Int, to: Int) extends PiMessage
case class TaskResult(value: Double) extends PiMessage

class Master(nOfWorkers: Int, calcCount: Int, batchSize: Int, resultQueue: SynchronousQueue[Double]) extends Actor {
  var almostPi: Double = 0
  var counter: Int = 0
  var startTime: Long = 0

  override protected def receive = {
    case CalculatePi =>
      val workers = Vector.fill(nOfWorkers) {actorOf[Worker].start()}
      val cyclicIterator = CyclicIterator(workers)
      0.until(calcCount).foreach{ i => cyclicIterator.next() ! Task(i * batchSize, (i + 1) * batchSize) }
      workers.foreach{_ ! PoisonPill}

    case TaskResult(value) =>
      almostPi += value
      counter += 1
      if (counter == calcCount)  self.stop()

    case msg@_ => println("Don't know how to respond to message: " + msg)
  }

  override def preStart() {
    startTime = System.nanoTime()
  }

  override def postStop() {
    println(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startTime))
    resultQueue.put(almostPi * 4)
  }
}

class Worker extends Actor {
  override protected def receive = {
    case Task(from, to) => self.reply(new TaskResult(calcPi(from, to)))
  }

  def calcPi(start: Int, end: Int): Double = {
    start.until(end).foldLeft(0d){ (result, n) => result + math.pow(-1, n) / (2 * n + 1) }
  }
}

