package ru.akka_tutorial.pi

import akka.actor._
import akka.routing.RoundRobinRouter
import java.util.concurrent.{TimeUnit, SynchronousQueue}

/**
 * User: dima
 * Date: 07/04/2012
 */
object Pi_ {
  def main(args: Array[String]) {
    val resultQueue = new SynchronousQueue[Double]()
    val system = ActorSystem("PiSystem")

    val master = system.actorOf(Props(new Master(4, 10000, 10000, resultQueue)))
    master ! CalculatePi
    println(resultQueue.take())

    system.shutdown()
  }
}

sealed trait PiMessage
case object CalculatePi extends PiMessage
case class Task(from: Int, to: Int) extends PiMessage
case class TaskResult(value: Double) extends PiMessage

class Master(nOfWorkers: Int, calcCount: Int, batchSize: Int, resultQueue: SynchronousQueue[Double]) extends Actor {
  var pi: Double = 0
  var counter: Int = 0
  var startTime: Long = 0

  override protected def receive = {
    case CalculatePi =>
      val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(nOfWorkers)))
      0.until(calcCount).foreach{ i => workerRouter ! Task(i * batchSize, (i + 1) * batchSize) }

    case TaskResult(value) =>
      pi += value
      counter += 1
      if (counter == calcCount) context.stop(self)

    case msg@_ => println("Don't know how to respond to message: " + msg)
  }

  override def preStart() {
    startTime = System.nanoTime()
  }

  override def postStop() {
    println(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startTime))
    resultQueue.put(pi)
  }
}

class Worker extends Actor {
  override protected def receive = {
    case Task(from, to) => sender ! new TaskResult(calcPi(from, to))
  }

  def calcPi(start: Int, end: Int): Double = {
    4.0 * start.until(end).foldLeft(0d){ (result, n) => result + math.pow(-1, n) / (2 * n + 1) }
  }
}

