package ru.akka_tutorial.pi

import akka.actor.{Actors, Actor}

/**
 * User: dima
 * Date: 07/04/2012
 */
object Pi_ {
  def main(args: Array[String]) {
    val a = Actor.actorOf[Worker].start()
    a ! "aaa"
  }
}

class Worker extends Actor {
  protected def receive = {
    case msg@_ => println(msg)
  }
}

