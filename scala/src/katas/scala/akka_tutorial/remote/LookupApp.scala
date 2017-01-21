package katas.scala.akka_tutorial.remote


import akka.kernel.Bootable
import scala.util.Random
import com.typesafe.config.ConfigFactory
import akka.actor.{ActorRef, Props, Actor, ActorSystem}

object LookupApp {
  def main(args: Array[String]) {
    val app = new LookupApplication
    println("Started Lookup Application")
    while (true) {
      if (Random.nextInt(100) % 2 == 0) app.doSomething(Add(Random.nextInt(100), Random.nextInt(100)))
      else app.doSomething(Subtract(Random.nextInt(100), Random.nextInt(100)))

      Thread.sleep(200)
    }
  }
}

class LookupApplication extends Bootable {
  val system = ActorSystem("LookupApp", ConfigFactory.load(CalcApp.config).getConfig("remoteLookup").withFallback(CalcApp.config))
  val actor = system.actorOf(Props[LookupActor], "lookupActor")
  val remoteActor = system.actorFor("akka://CalcApp@127.0.0.1:2552/user/simpleCalc")

  def doSomething(op: MathOp) {
    println(remoteActor)
    actor !(remoteActor, op)
  }

  def startup() {
  }

  def shutdown() {
    system.shutdown()
  }
}

class LookupActor extends Actor {
  def receive = {
    case (actor: ActorRef, op: MathOp) => actor ! op
    case result: MathResult => result match {
      case AddResult(n1, n2, r) => println("Add result: %d + %d = %d".format(n1, n2, r))
      case SubtractResult(n1, n2, r) => println("Sub result: %d - %d = %d".format(n1, n2, r))
    }
  }
}
