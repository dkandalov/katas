package katas.scala.akka_tutorial.remote

import com.typesafe.config.ConfigFactory
import akka.actor.{Actor, Props, ActorSystem}
import akka.kernel.Bootable

/**
 * User: dima
 * Date: 08/04/2012
 */

object CalcApp {
  val config = ConfigFactory.parseString("""
akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    netty {
      hostname = "127.0.0.1"
    }
  }
}

calculator {
  akka { remote.netty.port = 2552 }
}
remoteLookup {
  akka { remote.netty.port = 2553 }
}
remoteCreate {
  akka {
    remote.netty.port = 2554
    actor {
      deployment {
        /advancedCalc {
          remote = "akka://CalcApp@127.0.0.1:2552"
        }
      }
    }
  }
}
""")

  def main(args: Array[String]) {
    val app = new CalculatorApplication
    println("Started Calculator Application - waiting for messages")
    app.actor ! Add(12, 45)
    println(app.actor)
  }
}

class CalculatorApplication extends Bootable {
  val system = ActorSystem("CalcApp", ConfigFactory.load(CalcApp.config).getConfig("calculator").withFallback(CalcApp.config))
  val actor = system.actorOf(Props[SimpleCalculatorActor], "simpleCalc")

  def startup() {
  }

  def shutdown() {
    system.shutdown()
  }
}

class SimpleCalculatorActor extends Actor {
  def receive = {
    case Add(n1, n2) =>
      println("Calculating " + n1 + " + " + n2)
      sender ! AddResult(n1, n2, n1 + n2)
    case Subtract(n1, n2) =>
      println("Calculating " + n1 + " - " + n2)
      sender ! SubtractResult(n1, n2, n1 - n2)
  }
}