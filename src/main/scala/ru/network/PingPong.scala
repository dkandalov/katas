package ru.network


/**
 * @author DKandalov
 */
/*
 TODO broken after moving to scala 2.10; should change code below to use akka

object PingPong {
  def main(args: Array[String]) {
    val pong = actor {
      var pongCount = 0
      loop {
        react {
          case PingSender =>
            if (pongCount % 1000 == 0)
              Console.println("Pong: ping " + pongCount)
            sender ! PongSender
            pongCount = pongCount + 1
          case Stop =>
            Console.println("Pong: stop")
            exit()
        }
      }
    }
    val count = 100000
    actor {
      var pingsLeft = count - 1
      pong ! PingSender
      loop {
        react {
          case PongSender =>
            if (pingsLeft % 1000 == 0)
              Console.println("Ping: pong")
            if (pingsLeft > 0) {
              pong ! PingSender
              pingsLeft -= 1
            } else {
              Console.println("Ping: stop")
              pong ! Stop
              exit()
            }
        }
      }
    }
  }

  case object PingSender

  case object PongSender

  case object Stop

}
*/
