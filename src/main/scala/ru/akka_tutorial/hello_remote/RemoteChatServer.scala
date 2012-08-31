package ru.akka_tutorial.hello_remote

import com.typesafe.config.ConfigFactory
import ru.akka_tutorial.remote.CalcApp
import akka.actor.{ActorRef, Actor, Props, ActorSystem}

/**
 * User: dima
 * Date: 16/08/2012
 */

object RemoteChatServer {
	val config = ConfigFactory.parseString("""
akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    netty {
      hostname = "10.106.0.20"
    }
  }
}

chatServer {
  akka {
    remote.netty.port = 2552
    remote.netty.hostname = "10.106.0.20"
  }
}
chatClient {
  akka {
    remote.netty.port = 2553
  }
}
""")

	def main(args: Array[String]) {
		val system = ActorSystem("MyChat", ConfigFactory.load(config).getConfig("chatServer").withFallback(config))
		val actor = system.actorOf(Props[CharServerActor], "myChatServer")
	}

	trait ServerMessage
	case class AddClient(actor: ActorRef)
	case class TextMessage(text: String)

	class CharServerActor extends Actor {
		var clients: Seq[ActorRef] = Seq()
		
		def receive = {
			case AddClient(actor) => clients = clients :+ actor
			case TextMessage(text) => clients.foreach{ _ ! text }
		}
	}
}