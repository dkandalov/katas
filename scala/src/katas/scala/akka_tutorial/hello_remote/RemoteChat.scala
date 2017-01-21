package katas.scala.akka_tutorial.hello_remote

import akka.actor.ActorSystem._
import com.typesafe.config.ConfigFactory
import akka.actor.{Actor, Props, ActorSystem}

/**
 * User: dima
 * Date: 16/08/2012
 */

object RemoteChat {
	def main(args: Array[String]) {
		val system = ActorSystem("MyChat", ConfigFactory.load(RemoteChatServer.config).getConfig("chatClient").withFallback(RemoteChatServer.config))
		val chatServer = system.actorFor("akka://MyChat@10.106.0.20:2552/user/myChatServer")
		chatServer ! "Heeeeey!!!"
	}

//	class ChatClientActor extends Actor {
//		def receive = {
//		}
//	}
}