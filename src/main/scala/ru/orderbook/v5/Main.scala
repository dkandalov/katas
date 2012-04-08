package ru.orderbook.v5

import scala.xml.XML
import java.io.File
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{Attributes}
import akka.actor.{ActorRef, Actor, Props, ActorSystem}

/**
 * User: dima
 * Date: 08/04/2012
 */

object Main {
  def main(args: Array[String]) {
    val system = ActorSystem("orderBook")

    val commandRouter = system.actorOf(Props[CommandRouter])
    val commandReader = system.actorOf(Props(new XmlCommandReader(commandRouter)))
    commandReader ! ReadCommandsFrom("/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders1.xml")

//    system.shutdown()
  }
}

// TODO try using Symbol type with StringLike behavior


sealed trait Action
case object StartOfStream extends Action
case object EndOfStream extends Action
case class Add(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int) extends Action
case class Edit(id: Int, price: Int, size: Int) extends Action
case class Remove(id: Int) extends Action
case object Unknown extends Action

class OrderBook extends Actor {
  protected def receive = null
}

class CommandRouter extends Actor {
  var orderBooks: Map[String, ActorRef] = Map().withDefault { _ => context.actorOf(Props[OrderBook]) }

  protected def receive = {
    case cmd : Add => orderBooks(cmd.symbol) ! cmd
    case cmd : Remove =>
    case cmd : Edit =>
    case msg@_ => println(msg)
  }
}

case class ReadCommandsFrom(filename: String)

class XmlCommandReader(commandRouter: ActorRef) extends Actor {
  protected def receive = {
    case ReadCommandsFrom(filename) =>
      commandRouter ! StartOfStream
      new Thread(new Runnable() {
        def run() {
          parse(filename)
        }
      }, "XML reading thread").start()
    case _ => println("")
  }

  private def parse(filename: String) {
    XML.parser.parse(new File(filename), new DefaultHandler {
      override def startElement(uri: String, localName: String, qName: String, attributes: Attributes) {
        def valueOf(name: String) = attributes.getValue(name)

        val command = qName match {
          case "add" => Add(
            valueOf("order-id").toInt,
            valueOf("symbol"),
            valueOf("type") == "buy",
            valueOf("price").toInt,
            valueOf("quantity").toInt
          )
          case "edit" => Edit(
            valueOf("order-id").toInt,
            valueOf("price").toInt,
            valueOf("quantity").toInt
          )
          case "remove" => Remove(valueOf("order-id").toInt)
          case _ => Unknown
        }
        commandRouter ! command
      }

      override def endDocument() {
        commandRouter ! EndOfStream
      }
    })
  }
}
