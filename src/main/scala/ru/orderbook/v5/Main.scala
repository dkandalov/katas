package ru.orderbook.v5

import scala.xml.XML
import java.io.File
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{Attributes}
import akka.actor.{Actor, Props, ActorSystem}

/**
 * User: dima
 * Date: 08/04/2012
 */

object Main {
  def main(args: Array[String]) {
    val system = ActorSystem("orderBook")
    val commandReader = system.actorOf(Props[XmlCommandReader])
    system.shutdown()

    val filename = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders1.xml"
    XML.parser.parse(new File(filename), new DefaultHandler {
      override def startElement(uri: String, localName: String, qName: String, attributes: Attributes) {
        def valueOf(name: String) = attributes.getValue(name)

        val command = qName match {
          case "add" =>
            Add(
              valueOf("order-id").toInt,
              valueOf("symbol"),
              valueOf("type") == "buy",
              valueOf("price").toInt,
              valueOf("quantity").toInt
            )
          case "edit" =>
            Edit(
              valueOf("order-id").toInt,
              valueOf("price").toInt,
              valueOf("quantity").toInt
            )
          case "remove" =>
            Remove(valueOf("order-id").toInt)
          case _ => Skip
        }
        println(command)
      }

      override def endDocument() {
        println("finished")
      }
    })
  }
}

class XmlCommandReader extends Actor {
  protected def receive = null
}

// TODO try using Symbol type with StringLike behavior

sealed trait Command
case class Add(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int) extends Command
case class Edit(id: Int, price: Int, size: Int) extends Command
case class Remove(id: Int) extends Command
case object Skip extends Command