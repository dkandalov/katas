package ru.orderbook.v5

import scala.xml.XML
import java.io.File
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{Attributes}
import akka.actor.{ActorRef, Actor, Props, ActorSystem}

import scala.collection._

/**
 * User: dima
 * Date: 08/04/2012
 */

object Main {
  def main(args: Array[String]) {
    val system = ActorSystem("orderBook")

    val commandRouter = system.actorOf(Props[CommandRouter])
    val orderRegistry = system.actorOf(Props(new OrderRegistry(commandRouter)))
    val commandReader = system.actorOf(Props(new XmlCommandReader(orderRegistry)))
    commandReader ! ReadCommandsFrom("/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders1.xml")

//    system.shutdown()
  }
}

// TODO try using Symbol type with StringLike behavior
// TODO try using Price type for price


case object StartOfStream
case object EndOfStream
case object Unknown

case class Add(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int)
case class Edit(id: Int, price: Int, size: Int)
case class Remove(id: Int)

sealed trait ActionType
case object Add extends ActionType
case object Update extends ActionType
case object Remove extends ActionType
case class Action(actionType: ActionType, order: Order)

case class Order(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int)

case class PriceLevel(price: Int, size: Int, count: Int)

class OrderBook extends Actor {
  var bidSide: mutable.Map[Int, PriceLevel] = mutable.Map()
  var askSide: mutable.Map[Int, PriceLevel] = mutable.Map()

  protected def receive = {
    case _ => println("Aaaa")
  }
}

class CommandRouter extends Actor {
  private var orderBooks: mutable.Map[String, ActorRef] = mutable.Map().withDefault { _ => context.actorOf(Props[OrderBook]) }

  protected def receive = {
    case msg@Action(_, order) => orderBooks(order.symbol) ! msg
    case msg@_ => println(msg)
  }
}

class OrderRegistry(commandRouter: ActorRef) extends Actor {
  private var orders: mutable.Map[Int, Order] = mutable.Map()

  protected def receive = {
    case Add(id, symbol, isBuy, price, size) =>
      val order = Order(id, symbol, isBuy, price, size)
      orders = orders.updated(id, order)
      commandRouter ! Action(Add, order)
    case Edit(id, price, size) =>
      val order = orders(id)
      val newOrder = Order(id, order.symbol, order.isBuy, price, size)
      orders(id) = newOrder
      commandRouter ! Action(Update, order)
    case Remove(id) =>
      commandRouter ! Action(Remove, orders.remove(id).get)
  }
}

case class ReadCommandsFrom(filename: String)

class XmlCommandReader(orderRegistry: ActorRef) extends Actor {
  protected def receive = {
    case ReadCommandsFrom(filename) =>
      orderRegistry ! StartOfStream
      // separate thread so that not to use actors thread-pool
      new Thread(new Runnable() {
        def run() {
          parse(filename)
        }
      }, "XML reading thread").start()
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
        orderRegistry ! command
      }

      override def endDocument() {
        orderRegistry ! EndOfStream
      }
    })
  }
}
