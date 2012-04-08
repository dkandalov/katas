package ru.orderbook.v5

import scala.xml.XML
import java.io.File
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{Attributes}
import akka.actor.{ActorRef, Actor, Props, ActorSystem}

import scala.collection._
import ru.orderbook.v5.XmlCommandReader.ReadFrom

/**
 * User: dima
 * Date: 08/04/2012
 */

object Main {
  def main(args: Array[String]) {
    val system = ActorSystem("orderBook")

    val orderRouter = system.actorOf(Props[OrderRouter])
    val reportBuilder = system.actorOf(Props(new ReportBuilder(orderRouter)))
    val orderRegistry = system.actorOf(Props(new OrderRegistry(orderRouter)))
    val commandReader = system.actorOf(Props(new XmlCommandReader(orderRegistry, reportBuilder)))
    commandReader ! XmlCommandReader.ReadFrom("/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders1.xml")

//    system.shutdown()
  }
}

// TODO try using Symbol type with StringLike behavior
// TODO try using Price type for price

// common messages
case object StartOfStream
case object EndOfStream
case object Unknown

// sent from XmlCommandReader to OrderRegistry
case class Add(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int)
case class Edit(id: Int, price: Int, size: Int)
case class Remove(id: Int)

// sent from OrderRegistry to OrderBooks
case class AddOrder(order: Order)
case class RemoveOrder(order: Order)
case class UpdateOrder(oldOrder: Order, newOrder: Order)
case class Order(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int)
case class PriceLevel(price: Int, size: Int, count: Int)

case object ReportRequest
case class ExpectedReportSize(size: Int)
case class OrderBookReport(symbol: String, bidSide: immutable.Map[Int, PriceLevel], askSide: immutable.Map[Int, PriceLevel])

class ReportBuilder(orderRouter: ActorRef) extends Actor {
  var expectedReportsSize: Int = 0
  val reports: mutable.Map[String, OrderBookReport] = mutable.Map()
  
  protected def receive = {
    case EndOfStream => orderRouter ! ReportRequest
    case ExpectedReportSize(size) => expectedReportsSize = size
    case report@OrderBookReport(symbol, _, _) =>
      reports(symbol) = report
      if (reports.size == expectedReportsSize)
        println(reports)
  }
}

class OrderBook(symbol: String) extends Actor {
  private val bidSide: mutable.Map[Int, PriceLevel] = new mutable.HashMap() // TODO (Ordering.Int.reverse)
  private val askSide: mutable.Map[Int, PriceLevel] = new mutable.HashMap()

  protected def receive = {
    case AddOrder(order) => add(order)
    case RemoveOrder(order) => remove(order)
    case UpdateOrder(oldOrder, newOrder) =>
      remove(oldOrder)
      add(newOrder)
    case ReportRequest => sender ! OrderBookReport(symbol, immutable.Map(), immutable.Map())
    case msg@_ => println("OrderBook doesn't understand: " + msg)
  }

  private def add(order: Order) {
    bookSide(order.isBuy).put(order.price, PriceLevel(order.price, order.size, 1))
  }

  private def remove(order: Order) {
    val level = bookSide(order.isBuy)(order.price)
    bookSide(order.isBuy)(order.price) = PriceLevel(order.price, level.size - order.size, level.count - 1)
  }

  private def bookSide(isBuy: Boolean) = if (isBuy) bidSide else askSide
}

class OrderRouter extends Actor {
  private val orderBooks: mutable.Map[String, ActorRef] = mutable.Map()

  protected def receive = {
    case msg@AddOrder(order) => orderBookFor(order) ! msg
    case msg@UpdateOrder(oldOrder, _) => orderBookFor(oldOrder) ! msg
    case msg@RemoveOrder(order) => orderBookFor(order) ! msg
    case request@ReportRequest =>
      sender ! ExpectedReportSize(orderBooks.size)
      orderBooks.values.foreach{_ forward request}
    case msg@_ => println("OrderRouter doesn't understand :" + msg)
  }

  def orderBookFor(order: Order): ActorRef = {
    orderBooks.getOrElseUpdate(order.symbol, context.actorOf(Props(new OrderBook(order.symbol))))
  }
}

class OrderRegistry(orderRouter: ActorRef) extends Actor {
  private var orders: mutable.Map[Int, Order] = mutable.Map()

  protected def receive = {
    case Add(id, symbol, isBuy, price, size) =>
      val order = Order(id, symbol, isBuy, price, size)
      orders = orders.updated(id, order)
      orderRouter ! AddOrder(order)
    case Edit(id, price, size) =>
      Thread.sleep(100)
      val order = orders(id)
      val newOrder = Order(id, order.symbol, order.isBuy, price, size)
      orders(id) = newOrder
      orderRouter ! UpdateOrder(order, newOrder)
    case Remove(id) =>
      orderRouter ! RemoveOrder(orders.remove(id).get)
    case msg@_ => orderRouter ! msg
  }
}

object XmlCommandReader {
  case class ReadFrom(filename: String)
}

class XmlCommandReader(orderRegistry: ActorRef, reportBuilder: ActorRef) extends Actor {
  protected def receive = {
    case ReadFrom(filename) =>
      // use separate thread so that not to use actors thread-pool
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

      override def startDocument() {
        reportBuilder ! StartOfStream
      }

      override def endDocument() {
        reportBuilder ! EndOfStream
      }
    })
  }
}
