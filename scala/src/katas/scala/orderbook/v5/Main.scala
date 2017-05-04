package katas.scala.orderbook.v5

import scala.xml.XML
import java.io.File

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.Attributes
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection._
import immutable.TreeMap
import katas.scala.orderbook.v5.XmlCommandReader.ReadFrom
import java.util.concurrent.{SynchronousQueue, TimeUnit}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * User: dima
 * Date: 08/04/2012
 */

object Main {
  def main(args: Array[String]) {
    val system = ActorSystem("orderBook")

    val report = new SynchronousQueue[CharSequence]()
    val reportBuilder = system.actorOf(Props(new ReportBuilder(report)))
    val orderRouter = system.actorOf(Props(new OrderRouter(reportBuilder)))
    val orderRegistry = system.actorOf(Props(new OrderRegistry(orderRouter)))
    val commandReader = system.actorOf(Props(new XmlCommandReader(orderRegistry)))
    commandReader ! XmlCommandReader.ReadFrom("/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders2.xml")

    println(report.poll(10, TimeUnit.SECONDS))

    Await.ready(system.terminate(), Duration.Inf)
  }
}

// common messages
case object StartOfStream
case object EndOfStream

// sent from XmlCommandReader to OrderRegistry
case class Add(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int)
case class Edit(id: Int, price: Int, size: Int)
case class Remove(id: Int)

// sent from OrderRegistry to OrderRouter and eventually OrderBooks
case class AddOrder(order: Order)
case class RemoveOrder(order: Order)
case class UpdateOrder(oldOrder: Order, newOrder: Order)

// messages for generating report
case object ReportRequest
case class ExpectedReportSize(size: Int)
case class OrderBookReport(symbol: String, bidSide: immutable.Map[Int, PriceLevel], askSide: immutable.Map[Int, PriceLevel])

case class Order(id: Int, symbol: String, isBuy: Boolean, price: Int, size: Int)
case class PriceLevel(price: Int, size: Int, count: Int)


class ReportBuilder(reportOutput: SynchronousQueue[CharSequence]) extends Actor {
  private var expectedReportsSize: Int = 0
  private var reports: TreeMap[String, OrderBookReport] = TreeMap()
  
  def receive = {
    case ExpectedReportSize(size) => expectedReportsSize = size
    case report@OrderBookReport(symbol, _, _) =>
      reports = reports.updated(symbol, report)
      if (reports.size == expectedReportsSize) {
        reportOutput.add(format(reports))
      }
  }

  private def format(reports: TreeMap[String, OrderBookReport]): CharSequence = {
    reports.values.foldLeft("") { (acc, report) =>
      acc + "\n" + report.symbol + "\n" +
        "bidSide\n" + format(report.bidSide) +
        "askSide\n" + format(report.askSide)
    }
  }

  private def format(bookSide: immutable.Map[Int, PriceLevel]): CharSequence = {
    bookSide.values.foldLeft("") { (acc, level) =>
      acc + "\tprice = " + level.price + ", size = " + level.size + ", count = " + level.count + "\n"
    }
  }
}


class OrderBook(symbol: String) extends Actor {
  private var bidSide: immutable.Map[Int, PriceLevel] = new TreeMap()(Ordering.Int.reverse).withDefault{ PriceLevel(_, 0, 0) }
  private var askSide: immutable.Map[Int, PriceLevel] = new TreeMap()(Ordering.Int).withDefault{ PriceLevel(_, 0, 0) }

  def receive = {
    case AddOrder(order) => add(order)
    case RemoveOrder(order) => remove(order)
    case UpdateOrder(oldOrder, newOrder) =>
      remove(oldOrder)
      add(newOrder)
    case ReportRequest => sender ! OrderBookReport(symbol, bidSide, askSide)
    case msg@_ => println("OrderBook doesn't understand: " + msg)
  }

  private def add(order: Order) {
    updateBookSideFor(order, { (bookSide, level) =>
      bookSide.updated(level.price, PriceLevel(level.price, level.size + order.size, level.count + 1))
    })
  }

  private def remove(order: Order) {
    updateBookSideFor(order, { (bookSide, level) =>
      if (level.count <= 1) bookSide - level.price
      else bookSide.updated(level.price, PriceLevel(level.price, level.size - order.size, level.count - 1))
    })
  }

  private def updateBookSideFor(order: Order, f: (immutable.Map[Int, PriceLevel], PriceLevel) => immutable.Map[Int, PriceLevel]) {
    if (order.isBuy)
      bidSide = f(bidSide, bidSide(order.price))
    else
      askSide = f(askSide, askSide(order.price))
  }
}


class OrderRouter(reportBuilder: ActorRef) extends Actor {
  private val orderBooks: mutable.Map[String, ActorRef] = mutable.Map()

  def receive = {
    case msg@AddOrder(order) => orderBookFor(order) ! msg
    case msg@UpdateOrder(oldOrder, _) => orderBookFor(oldOrder) ! msg
    case msg@RemoveOrder(order) => orderBookFor(order) ! msg

    case EndOfStream =>
      reportBuilder ! ExpectedReportSize(orderBooks.size)
      orderBooks.values.foreach{_ ! ReportRequest}
    case msg : OrderBookReport => reportBuilder ! msg

    case msg@_ => println("OrderRouter doesn't understand :" + msg)
  }

  def orderBookFor(order: Order): ActorRef = {
    orderBooks.getOrElseUpdate(order.symbol, { context.actorOf(Props(new OrderBook(order.symbol))) })
  }
}


class OrderRegistry(orderRouter: ActorRef) extends Actor {
  private var orders: mutable.Map[Int, Order] = mutable.Map()

  def receive = {
    case Add(id, symbol, isBuy, price, size) =>
      val order = Order(id, symbol, isBuy, price, size)
      orders = orders.updated(id, order)
      orderRouter ! AddOrder(order)
    case Edit(id, price, size) =>
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

class XmlCommandReader(orderRegistry: ActorRef) extends Actor {
  def receive = {
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
          case _ => () // ignore
        }
        if (command != ()) {
          orderRegistry ! command
        }
      }

      override def startDocument() {
        orderRegistry ! StartOfStream
      }

      override def endDocument() {
        orderRegistry ! EndOfStream
      }
    })
  }
}
