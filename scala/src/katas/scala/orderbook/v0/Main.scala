package katas.scala.orderbook.v0

import scala.xml.{Node, XML}

/**
 * User: dima
 * Date: 08/11/2011
 */
object Direction extends Enumeration {
  type Direction = Value
  val Buy, Sell = Value
}
import katas.scala.orderbook.v0.Direction._


object Main {
  def main(args: Array[String]) {
    val xml = XML.loadFile("scala/src/katas/scala/orderbook/orders1.xml")
    val commands = xml.child.filterNot(_.label == "#PCDATA").map(createCommand)
//    commands.foreach(println)
    val orderBook = commands.foldLeft(OrderBook()) { (book, command) => command.applyTo(book)}

    println("Bid:")
    orderBook.bidSide.foreach(println)
    println("Ask:")
    orderBook.askSide.foreach(println)
  }

  def createCommand(node: Node): Command = {
    if (node.label == "add") AddOrder(
      node.attribute("order-id").get.text.toInt,
      node.attribute("symbol").get.text,
      node.attribute("quantity").get.text.toInt,
      node.attribute("price").get.text.toDouble,
      asDirection(node.attribute("type").get.text)
    )
    else if (node.label == "edit") EditOrder(
      node.attribute("order-id").get.text.toInt,
      node.attribute("quantity").get.text.toInt,
      node.attribute("price").get.text.toDouble
    )
    else if (node.label == "remove") RemoveOrder(
      node.attribute("order-id").get.text.toInt
    )
    else throw new IllegalStateException(  "Unsupported node with label: " + node.label)
  }

  def asDirection(s: String): Direction = {
    if (s.equalsIgnoreCase("buy")) Buy
    else if (s.equalsIgnoreCase("sell")) Sell
    else throw new IllegalStateException()
  }
}

case class Order(id: Int, size: Int, price: Double, direction: Direction)

case class OrderBook(bidSide: List[Order] = List(), askSide: List[Order] = List()) {
  def add(order: Order): OrderBook = order.direction match {
    case Buy => OrderBook((order :: bidSide).sortBy(_.price), askSide)
    case Sell => OrderBook(bidSide, (order :: askSide).sortBy(_.price)(Ordering[Double].reverse))
  }

  def edit(updatedOrder: Order): OrderBook = {
    val oldOrder = bidSide.find(_.id == updatedOrder.id) match {
      case Some(x) => x
      case None => askSide.find(_.id == updatedOrder.id) match {
        case Some(x) => x
        case None => throw new IllegalStateException()
      }
    }
    remove(updatedOrder.id).add(Order(
      oldOrder.id,
      updatedOrder.size,
      updatedOrder.price,
      oldOrder.direction
    ))
  }

  def remove(orderId: Int): OrderBook = {
    OrderBook(bidSide.filterNot(_.id == orderId), askSide.filterNot(_.id == orderId))
  }
}

abstract class Command {
  def applyTo(orderBook: OrderBook): OrderBook = this match {
    case AddOrder(id, symbol, quantity, price, direction) => orderBook.add(Order(id, quantity, price, direction))
    case EditOrder(id, quantity, price) => orderBook.edit(Order(id, quantity, price, null))
    case RemoveOrder(id) => orderBook.remove(id)
  }
}
case class AddOrder(orderId: Int, symbol: String, quantity: Int, price: Double, direction: Direction) extends Command
case class EditOrder(orderId: Int, quantity: Int, price: Double) extends Command
case class RemoveOrder(orderId: Int) extends Command