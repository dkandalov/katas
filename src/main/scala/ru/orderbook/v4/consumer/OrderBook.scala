package ru.orderbook.v4.consumer

import ru.orderbook.v4.iface.Order
import collection.immutable.{TreeMap}

case class OrderBook(bidSide: BookSide = BookSide.newBid, askSide: BookSide = BookSide.newAsk,
                      orders: Map[Long, Order] = Map()) {

  def add(order: Order): OrderBook = {
    val updatedOrders = orders.updated(order.getOrderId, order)

    if (order.isBuy) OrderBook(bidSide.add(order), askSide, updatedOrders)
    else OrderBook(bidSide, askSide.add(order), updatedOrders)
  }

  def edit(order: Order): OrderBook = {
    val oldOrder = orders(order.getOrderId)
    val newOrder = new Order(order.getOrderId, oldOrder.getSymbol, oldOrder.isBuy, order.getPrice, order.getQuantity)
    val updatedOrders = orders.updated(order.getOrderId, newOrder)

    if (oldOrder.isBuy) OrderBook(bidSide.update(oldOrder, newOrder), askSide, updatedOrders)
    else OrderBook(bidSide, askSide.update(oldOrder, newOrder), updatedOrders)
  }

  def remove(order: Order): OrderBook = {
    val oldOrder = orders(order.getOrderId)
    val updatedOrders = orders - order.getOrderId

    if (oldOrder.isBuy) OrderBook(bidSide.remove(oldOrder), askSide, updatedOrders)
    else OrderBook(bidSide, askSide.remove(oldOrder), updatedOrders)
  }

  def has(order: Order) = orders.contains(order.getOrderId)

  override def toString = "\nbidSide: " + bidSide + "\naskSide: " + askSide
}

object BookSide {
  def newBid = BookSide(TreeMap[Int, PriceLevel]()(Ordering.Int.reverse).withDefault{PriceLevel(_)})
  def newAsk = BookSide(TreeMap[Int, PriceLevel]().withDefault{PriceLevel(_)})
}

case class BookSide(levels: Map[Int, PriceLevel]) {
  def add(order: Order): BookSide = {
    val level = levels(order.getPrice)
    BookSide(levels.updated(level.price,
      PriceLevel(level.price, level.size + order.getQuantity, level.count + 1)
    ))
  }

  def remove(order: Order): BookSide = {
    val level = levels(order.getPrice)
    if (level.count == 1)
      BookSide(levels - order.getPrice)
    else
      BookSide(levels.updated(level.price,
        PriceLevel(level.price, level.size - order.getQuantity, level.count - 1)
      ))
  }

  def update(oldOrder: Order, newOrder: Order): BookSide = {
    remove(oldOrder).add(newOrder)
  }
}

case class PriceLevel(price: Int, size: Int = 0, count: Int = 0)