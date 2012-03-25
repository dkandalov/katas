package ru.orderbook.v4.consumer

import ru.orderbook.v4.iface._
import collection.immutable.HashMap


/**
 * User: dima
 * Date: 25/03/2012
 */
class OrderConsumerImpl extends OrderConsumer {
  var log: Log = null
  var orderBooks: Map[String, OrderBook] = new HashMap().withDefaultValue( OrderBook() )

  override def startProcessing(log: Log) {
    this.log = log
    log.log(LogLevel.INFO, "Started processing orders")
  }

  override def handleEvent(action: Action, order: Order) {
    val (symbol, book) = findBookFor(order)
    val updatedBook = action match {
      case Action.ADD => book.add(order)
      case Action.EDIT => book.edit(order)
      case Action.REMOVE => book.remove(order)
    }
    orderBooks = orderBooks.updated(symbol, updatedBook)
  }

  private def findBookFor(order: Order): (String, OrderBook) = {
    if (order.getSymbol != null) (order.getSymbol, orderBooks(order.getSymbol))
    else orderBooks.find(entry => entry._2.has(order)).get
  }

  override def finishProcessing() {
    orderBooks.foreach{ book =>
      log.log(LogLevel.INFO, book.toString())
    }
  }
}

