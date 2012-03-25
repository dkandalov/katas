package ru.orderbook.v4.app

import ru.orderbook.v4.iface.{Order, Action, LogLevel}


/**
 * User: dima
 * Date: 25/03/2012
 */
class FakeAppEnvironment(var logLevel: LogLevel) extends AbstractAppEnvironment(logLevel) {

  override protected def feedOrders() {
    val commands = Seq(
      (Action.ADD, new Order(1L, "MSFT.L", true, 5, 200)),
      (Action.ADD, new Order(2L, "VOD.L", true, 15, 100)),
      (Action.ADD, new Order(3L, "MSFT.L", false, 5, 300)),
      (Action.ADD, new Order(4L, "MSFT.L", true, 7, 150)),
      (Action.REMOVE, new Order(1L, null, true, -1, -1)),
      (Action.ADD, new Order(5L, "VOD.L", false, 17, 300)),
      (Action.ADD, new Order(6L, "VOD.L", true, 12, 150)),
      (Action.EDIT, new Order(3L, null, true, 7, 200)),
      (Action.ADD, new Order(7L, "VOD.L", false, 16, 100)),
      (Action.ADD, new Order(8L, "VOD.L", false, 19, 100)),
      (Action.ADD, new Order(9L, "VOD.L", false, 21, 112)),
      (Action.REMOVE, new Order(5L, null, false, -1, -1))
    )
    commands.foreach{ command => notifyOrder(command._1, command._2) }
  }
}