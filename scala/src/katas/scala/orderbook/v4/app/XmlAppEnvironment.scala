package katas.scala.orderbook.v4.app

import katas.scala.orderbook.v4.iface.{Action, LogLevel, Order}

import scala.xml.XML

/**
 * User: dima
 * Date: 26/03/2012
 */
class XmlAppEnvironment(filename: String, logLevel: LogLevel) extends AbstractAppEnvironment(logLevel) {
  val actions = Map(
    "add" -> Action.ADD,
    "edit" -> Action.EDIT,
    "remove" -> Action.REMOVE
  )

  override protected def feedOrders() {
    val xml = XML.loadFile(filename)
    xml.child.filter{ command => actions.contains(command.label) }.foreach { command =>
      val action = actions(command.label)
      val order = new Order(
        command.attribute("order-id").get.text.toLong,
        command.attribute("symbol").map(_.text).getOrElse(null),
        command.attribute("type").map(_.text == "buy").getOrElse(false),
        command.attribute("price").map(_.text.toInt).getOrElse(-1),
        command.attribute("quantity").map(_.text.toInt).getOrElse(-1)
      )
      notifyOrder(action, order)
    }
  }
}