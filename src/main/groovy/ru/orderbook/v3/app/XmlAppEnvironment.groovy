package ru.orderbook.v3.app

import ru.orderbook.v3.iface.Action
import ru.orderbook.v3.iface.LogLevel
import ru.orderbook.v3.iface.Order

/**
 * User: dima
 * Date: 25/03/2012
 */
class XmlAppEnvironment extends AbstractAppEnvironment {
  private final String filename

  XmlAppEnvironment(String filename, LogLevel logLevel) {
    super(logLevel)
    this.filename = filename
  }

  @Override
  protected void feedOrders() {
    def commands = new XmlParser().parse(new File(filename))
    commands.each { command ->
      def order = command.attributes().with {
        new Order(
                asLong(it["order-id"]),
                it["symbol"],
                it["type"] == "buy",
                asInt(it["price"]),
                asInt(it["quantity"])
        )
      }
      def action = ["add": Action.ADD, "edit": Action.EDIT, "remove": Action.REMOVE][command.name()]

      notifyOrder(action, order)
    }
  }

  private static long asLong(value) {
    try {
      Long.parseLong(value)
    } catch (NumberFormatException e) {
      0L
    }
  }

  private static int asInt(value) {
    try {
      Integer.parseInt(value)
    } catch (NumberFormatException e) {
      0
    }
  }
}
