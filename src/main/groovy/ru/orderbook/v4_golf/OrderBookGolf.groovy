package ru.orderbook.v4_golf

import ru.util.Pomodoro

/**
 * User: dima
 * Date: 07/04/2012
 */
@Pomodoro("2")
class OrderBookGolf {
  public static void main(String[] args) {
    println new XmlParser().parse("file:///Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders1.xml").children().collect() { node ->
      def asInt = { it != null ? Integer.parseInt(it) : 0 }
      node.attributes().with { [action: node.name(), id: asInt(it["order-id"]), symbol: it["symbol"], buy: it["type"] == "buy", price: asInt(it["price"]), quantity: asInt(it["quantity"])] }
    }.inject(new HashMap().withDefault{[size: 0, count: 0, cmds: []]}) { acc, command ->
      def add = {
        def key = [command.symbol, command.buy, command.price]
        def old = acc[key]
        acc[key] = [size: old.size + command.quantity, count: old.count + 1, cmds: old.cmds + command]
      }
      def remove = {
        def old = acc.find{ it.value.cmds.any{it.id == command.id} }
        command = old.value.cmds.find{ it.id == command.id }
        acc[old.key] = [size: old.value.size - command.quantity, count: old.value.count - 1, ids: old.value.cmds - command]
        if (acc[old.key].count == 0) acc.remove(old.key)
      }
      if (command.action == "add") {
        add()
      } else if (command.action == "remove") {
        remove()
      } else if (command.action == "edit") {
        remove()
        add()
      }
      println acc
      acc
    }.groupBy{it.key[0]}.inject("") { acc, entry ->
      acc + "\n${entry.key}\n" +
              entry.value.inject("") {a, e ->
                a + (e.key[1] ? "bid" : "ask") + "Side\n" +
                        "\tprice = ${e.key[2]}, size = ${e.value.size}, count = ${e.value.count}\n"
              }
    }
  }
}
