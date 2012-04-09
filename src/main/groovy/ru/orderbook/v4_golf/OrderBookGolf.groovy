package ru.orderbook.v4_golf

import ru.util.Pomodoro
import static java.lang.Integer.parseInt

/**
 * User: dima
 * Date: 07/04/2012
 */
@Pomodoro("4")
class OrderBookGolf {
  public static void main(String[] args) {
    def comp = {a, b ->
      if (a[0] != b[0]) a[0] <=> b[0]
      else if (a[1] != b[1]) -(a[1] <=> b[1])
      else (a[2] <=> b[2]) * (a[1] ? -1 : 1)
    } as Comparator

    def fileName = "file:///Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders2.xml"
    println new XmlParser().parse(fileName).children().collect() { node ->
      def asInt = { it != null ? parseInt(it) : 0 }
      node.attributes().with { [action: node.name(), id: asInt(it["order-id"]), symbol: it["symbol"],
              buy: it["type"] == "buy", price: asInt(it["price"]), size: asInt(it["quantity"])] }
    }.inject(new TreeMap(comp).withDefault{[size: 0, count: 0, cmds: []]}) { acc, command ->
      def add = { cmd ->
        def key = [cmd.symbol, cmd.buy, cmd.price]
        acc[key] = [size: acc[key].size + cmd.size, count: acc[key].count + 1, cmds: acc[key].cmds + cmd]
      }
      def remove = { cmd ->
        def old = acc.find{ it.value.cmds.any{it.id == cmd.id} }
        cmd = old.value.cmds.find{ it.id == cmd.id }
        if (old.value.count == 1) acc.remove(old.key)
        else acc[old.key] = [size: old.value.size - cmd.size, count: old.value.count - 1, cmds: old.value.cmds - cmd]
        cmd
      }
      [add: {add(command)}, remove: {remove(command)},
       edit: {add(remove(command).with {price = command.price; size = command.size; it})}][command.action](command)
      acc
    }.groupBy{[it.key[0], it.key[1]]}.groupBy{it.key[0]}.inject("") { acc, entry ->
      acc + "\n${entry.key}\n" +
              entry.value.inject("") { ac, en ->
                ac + (en.key[1] ? "bid" : "ask") + "Side\n" +
                        en.value.inject("") {a, e -> a + "\tprice = ${e.key[2]}, size = ${e.value.size}, count = ${e.value.count}\n" }
              }
    }
  }
}
