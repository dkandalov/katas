package ru.network.actors.prcthr

import ru.network.actors.Bus
import ru.network.actors.BusListener

/**
 * User: dima
 * Date: 14/11/2011
 */
class Main {
  public static void main(String[] args) {
    new Bus().with { Bus bus ->
      new BusListener(bus)
      new RawPriceFeed(bus, 300)
      new TimeBasedPriceThrottler(bus, 1000)
      new MultBasedPriceThrottler(bus, 1000, 2)
    }
    Thread.sleep(1000000)
  }
}
