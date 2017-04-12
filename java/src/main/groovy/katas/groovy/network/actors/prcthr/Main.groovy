package katas.groovy.network.actors.prcthr

import katas.groovy.network.actors.Bus

import katas.groovy.network.actors.PrintingBusListener

/**
 * User: dima
 * Date: 14/11/2011
 */
class Main {
  public static void main(String[] args) {
    new Bus().with { Bus bus ->
      new PrintingBusListener(bus)
      new RawPriceFeed(bus, 300)
      new TimeBasedPriceThrottler(bus, 1000)
      new MultBasedPriceThrottler(bus, 1000, 2)
    }
    Thread.sleep(1000000)
  }
}
