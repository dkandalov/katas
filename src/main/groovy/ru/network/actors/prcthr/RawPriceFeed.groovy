package ru.network.actors.prcthr

import groovyx.gpars.activeobject.ActiveObject
import ru.network.actors.Bus
import static ru.network.actors.util.Util.executeEvery

/**
 * User: dima
 * Date: 14/11/2011
 */
@ActiveObject
class RawPriceFeed {
  int intervalMillis

  RawPriceFeed(Bus bus, long intervalMillis = 500) {
    this.intervalMillis = intervalMillis
    executeEvery(intervalMillis, {
      bus.publish(rawPriceSnapshotFor("AAA"))
      bus.publish(rawPriceSnapshotFor("BBB"))
    })
  }

  RawPriceSnapshot rawPriceSnapshotFor(symbol) {
    new RawPriceSnapshot(new PriceSnapshot(
            symbol,
            new Price(new Random().nextDouble(), new Random().nextDouble()),
            new Price(new Random().nextDouble(), new Random().nextDouble()),
            intervalMillis
    ))
  }
}

@groovy.transform.Immutable
final class RawPriceSnapshot {
  PriceSnapshot priceSnapshot

  public String toString() {
    return "RawPriceSnapshot{" +
            "priceSnapshot=" + priceSnapshot +
            '}';
  }
}