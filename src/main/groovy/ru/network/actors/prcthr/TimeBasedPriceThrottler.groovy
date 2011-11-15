package ru.network.actors.prcthr

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import ru.network.actors.Bus
import static ru.network.actors.util.Util.executeEvery
import static java.lang.StrictMath.max
import static java.lang.Math.min

/**
 * User: dima
 * Date: 14/11/2011
 */
@ActiveObject
class TimeBasedPriceThrottler {
  private final Map<String, PriceSnapshot> snapshotMap = [:]
  private final Bus bus

  TimeBasedPriceThrottler(Bus bus, int interval) {
    this.bus = bus
    bus.addListener(this)
    executeEvery(interval) { publishThrottledPrices() }
  }

  @ActiveMethod
  def publishThrottledPrices() {
    snapshotMap.values().each { bus.publish(it) }
    snapshotMap.clear()
  }

  @ActiveMethod
  def onMessage(message) {
    if (!(message instanceof RawPriceSnapshot)) return

    message.priceSnapshot.with {
      def oldSnapshot = snapshotMap.get(symbol)
      if (oldSnapshot == null) {
        snapshotMap.put(symbol, it)
      } else {
        snapshotMap.put(symbol,
                new PriceSnapshot(symbol,
                        new Price(max(oldSnapshot.ask.high, ask.high), min(oldSnapshot.ask.low, ask.low)),
                        new Price(max(oldSnapshot.bid.high, bid.high), min(oldSnapshot.bid.low, bid.low))
                )
        )
      }
    }
  }
}
