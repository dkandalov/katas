package ru.network.actors.prcthr

import static java.lang.Math.max
import static java.lang.StrictMath.min

/**
 * User: dima
 * Date: 14/11/2011
 */
@groovy.transform.Immutable
final class PriceSnapshot {
  String symbol
  Price ask
  Price bid
  int interval

  PriceSnapshot mergeWith(PriceSnapshot snapshot) {
    new PriceSnapshot(symbol,
            new Price(max(snapshot.ask.high, ask.high), min(snapshot.ask.low, ask.low)),
            new Price(max(snapshot.bid.high, bid.high), min(snapshot.bid.low, bid.low)),
            snapshot.interval
    )
  }

  PriceSnapshot withInterval(int newInterval) {
    new PriceSnapshot(symbol, ask, bid, newInterval)
  }

  String toString() {
    "PriceSnapshot{" +
            "symbol='" + symbol + '\'' +
            ", ask=" + ask +
            ", bid=" + bid +
            ", interval=" + interval +
            '}';
  }
}

@groovy.transform.Immutable
final class StartOfSnapshots {}

@groovy.transform.Immutable
final class EndOfSnapshots {}
