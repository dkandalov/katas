package ru.network.actors.prcthr

/**
 * User: dima
 * Date: 14/11/2011
 */
@groovy.transform.Immutable
final class PriceSnapshot {
  String symbol
  Price ask
  Price bid

  public String toString() {
    return "PriceSnapshot{" +
            "symbol='" + symbol + '\'' +
            ", ask=" + ask +
            ", bid=" + bid +
            '}';
  }
}
