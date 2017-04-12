package katas.groovy.network.actors.trades

import groovy.transform.Immutable

/**
 * User: dima
 * Date: 22/10/2011
 */
@Immutable
final class Trade {
  String id
  String publisherId
  String instrument
  double price
  double quantity

  String toString() {
    return "Trade{" +
            "id='" + id + '\'' +
            ", publisherId='" + publisherId + '\'' +
            ", instrument='" + instrument + '\'' +
            ", price=" + price +
            ", quantity=" + quantity +
            '}';
  }
}
