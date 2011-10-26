package ru.network.actors

/**
 * User: dima
 * Date: 26/10/2011
 */
final class ConsumedTrade {
  final String consumerId
  final def payload

  ConsumedTrade(String consumerId, def payload) {
    this.consumerId = consumerId
    this.payload = payload
  }


  String toString() {
    return "ConsumedTrade{" +
            "consumerId='" + consumerId + '\'' +
            ", payload=" + payload +
            '}';
  }
}
