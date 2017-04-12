package katas.groovy.network.actors.trades

/**
 * User: dima
 * Date: 26/10/2011
 */
final class SequencedMessage {
  final String id
  final String publisherId
  final def payload

  SequencedMessage(String id, payload, String publisherId) {
    this.id = id
    this.payload = payload
    this.publisherId = publisherId
  }

  String toString() {
    return "SequencedMessage{" +
            "id='" + id + '\'' +
            ", publisherId='" + publisherId + '\'' +
            ", payload=" + payload +
            '}';
  }
}
