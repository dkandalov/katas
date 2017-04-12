package katas.groovy.network.attempt1

/**
 * User: dima
 * Date: 30/1/11
 */
class NOutputPort {
  def host
  String name
  Network network

  NOutputPort(def host, String name, Network network) {
    this.host = host
    this.name = name
    this.network = network
  }

  def send(def message) {
    network.sendMessage(host, name, message)
  }
}
