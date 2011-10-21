package ru.network.actors

/**
 * User: dima
 * Date: 21/10/2011
 */
class Main {
  static void main(String[] args) {
    def bus = new Bus()
    new BusListener(bus)
    new DataFeed(bus)

    Thread.sleep(100000)
  }
}
