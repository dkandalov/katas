package ru.network.actors

import java.awt.GridLayout
import javax.swing.JFrame
import ru.network.actors.ui.BusListeningForm

/**
 * User: dima
 * Date: 21/10/2011
 */
class Main {
  static void main(String[] args) {
    def bus = new Bus()
    def consumer = new TradeConsumer(bus, "cons-1")
    def replayer = new TradeReplayer(bus)

    displayInFrame(4, 2) {
      add(BusListeningForm.createForm("Bus", bus))
      add(BusListeningForm.createForm("Data Feed 1", bus) { it instanceof Trade && it.publisherId == "datafeed-1" } )
      add(BusListeningForm.createForm("Data Feed 2", bus) { it instanceof Trade && it.publisherId == "datafeed-2" } )
      add(BusListeningForm.createForm("Sequencer 1", bus) { it instanceof SequencedMessage && it.publisherId == "seq-1" } )
      add(BusListeningForm.createForm("Replayer", replayer.receivedTradesBus))
      add(BusListeningForm.createForm("Consumer", consumer.consumedTradesBus))
    }

    new TradeSequencer(bus, "seq-1")
    new DataFeed(bus, "datafeed-1")
    new DataFeed(bus, "datafeed-2", 2000)

    Thread.sleep(100000)
  }

  static def displayInFrame(columns = -1, rows = -1, Closure closure) {
    def frame = new JFrame("")
    frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
    def layout = new GridLayout()
    if (columns != -1) layout.columns = columns
    if (rows != -1) layout.rows = rows
    frame.contentPane.layout = layout

    closure.delegate = frame
    closure.call()

    frame.pack()
    frame.visible = true
  }
}
