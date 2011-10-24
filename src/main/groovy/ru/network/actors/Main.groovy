package ru.network.actors

import java.awt.GridLayout
import javax.swing.JFrame
import ru.network.actors.ui.TradeFormActor

/**
 * User: dima
 * Date: 21/10/2011
 */
class Main {
  static void main(String[] args) {
    def bus = new Bus()
//    new BusListener(bus)
    displayInFrame {
      add(TradeFormActor.createForm(bus))
    }

    new DataFeed(bus, "datafeed-1")
    new DataFeed(bus, "datafeed-2", 2000)

    Thread.sleep(100000)
  }

  static def displayInFrame(Closure closure) {
    def frame = new JFrame("")
    frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
    frame.contentPane.layout = new GridLayout()

    closure.delegate = frame
    closure.call()

    frame.pack()
    frame.visible = true
  }
}
