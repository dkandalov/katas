package ru.network.actors.ui

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import javax.swing.DefaultListModel
import javax.swing.JPanel
import javax.swing.SwingUtilities
import ru.network.actors.Bus
import static com.cmcmarkets.Util.runSafely

/**
 * User: dima
 * Date: 24/10/2011
 */
@ActiveObject
class TradeFormActor {
  private final DefaultListModel listModel

  static JPanel createForm(Bus bus) {
    def tradeForm = new TradeForm()
    def model = new DefaultListModel()
    tradeForm.list.model = model
    new TradeFormActor(bus, model)

    tradeForm.root
  }

  TradeFormActor(Bus bus, DefaultListModel listModel) {
    this.listModel = listModel
    bus.addListener(this)
  }

  @ActiveMethod
  def onMessage(message) {
    handle(message)
  }

  private def handle(message) {
    runSafely {
      println message
      SwingUtilities.invokeLater {
        listModel.addElement(message)
      }
    }
  }
}
