package ru.network.actors.ui

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import javax.swing.DefaultListModel
import javax.swing.JPanel
import ru.network.actors.Bus
import static com.cmcmarkets.Util.runSafely
import static javax.swing.SwingUtilities.invokeLater

/**
 * User: dima
 * Date: 24/10/2011
 */
@ActiveObject
class BusListeningForm {
  private final DefaultListModel listModel
  private final Closure acceptMessage

  static JPanel createForm(String title, Bus bus, Closure acceptMessage = {true}) {
    def form = new MessagesForm_()
    form.root.border.title = title
    def model = new DefaultListModel()
    form.list.model = model

    bus.addListener(new BusListeningForm(model, acceptMessage))

    form.root
  }

  BusListeningForm(DefaultListModel listModel, Closure acceptMessage) {
    this.listModel = listModel
    this.acceptMessage = acceptMessage
  }

  @ActiveMethod
  def onMessage(message) {
    handle(message)
  }

  private def handle(message) {
    runSafely {
      if (!acceptMessage(message)) return
      invokeLater {
        listModel.addElement(message)
      }
    }
  }
}
