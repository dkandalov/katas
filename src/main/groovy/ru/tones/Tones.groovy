package ru.tones
import org.joda.time.DateTime

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.LineUnavailableException
import javax.swing.*
import java.awt.*
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.List

class Tones {
  static void main(String[] args) {
    def model = new Model(new RealWorld())

    System.addShutdownHook { model.onExit() }

    def frame = new JFrame("tones")
    def panel = new JPanel()
    panel.layout = new GridLayout(4, 4)
    panel.with {
      add(new NextToneButton(model))
      model.allFrequencies.each {
        add(new GuessToneButton(it, model))
      }
    }
    frame.add(panel)
    frame.pack()
    frame.visible = true
  }

  private static class Model {
    private final RealWorld realWorld

    final List<Integer> allFrequencies = []
    private int lastFrequency = 0
    private int attempts = 0

    Model(RealWorld realWorld) {
      this.realWorld = realWorld

      50.step(501, 50) { allFrequencies << it }
      allFrequencies << 750
      1000.step(5001, 500) { allFrequencies << it }
      6000.step(10001, 2000) { allFrequencies << it }
    }

    def guessed(int frequency) {
      attempts++
      if (lastFrequency == frequency) {
        saveScore()
        realWorld.showMessage("Yes! It was ${lastFrequency.toString()} Hz")
      }
    }

    def playNextFrequency() {
      if (attempts > 0) saveZeroScore()

      int frequency = nextFrequency()
      lastFrequency = frequency
      realWorld.playSound(frequency)
    }

    def onExit() {
      if (attempts > 0) saveZeroScore()
    }

    private int nextFrequency() {
      def n = new Random().nextInt(allFrequencies.size())
      int frequency = allFrequencies[n]
      frequency
    }

    private saveZeroScore() {
      saveStats(0)
    }

    private void saveScore() {
      saveStats(1 / attempts)
      attempts = 0
    }

    private def saveStats(score) {
      def now = new DateTime()
      def date = now.toString("dd/MM/yyyy")
      def time = now.toString("hh:mm:ss")

      realWorld.saveStats("$date,$time,$score\n")
    }
  }

  private static class GuessToneButton extends JButton {
    GuessToneButton(int frequency, Model model) {
      text = frequency.toString()
      addActionListener(new AbstractAction() {
        @Override void actionPerformed(ActionEvent e) {
          model.guessed(frequency)
        }
      })
    }
  }

  private static class NextToneButton extends JButton {
    NextToneButton(Model model) {
      text = "Play"
      addActionListener(new ActionListener() {
        @Override void actionPerformed(ActionEvent e) {
          model.playNextFrequency()
        }
      })
    }
  }

  private static class RealWorld {
    def showMessage(String text) {
      JOptionPane.showMessageDialog(null, text)
    }

    def playSound(int frequency) {
      new Thread({
        sound(frequency, 3000, 0.8)
      }).start()
    }

    def saveStats(String text) {
      new File("tones_stats.csv").append(text)
    }

    private static void sound(int hz, int msecs, double volume) throws LineUnavailableException {
      if (hz <= 0) throw new IllegalArgumentException("Frequency <= 0 hz")
      if (msecs <= 0) throw new IllegalArgumentException("Duration <= 0 msecs")
      if (volume > 1 || volume < 0) throw new IllegalArgumentException("Volume out of range 0.0 - 1.0")

      double sampleRate = 8000
      double samplesPerCycle = sampleRate / hz

      byte[] buf = new byte[(int) sampleRate * (msecs / 1000)]

      for (int i = 0; i < buf.length; i++) {
        double angle = (i / samplesPerCycle) * 2 * Math.PI
        buf[i] = (byte) (Math.sin(angle) * 127 * volume)
      }

      // shape the front and back 10ms of the wave form
      for (int i = 0; i < sampleRate / 100 && i < buf.length / 2; i++) {
        buf[i] = (byte) (buf[i] * i / (sampleRate / 100))
        buf[buf.length - 1 - i] = (byte) (buf[buf.length - 1 - i] * i / (sampleRate / 100))
      }

      AudioFormat audioFormat = new AudioFormat((float) sampleRate, 8, 1, true, false)
      AudioSystem.getSourceDataLine(audioFormat).with {
        open(audioFormat)
        start()
        write(buf, 0, buf.length)
        drain()
        close()
      }
    }
  }
}
