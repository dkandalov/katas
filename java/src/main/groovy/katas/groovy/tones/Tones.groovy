package katas.groovy.tones
import org.joda.time.DateTime
import org.junit.Test

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
    def world = new RealWorld()
    def model = new Model(world)

    System.addShutdownHook { model.onExit() }

    new JFrame("guess tones").with {
      def panel = new JPanel()
      panel.layout = new GridLayout(4, 4)
      panel.add(new NextToneButton(model))
      model.allFrequencies.each { panel.add(new GuessToneButton(it, model)) }
      add(panel)
      pack()
      visible = true
    }

    new JFrame("play tones").with {
      def panel = new JPanel()
      panel.layout = new GridLayout(4, 4)
      panel.add(new JButton())
      model.allFrequencies.each { panel.add(new PlayToneButton(it, world)) }
      add(panel)
      pack()
      setLocation((int) location.x, (int) location.y + height + 30)
      visible = true
    }
  }

  private static class Model {
    private final RealWorld realWorld

    final List<Integer> allFrequencies = []
    private int lastFrequency = 0
    private int attempts = 0

    Model(RealWorld realWorld) {
      this.realWorld = realWorld

/*
      200.step(501, 50) { allFrequencies << it }
      allFrequencies << 750
      1000.step(5001, 500) { allFrequencies << it }
      6000.step(10001, 2000) { allFrequencies << it }
      allFrequencies << 15000
*/
      allFrequencies = [
              20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500, 630, 800,
              1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000, 12500, 16000
      ]
    }

    def guessed(int frequency) {
      attempts++
      if (lastFrequency == frequency) {
        saveScore()
//        realWorld.playSound(lastFrequency)
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

  private static class PlayToneButton extends JButton {
    PlayToneButton(int frequency, RealWorld world) {
      text = frequency.toString()
      addActionListener(new AbstractAction() {
        @Override void actionPerformed(ActionEvent e) {
          world.playSound(frequency)
        }
      })
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

    private static void sound(double hz, int msecs, double volume) throws LineUnavailableException {
      if (hz <= 0) throw new IllegalArgumentException("Frequency <= 0 hz")
      if (msecs <= 0) throw new IllegalArgumentException("Duration <= 0 msecs")
      if (volume > 1 || volume < 0) throw new IllegalArgumentException("Volume out of range 0.0 - 1.0")

      double sampleRate = 320000
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

  @Test void whatFrequenciesReallySoundLike() {
    RealWorld.sound(20, 1000, 0.7)
    RealWorld.sound(30, 1000, 0.7)
    RealWorld.sound(40, 1000, 0.7)
    RealWorld.sound(50, 1000, 0.7)
  }
}
