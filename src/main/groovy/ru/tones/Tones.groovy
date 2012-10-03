package ru.tones

import org.joda.time.DateTime

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.SourceDataLine
import javax.swing.*
import java.awt.GridLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.WindowEvent

import org.junit.Test

class Tones {
  static void main(String[] args) {
    def model = new Model()

    System.addShutdownHook { model.onClose() }

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
    final List<Integer> allFrequencies = []
    private int lastFrequency = 0

    private int roundsCount = 0
    private double score = 0
    private int attempts = 0

    Model() {
      50.step(501, 50) { allFrequencies << it }
      allFrequencies << 750
      1000.step(5001, 500) { allFrequencies << it }
      6000.step(10001, 2000) { allFrequencies << it }
    }

    def guessed(int frequency) {
      attempts++
      if (lastFrequency == frequency) {
        updateScore()
        JOptionPane.showMessageDialog(null, "Yes! It was ${lastFrequency.toString()} Hz")
      }
    }

    void playNextFrequency() {
      def n = new Random().nextInt(allFrequencies.size())
      int frequency = allFrequencies[n]
      lastFrequency = frequency

      updateScore()
      roundsCount++

      new Thread({
        sound(frequency, 3000, 0.8)
      }).start()
    }

    private void updateScore() {
      if (attempts > 0) score += 1 / attempts
      attemps = 0
    }

    def onClose() {
      if (roundsCount == 0) return

      def now = new DateTime()
      def date = now.toString("dd/MM/yyyy")
      def time = now.toString("hh:mm")

      new File("tones_stats.csv").append("$date,$time,$roundsCount,${score / roundsCount}\n")
    }
  }

  @Test public void aaa() {
    println(new DateTime().toString("dd/MM/yyyy"))
    println(new DateTime().toString("hh:mm"))
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

  public static void sound(int hz, int msecs, double vol) throws LineUnavailableException {
    float SAMPLE_RATE = 8000f;

    if (hz <= 0) throw new IllegalArgumentException("Frequency <= 0 hz");
    if (msecs <= 0) throw new IllegalArgumentException("Duration <= 0 msecs");
    if (vol > 1.0 || vol < 0.0) throw new IllegalArgumentException("Volume out of range 0.0 - 1.0");

    byte[] buf = new byte[(int) SAMPLE_RATE * msecs / 1000];

    for (int i = 0; i < buf.length; i++) {
      double angle = i / (SAMPLE_RATE / hz) * 2.0 * Math.PI;
      buf[i] = (byte) (Math.sin(angle) * 127.0 * vol);
    }

    // shape the front and back 10ms of the wave form
    for (int i = 0; i < SAMPLE_RATE / 100.0 && i < buf.length / 2; i++) {
      buf[i] = (byte) (buf[i] * i / (SAMPLE_RATE / 100.0));
      buf[buf.length - 1 - i] = (byte) (buf[buf.length - 1 - i] * i / (SAMPLE_RATE / 100.0));
    }

    AudioFormat audioFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, false);
    SourceDataLine line = AudioSystem.getSourceDataLine(audioFormat);
    line.open(audioFormat);
    line.start();
    line.write(buf, 0, buf.length);
    line.drain();
    line.close();
  }
}
