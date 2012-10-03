package ru.tones

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.SourceDataLine
import javax.swing.*
import java.awt.GridLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener

class Tones {
  static void main(String[] args) {
    def frame = new JFrame("tones")
    def panel = new JPanel()
    panel.layout = new GridLayout(4, 4)
    panel.with {
      add(new NextToneButton())

      50.step(501, 50) {
        add(new GuessToneButton(it))
      }
      add(new JButton("750"))
      1000.step(5001, 500) {
        add(new JButton(it.toString()))
      }
    }
    frame.add(panel)
    frame.pack()
    frame.visible = true
  }

  private static class FrequencySource {

  }

  int lastFrequency = 0

  private static class GuessToneButton extends JButton {
    GuessToneButton(int frequency) {
      text = frequency.toString()

      addActionListener(new AbstractAction() {
        @Override void actionPerformed(ActionEvent e) {

        }
      })
    }
  }

  private static class NextToneButton extends JButton {
    NextToneButton() {
      text = "Play"
      addActionListener(new ActionListener() {
        @Override void actionPerformed(ActionEvent e) {
          new Thread({
            int frequency = new Random().nextInt()
            sound(frequency, 3000, 0.8)
            lastFrequency = frequency
          }).start()
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
