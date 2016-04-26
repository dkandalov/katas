package ru.snake;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class Snake0 {
    public static void main(String[] args) {
        GamePanel gamePanel = new GamePanel();

        JFrame jFrame = new JFrame("Snake");
        jFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        jFrame.addKeyListener(new KeyAdapter() {
            @Override public void keyPressed(KeyEvent e) {
                gamePanel.i -= 2;
                gamePanel.repaint();
            }
        });
        jFrame.add(gamePanel);
        jFrame.pack();
        jFrame.setLocationRelativeTo(null);
        jFrame.setVisible(true);

        new Thread(() -> {
            while (true) {
                gamePanel.repaint();
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }).start();
    }

    private static class GamePanel extends JPanel {
        @Override public Dimension getPreferredSize() {
            return new Dimension(800, 800);
        }
        int i = 0;
        @Override protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            g.setColor(Color.green);
            g.fillRect(i + 0, i +0, i+100, i+100);
            i++;
        }
    }
}
