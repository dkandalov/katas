package ru.snake;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.List;

import static java.util.Arrays.asList;

public class Snake0 {
    public static void main(String[] args) {
        GameState gameState = new GameState(asList(
                new Point(1, 1), new Point(2, 1), new Point(3, 1)
        ), asList(
                new Point(5,5)
        ));
        GameUI gameUI = new GameUI().init();

        new Thread(() -> {
            while (true) {
                gameUI.onUpdate(gameState);
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace(); // TODO
                }
            }
        }).start();
    }

    private static class GameState {
        final List<Point> snake;
        final List<Point> apples;

        public GameState(List<Point> snake, List<Point> apples) {
            this.snake = snake;
            this.apples = apples;
        }
    }

    private static class GameUI {
        private GamePanel gamePanel;

        public GameUI init() {
            gamePanel = new GamePanel();

            JFrame jFrame = new JFrame("Snake");
            jFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            jFrame.addKeyListener(new KeyAdapter() {
                @Override public void keyPressed(KeyEvent e) {
                    gamePanel.repaint();
                }
            });
            jFrame.add(gamePanel);
            jFrame.pack();
            jFrame.setLocationRelativeTo(null);
            jFrame.setVisible(true);

            return this;
        }

        public void onUpdate(GameState gameState) {
            SwingUtilities.invokeLater(() ->
                    gamePanel.repaintState(gameState)
            );
        }
    }

    private static class GamePanel extends JPanel {
        private GameState gameState;

        @Override public Dimension getPreferredSize() {
            return new Dimension(800, 800);
        }

        public void repaintState(GameState gameState) {
            this.gameState = gameState;
            repaint();
        }

        @Override protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            if (gameState == null) return;

            int cellWidth = 50;
            int cellHeight = 50;
            int xPad = 5;
            int yPad = 5;

            g.setColor(Color.blue);
            for (Point point : gameState.snake) {
                g.fillRect(
                    point.x * cellWidth,
                    point.y * cellHeight,
                    cellWidth - xPad,
                    cellHeight - yPad
                );
            }
            g.setColor(Color.red);
            for (Point point : gameState.apples) {
                g.fillRect(
                    point.x * cellWidth,
                    point.y * cellHeight,
                    cellWidth - xPad,
                    cellHeight - yPad
                );
            }
        }
    }
}
