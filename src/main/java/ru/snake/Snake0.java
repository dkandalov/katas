package ru.snake;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

import static java.util.Arrays.asList;

public class Snake0 {
    private static GameState gameState;

    public static void main(String[] args) {
        gameState = new GameState(Direction.right, asList(
                new Point(3, 1), new Point(2, 1), new Point(1, 1)
        ), asList(
                new Point(5, 5)
        ));
        GameTimer gameTimer = new GameTimer(i ->
            gameState = gameState.onTimer()
        ).init();
        GameUI gameUI = new GameUI().init(direction ->
            gameState = gameState.onDirection(direction)
        );

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

    private enum Direction {
        up, right, down, left
    }

    private static class GameState {
        final Direction snakeDirection;
        final List<Point> snake;
        final List<Point> apples;

        public GameState(Direction snakeDirection, List<Point> snake, List<Point> apples) {
            this.snakeDirection = snakeDirection;
            this.snake = snake;
            this.apples = apples;
        }

        public GameState onTimer() {
            List<Point> newSnake = new ArrayList<>(snake);
            newSnake.remove(newSnake.size() - 1);
            Point head = newSnake.get(0);

            Point newHead;
            if (snakeDirection == Direction.up) {
                newHead = new Point(head.x, head.y - 1);
            } else if (snakeDirection == Direction.right) {
                newHead = new Point(head.x + 1, head.y);
            } else if (snakeDirection == Direction.down) {
                newHead = new Point(head.x, head.y + 1);
            } else if (snakeDirection == Direction.left) {
                newHead = new Point(head.x - 1, head.y);
            } else {
                throw new IllegalStateException();
            }
            newSnake.add(0, newHead);

            return new GameState(snakeDirection, newSnake, apples);
        }

        public GameState onDirection(Direction newDirection) {
            List<Point> newSnake = new ArrayList<>(snake);
            boolean isHorizontalReverse = isHorizontal(snake) &&
                    ((snakeDirection == Direction.left && newDirection == Direction.right) ||
                    (snakeDirection == Direction.right && newDirection == Direction.left));
            boolean isVerticalReverse = isVertical(snake) &&
                    ((snakeDirection == Direction.up && newDirection == Direction.down) ||
                    (snakeDirection == Direction.down && newDirection == Direction.up));
            if (isHorizontalReverse || isVerticalReverse)  {
                Collections.reverse(newSnake);
            }
            return new GameState(newDirection, newSnake, apples);
        }

        private static boolean isVertical(List<Point> snake) {
            return snake.stream().allMatch(it -> it.getX() == snake.get(0).getX());
        }

        private static boolean isHorizontal(List<Point> snake) {
            return snake.stream().allMatch(it -> it.getY() == snake.get(0).getY());
        }
    }

    private static class GameUI {
        private GamePanel gamePanel;

        public GameUI init(Consumer<Direction> directionConsumer) {
            gamePanel = new GamePanel();

            JFrame jFrame = new JFrame("Snake");
            jFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            jFrame.addKeyListener(new KeyAdapter() {
                @Override public void keyPressed(KeyEvent event) {
                    Direction direction = null;
                    int keyCode = event.getKeyCode();
                    if (keyCode == KeyEvent.VK_UP) {
                        direction = Direction.up;
                    } else if (keyCode == KeyEvent.VK_RIGHT) {
                        direction = Direction.right;
                    } else if (keyCode == KeyEvent.VK_DOWN) {
                        direction = Direction.down;
                    } else if (keyCode == KeyEvent.VK_LEFT) {
                        direction = Direction.left;
                    }
                    if (direction != null) {
                        directionConsumer.accept(direction);
                    }
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

    private static class GameTimer {
        private final Timer timer;

        public GameTimer(Consumer<Void> callback) {
            this.timer = new Timer(500, e -> callback.accept(null));
        }

        public GameTimer init() {
            timer.start();
            return this;
        }
    }
}
