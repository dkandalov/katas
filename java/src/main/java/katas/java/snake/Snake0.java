package katas.java.snake;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static katas.java.snake.Snake0.GameState.Status.gameOver;
import static katas.java.snake.Snake0.GameState.Status.playing;

public class Snake0 {
    private static GameState gameState;

    public static void main(String[] args) {
        List<Point> snake = asList(
                new Point(4, 1), new Point(3, 1), new Point(2, 1), new Point(1, 1)
        );
        List<Point> apples = asList(
                new Point(5, 5)
        );
        gameState = new GameState(Direction.right, snake, apples, playing);

        GameUI gameUI = new GameUI();
        gameUI.init(direction -> {
            gameState = gameState.onDirectionChange(direction);
            gameUI.repaint(gameState);
        });
        new GameTimer(i -> {
            gameState = gameState.onTimer();
            gameUI.repaint(gameState);
        }).init();
        gameUI.repaint(gameState);
    }

    private enum Direction {
        up, right, down, left
    }

    static class GameState {
        private final Direction snakeDirection;
        private final List<Point> snake;
        private final List<Point> apples;
        private final Status status;
        private final boolean skipNextTimerTick;

        GameState(Direction snakeDirection, List<Point> snake, List<Point> apples, Status status) {
            this(snakeDirection, snake, apples, status, false);
        }

        GameState(Direction snakeDirection, List<Point> snake, List<Point> apples, Status status, boolean skipNextTimerTick) {
            this.snakeDirection = snakeDirection;
            this.snake = snake;
            this.apples = apples;
            this.status = status;
            this.skipNextTimerTick = skipNextTimerTick;
        }

        GameState skipNextTimerTick(boolean value) {
            return new GameState(snakeDirection, snake, apples, status, value);
        }

        GameState onTimer() {
            if (skipNextTimerTick) return skipNextTimerTick(false);
            if (status != playing) return this;

            List<Point> newSnake = new ArrayList<>(snake);
            Point head = newSnake.get(0);
            Point newHead = snakeHeadAfterMove(head);

            newSnake.add(0, newHead);

            List<Point> newApples = apples.stream().filter(it -> !it.equals(newHead)).collect(toList());
            if (newApples.size() == apples.size()) {
                newSnake.remove(newSnake.size() - 1);
            }

            boolean hasBittenItself = newSnake.stream().filter(it -> it.equals(newHead)).count() > 1;
            if (hasBittenItself) {
                return new GameState(snakeDirection, newSnake, apples, gameOver);
            }

            return new GameState(snakeDirection, newSnake, newApples, status);
        }

        @NotNull private Point snakeHeadAfterMove(Point head) {
            if (snakeDirection == Direction.up) return new Point(head.x, head.y - 1);
            else if (snakeDirection == Direction.right) return new Point(head.x + 1, head.y);
            else if (snakeDirection == Direction.down) return new Point(head.x, head.y + 1);
            else if (snakeDirection == Direction.left) return new Point(head.x - 1, head.y);
            else throw new IllegalStateException();
        }

        GameState onDirectionChange(Direction newDirection) {
            if (status != playing) return this;

            List<Point> newSnake = new ArrayList<>(snake);
            Point head = newSnake.get(0);
            Point tailTip = newSnake.get(newSnake.size() - 1);

            boolean isHorizontalReverse = isHorizontal(snake) &&
                    ((newDirection == Direction.right && head.x < tailTip.x) ||
                    (newDirection == Direction.left && head.x > tailTip.x));
            boolean isVerticalReverse = isVertical(snake) &&
                    ((newDirection == Direction.down && head.y < tailTip.y) ||
                    (newDirection == Direction.up && head.y > tailTip.y));
            if (isHorizontalReverse || isVerticalReverse)  {
                Collections.reverse(newSnake);
            }
            return new GameState(newDirection, newSnake, apples, status).onTimer().skipNextTimerTick(true);
        }

        private static boolean isVertical(List<Point> snake) {
            return snake.stream().allMatch(it -> it.x == snake.get(0).x);
        }

        private static boolean isHorizontal(List<Point> snake) {
            return snake.stream().allMatch(it -> it.y == snake.get(0).y);
        }

        public enum Status {
            playing, gameOver
        }
    }

    private static class GameUI {
        private GamePanel gamePanel;

        void init(Consumer<Direction> directionConsumer) {
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

        }

        void repaint(GameState gameState) {
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

        void repaintState(GameState gameState) {
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

            if (gameState.status == gameOver) {
                g.drawString("Game Over!", 100, 100);
            }
        }
    }

    private static class GameTimer {
        private final Timer timer;

        GameTimer(Consumer<Void> callback) {
            this.timer = new Timer(500, e -> callback.accept(null));
        }

        public GameTimer init() {
            timer.start();
            return this;
        }
    }
}
