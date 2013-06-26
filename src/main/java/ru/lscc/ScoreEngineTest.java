package ru.lscc;

import org.junit.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static ru.lscc.ScoreEngineTest.ScoreEngine.PLAYER1;
import static ru.lscc.ScoreEngineTest.ScoreEngine.PLAYER2;


public class ScoreEngineTest {

    private final Display display = mock(Display.class);
    private final ScoreEngine engine = new ScoreEngine(display);

    @Test public void whenGameStarts_shouldShowScore() {
        engine.gameHasStarted(PLAYER1);

        verify(display).showScore(PLAYER1, 0, PLAYER2, 0);
        verify(display).showServer(PLAYER1);
    }

    @Test public void whenServingPlayerScores_shouldUpdateScore() {
        engine.gameHasStarted(PLAYER1);
        engine.update(PLAYER1);

        verify(display).showScore(PLAYER1, 0, PLAYER2, 0);
        verify(display).showScore(PLAYER1, 1, PLAYER2, 0);
        verify(display, times(2)).showServer(PLAYER1);
    }

    @Test public void whenNonServingPlayerWins_shouldNotUpdateScore() {
        engine.gameHasStarted(PLAYER1);
        verify(display).showScore(PLAYER1, 0, PLAYER2, 0);
        verify(display).showServer(PLAYER1);

        engine.update(PLAYER2);
        verify(display).showServer(PLAYER2);
    }

    @Test public void whenPlayerHasGamePoint_shouldShowWinner() {
        engine.gameHasStarted(PLAYER1);
        verify(display).showScore(PLAYER1, 0, PLAYER2, 0);
        verify(display).showServer(PLAYER1);

        engine.update(PLAYER2);
    }

    public static class ScoreEngine {
        public static final String PLAYER1 = "Player1";
        public static final String PLAYER2 = "Player2";

        private final Display display;
        private String servingPlayer;
        private int player1Score;
        private int player2Score;

        public ScoreEngine(Display display) {
            this.display = display;
        }

        public void gameHasStarted(String servingPlayer) {
            this.servingPlayer = servingPlayer;
            player1Score = 0;
            player2Score = 0;

            display.showScore(PLAYER1, player1Score, PLAYER2, player2Score);
            display.showServer(servingPlayer);
        }

        public void update(String playerThatWon) {
            if (playerThatWon.equals(servingPlayer)) {
                if (playerThatWon.equals(PLAYER1)) {
                    player1Score++;
                } else {
                    player2Score++;
                }
            }
            this.servingPlayer = playerThatWon;

            display.showScore(PLAYER1, player1Score, PLAYER2, player2Score);
            display.showServer(servingPlayer);
        }
    }

    private interface Display {
        void showScore(String player1, int score1, String player2, int score2);

        void showServer(String player);
    }
}
