package katas.kotlin.snake.v0_refactored

import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.Graphics
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.KeyEvent.*
import java.awt.event.WindowEvent
import javax.swing.*


fun main() {
    val gameUI = GameSwingUI()
    gameUI.init(object: GameUI.Observer {
        lateinit var game: Game

        override fun onGameStart() {
            game = Game.create(50, 50)
            gameUI.paint(game)
        }

        override fun onTimer() {
            if (game.state != Game.State.Playing) return
            game = game.updateOnTimer()
            gameUI.paint(game)
        }

        override fun onUserInput(direction: Direction) {
            if (game.state != Game.State.Playing) return
            game = game.updateOnUserInput(direction)
            gameUI.paint(game)
        }
    })
}


interface GameUI {
    fun init(observer: Observer)
    fun paint(game: Game)

    interface Observer {
        fun onGameStart()
        fun onTimer()
        fun onUserInput(direction: Direction)
    }
}


class GameSwingUI: GameUI {
    private lateinit var gamePanel: GamePanel
    private lateinit var jFrame: JFrame
    private lateinit var timer: GameTimer

    override fun init(observer: GameUI.Observer) {
        gamePanel = GamePanel()
        timer = GameTimer {
            SwingUtilities.invokeLater {
                observer.onTimer()
            }
        }.init()

        jFrame = JFrame("Snake")
        jFrame.apply {
            defaultCloseOperation = WindowConstants.EXIT_ON_CLOSE
            addKeyListener(object: KeyAdapter() {
                override fun keyPressed(event: KeyEvent) {
                    val direction = when (event.keyCode) {
                        VK_UP, VK_I -> Direction.Up
                        VK_RIGHT, VK_L -> Direction.Right
                        VK_DOWN, VK_K -> Direction.Down
                        VK_LEFT, VK_J -> Direction.Left
                        else -> null
                    }
                    if (direction != null) {
                        observer.onUserInput(direction)
                    }
                    if (event.keyCode == VK_Q) {
                        dispatchEvent(WindowEvent(jFrame, WindowEvent.WINDOW_CLOSING))
                    }
                    if (event.keyCode == VK_S) {
                        observer.onGameStart()
                    }
                }
            })
            add(gamePanel)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }

        SwingUtilities.invokeLater {
            observer.onGameStart()
        }
    }

    override fun paint(game: Game) {
        gamePanel.repaintState(game)
    }
}


private class GameTimer(delay: Int = 500, callback: () -> Unit) {
    private val timer = Timer(delay) { callback() }

    fun init() = this.apply {
        timer.start()
    }
}

private class GamePanel: JPanel() {
    private var game: Game? = null
    private val messageFont = Font("DejaVu Sans", Font.BOLD, 35)

    fun repaintState(game: Game) {
        this.game = game
        repaint()
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        game?.let { game ->
            val cellWidth = width / game.width
            val cellHeight = height / game.height
            val xPad = cellWidth / 10
            val yPad = cellHeight / 10

            g.color = Color.blue
            for ((x, y) in game.snake.body) {
                g.fillRect(
                    x * cellWidth,
                    y * cellHeight,
                    cellWidth - xPad,
                    cellHeight - yPad
                )
            }
            g.color = Color.red
            for ((x, y) in game.apples) {
                g.fillRect(
                    x * cellWidth,
                    y * cellHeight,
                    cellWidth - xPad,
                    cellHeight - yPad
                )
            }

            if (game.state != Game.State.Playing) {
                val message = "Game Over!"
                g.font = messageFont
                val textWidth = g.fontMetrics.stringWidth(message)
                val textHeight = g.fontMetrics.height
                g.drawString(message, width / 2 - textWidth / 2, height / 2 - textHeight / 2)
            }
        }
    }

    override fun getPreferredSize() = Dimension(800, 800)
}
