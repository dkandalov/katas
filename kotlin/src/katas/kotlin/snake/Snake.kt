package katas.kotlin.snake

import katas.kotlin.shouldEqual
import katas.kotlin.snake.Direction.*
import katas.kotlin.snake.Game.State.*
import org.junit.Test
import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.Graphics
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.WindowEvent
import java.util.*
import javax.swing.*
import javax.swing.Timer

class SnakeTests {
    @Test fun `horizontal snake moves`() {
        // Xxx
        Snake(Point(2, 2), Point(3, 2), Point(4, 2)).apply {
            move(Left).body shouldEqual listOf(Point(1, 2), Point(2, 2), Point(3, 2))
            move(Right).body shouldEqual listOf(Point(5, 2), Point(4, 2), Point(3, 2))

            // xx
            // X
            move(Down).body shouldEqual listOf(Point(2, 3), Point(2, 2), Point(3, 2))

            // X
            // xx
            move(Up).body shouldEqual listOf(Point(2, 1), Point(2, 2), Point(3, 2))
        }
    }

    @Test fun `vertical snake moves`() {
        // X
        // x
        // x
        Snake(Point(2, 2), Point(2, 3), Point(2, 4)).apply {
            move(Up) shouldEqual Snake(Point(2, 1), Point(2, 2), Point(2, 3))
            move(Down) shouldEqual Snake(Point(2, 5), Point(2, 4), Point(2, 3))

            // Xx
            //  x
            move(Left) shouldEqual Snake(Point(1, 2), Point(2, 2), Point(2, 3))

            // xX
            // x
            move(Right) shouldEqual Snake(Point(3, 2), Point(2, 2), Point(2, 3))
        }
    }

    @Test fun `shaped snake moves`() {
        // X
        // xx
        //  x
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 3)).apply {
            // X
            // x
            // xx
            move(Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            // xx
            //  x
            //  X
            move(Down) shouldEqual Snake(Point(2, 4), Point(2, 3), Point(2, 2), Point(1, 2))
        }

        // Xx
        // xx
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 1)).apply {
            // X
            // x
            // xx
            move(Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            //  X
            //  x
            // xx
            move(Down) shouldEqual Snake(Point(2, 0), Point(2, 1), Point(2, 2), Point(1, 2))
        }
    }
}

class GameTests {
    @Test fun `game tracks when snake hits wall`() {
        var game = Game(width = 5, height = 5, snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)))

        game = game.updateOnTimer()
        game.apply {
            state shouldEqual Playing
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2))
        }

        game = game.updateOnTimer()
        game.apply {
            state shouldEqual SnakeHitWall
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2))
        }
    }

    @Test fun `game tracks when snake bites itself`() {
        val game = Game(width = 5, height = 5,
            // xxx
            // xX
            // xx
            snake = Snake(Point(1, 1), Point(1, 2), Point(0, 2), Point(0, 1), Point(0, 0), Point(1, 0), Point(2, 0))
        )

        game.updateOnTimer().apply {
            state shouldEqual SnakeBitItself
        }
    }

    @Test fun `snake grows after eating an apple`() {
        val game = Game(width = 5, height = 5,
            snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)),
            apples = listOf(Point(0, 2))
        )

        game.updateOnTimer().apply {
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2), Point(3, 2))
            apples shouldEqual emptyList()
        }
    }

    @Test fun `new apples appear on timer update`() {
        var game = Game(width = 5, height = 5,
            snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)),
            apples = emptyList(),
            appleFactory = AppleFactory(Random(123))
        )

        game = game.updateOnTimer()
        game.apply {
            apples shouldEqual listOf(Point(2, 0))
        }
    }

    @Test fun `snake moves on user input`() {
        val game = Game(width = 5, height = 5, snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)))
        game.updateOnUserInput(Down).apply {
            snake shouldEqual Snake(Point(1, 3), Point(1, 2), Point(2, 2))
        }
    }
}

fun main(args: Array<String>) {
    val gameUI = GameSwingUI()
    gameUI.init(object : GameUI.Listener {
        lateinit var game: Game

        override fun onGameStart() {
            game = Game.create(50, 50)
            gameUI.paint(game)
        }

        override fun onTimer() {
            if (game.state != Playing) return
            game = game.updateOnTimer()
            gameUI.paint(game)
        }

        override fun onUserDirection(direction: Direction) {
            if (game.state != Playing) return
            game = game.updateOnUserInput(direction)
            gameUI.paint(game)
        }
    })
}

interface GameUI {
    fun init(listener: Listener)
    fun paint(game: Game)

    interface Listener {
        fun onTimer()
        fun onUserDirection(direction: Direction)
        fun onGameStart()
    }
}

class GameSwingUI : GameUI {
    private lateinit var gamePanel: GamePanel
    private lateinit var jFrame: JFrame
    private lateinit var timer: GameTimer

    override fun init(listener: GameUI.Listener) {
        gamePanel = GamePanel()
        timer = GameTimer {
            SwingUtilities.invokeLater {
                listener.onTimer()
            }
        }.init()

        jFrame = JFrame("Snake")
        jFrame.apply {
            defaultCloseOperation = WindowConstants.EXIT_ON_CLOSE
            addKeyListener(object : KeyAdapter() {
                override fun keyPressed(event: KeyEvent) {
                    val direction = when (event.keyCode) {
                        KeyEvent.VK_UP -> Direction.Up
                        KeyEvent.VK_RIGHT -> Direction.Right
                        KeyEvent.VK_DOWN -> Direction.Down
                        KeyEvent.VK_LEFT -> Direction.Left
                        else -> null
                    }
                    if (direction != null) {
                        listener.onUserDirection(direction)
                    }
                    if (event.keyCode == KeyEvent.VK_Q) {
                        dispatchEvent(WindowEvent(jFrame, WindowEvent.WINDOW_CLOSING))
                    }
                    if (event.keyCode == KeyEvent.VK_S) {
                        listener.onGameStart()
                    }
                }
            })
            add(gamePanel)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }

        SwingUtilities.invokeLater {
            listener.onGameStart()
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

private class GamePanel : JPanel() {
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

            if (game.state != Playing) {
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


interface AppleFactory {
    fun produceApples(game: Game): List<Point>

    companion object {
        val noop = object : AppleFactory {
            override fun produceApples(game: Game) = game.apples
        }

        operator fun invoke(random: Random = Random()) = object : AppleFactory {
            override fun produceApples(game: Game): List<Point> = game.run {
                if (apples.size + snake.body.size == width * height) return apples

                val p = Point(random.nextInt(width), random.nextInt(height))
                return if (p !in apples && p !in snake.body) apples + p else produceApples(this)
            }
        }
    }
}


data class Game(val width: Int, val height: Int, val state: State = Playing, val snake: Snake,
                val apples: List<Point> = emptyList(), val appleFactory: AppleFactory = AppleFactory.noop) {

    fun updateOnTimer() = update(snake.direction, appleFactory)

    fun updateOnUserInput(direction: Direction) = update(direction, AppleFactory.noop)

    private fun update(direction: Direction, appleFactory: AppleFactory): Game {
        var newApples = appleFactory.produceApples(this)
        var newSnake = snake.move(direction)
        if (newSnake.body.first() in apples) {
            newSnake = newSnake.copy(body = newSnake.body + snake.body.last())
            newApples = newApples.filter { it != newSnake.body.first() }
        }
        return if (newSnake.outsideOf(width, height)) copy(state = SnakeHitWall)
        else if (newSnake.bitItself) copy(state = SnakeBitItself)
        else copy(state = Playing, snake = newSnake, apples = newApples)
    }

    private fun Snake.outsideOf(width: Int, height: Int) = body.any{ it.x < 0 || it.x >= width || it.y < 0 || it.y >= height }

    enum class State {
        Playing, SnakeHitWall, SnakeBitItself
    }

    companion object {
        fun create(width: Int, height: Int, snakeLength: Int = 4): Game {
            val x = (width / 2) - snakeLength
            val y = height / 2
            val snake = Snake(0.until(snakeLength).map{ Point(x + it, y) })
            return Game(width, height, Playing, snake, emptyList(), AppleFactory(Random()))
        }
    }
}


data class Snake(val body: List<Point>) {
    constructor(vararg body: Point) : this(body.toList())

    val bitItself = body.distinct().size != body.size
    val direction = body.direction

    fun move(direction: Direction): Snake {
        val reversed = direction.oppositeTo(body.direction)
        val updatedBody = if (reversed) body.reversed() else body
        val actualDirection = if (reversed) updatedBody.direction else direction
        return Snake(updatedBody.moveIn(actualDirection))
    }

    private fun List<Point>.moveIn(direction: Direction) = listOf(first().moveIn(direction)) + dropLast(1)

    private fun Point.moveIn(direction: Direction) = when (direction) {
        Left -> Point(x - 1, y)
        Up -> Point(x, y - 1)
        Right -> Point(x + 1, y)
        Down -> Point(x, y + 1)
    }

    private val List<Point>.direction get(): Direction = this.let {
        if (it[0].x == it[1].x - 1) Left
        else if (it[0].x == it[1].x + 1) Right
        else if (it[0].y == it[1].y - 1) Up
        else if (it[0].y == it[1].y + 1) Down
        else error("")
    }
}

data class Point(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

enum class Direction {
    Left, Up, Right, Down
}

private fun Direction.oppositeTo(direction: Direction): Boolean = when (this) {
    Left -> direction == Right
    Up -> direction == Down
    Right -> direction == Left
    Down -> direction == Up
}
