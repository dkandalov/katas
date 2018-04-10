package katas.kotlin.snake.v0_refactored

import katas.kotlin.coroutines.steps.step1.EmptyContinuation
import kotlin.coroutines.experimental.Continuation
import kotlin.coroutines.experimental.createCoroutine
import kotlin.coroutines.experimental.intrinsics.COROUTINE_SUSPENDED
import kotlin.coroutines.experimental.intrinsics.suspendCoroutineOrReturn

fun main(args: Array<String>) {
    CoGameUI.build(GameSwingUI()) {
        startGame()
        var game = Game.create(50, 50)

        while (game.state == Game.State.Playing) {
            paint(game)

            val event = readEvent()
            game = if (event == null) {
                game.updateOnTimer()
            } else {
                game.updateOnUserInput(event)
            }
        }
        paint(game)
    }
}

private class CoGameUI(private val gameUI: GameUI) {
    private var eventContinuation: Continuation<Direction?>? = null

    suspend fun startGame() {
        return suspendCoroutineOrReturn { continuation: Continuation<Unit> ->
            gameUI.init(object: GameUI.Observer {
                override fun onGameStart() {
                    continuation.resume(Unit)
                }

                override fun onTimer() {
                    eventContinuation?.resume(null)
                }

                override fun onUserInput(direction: Direction) {
                    eventContinuation?.resume(direction)
                }
            })
            COROUTINE_SUSPENDED
        }
    }

    fun paint(game: Game) {
        gameUI.paint(game)
    }

    suspend fun readEvent(): Direction? {
        return suspendCoroutineOrReturn { continuation: Continuation<Direction?> ->
            eventContinuation = continuation
            COROUTINE_SUSPENDED
        }
    }

    companion object {
        fun build(gameUI: GameUI, callback: suspend CoGameUI.() -> Unit) {
            val result = CoGameUI(gameUI)
            callback.createCoroutine(result, completion = EmptyContinuation).resume(Unit)
        }
    }
}
