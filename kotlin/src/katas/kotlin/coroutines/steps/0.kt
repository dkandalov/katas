package katas.kotlin.coroutines.steps

import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test


class CPS {

    @Test fun `identity functions`() {
        fun <T> identity(value: T): T { return value }
        fun <T> identityCPS(value: T, rest: (T) -> Unit) { rest(value) }

        identity(123).printed()
        identityCPS(123) { it.printed() }
    }

    @Test fun `max functions`() {
        fun max(a: Int, b: Int): Int = if (a < b) b else a
        fun maxCPS(a: Int, b: Int, rest: (Int) -> Unit) = if (a < b) rest(b) else rest(a)

        max(4, 2).printed()
        maxCPS(5, 3) { it.printed() }
    }

    @Test fun `f and g`() {
        fun g(n: Int): Int = n + 1
        fun f(n: Int): Int = g(n + 2) + 3

        fun gCPS(n: Int, rest: (Int) -> Unit) = rest(n + 1)
        fun fCPS(n: Int, rest: (Int) -> Unit) = gCPS(n + 2, { rest(it + 3) })

        f(10) shouldEqual 16
        fCPS(10) { it shouldEqual 16 }
    }

    @Test fun `factorial functions`() {
        fun factorial(n: Int): Int {
            if (n == 0) return 1
            else return n * factorial(n - 1)
        }
        fun factorialCPS(n: Int, rest: (Int) -> Unit) {
            if (n == 0) rest(1)
            else factorialCPS(n - 1, { rest(n * it) })
        }

        factorial(5) shouldEqual 120
        factorialCPS(5) { it shouldEqual 120 }
    }

    @Test fun `🙈`() {
        val events = ArrayList<Int>()
        fun log(it: Int) {
            events.add(it)
        }

        fun f(yield: (((() -> Unit) -> Unit) -> Unit) -> Unit) {
            log(1)
            yield() { yield_ ->
                log(3)
                yield_ {
                    log(5)
                }
            }
        }

        log(0)
        f { resume ->
            log(2)
            resume { resume_ ->
                log(4)
                resume_()
            }
        }

        events.printed() shouldEqual listOf(0, 1, 2, 3, 4, 5)
    }

    private val events = ArrayList<Int>()
    private fun log(it: Int) {
        events.add(it)
    }

    @Test fun `🙈🙈`() {
        class ResumableFunction(var state: Int = 0) {
            fun invoke() {
                if (state == 0) {
                    log(1)
                    state++
                } else if (state == 1) {
                    log(3)
                    state++
                } else if (state == 2) {
                    log(5)
                    state++
                }
                else error("")
            }
        }

        val f = ResumableFunction()
        log(0)
        f.invoke()
        log(2)
        f.invoke()
        log(4)
        f.invoke()

        events.printed() shouldEqual listOf(0, 1, 2, 3, 4, 5)
    }
}
