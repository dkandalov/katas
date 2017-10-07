package katas.kotlin.bowling

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.lang.Math.min

class BowlingKata {
    @Test fun `parsing game score`() =
            assertThat(parse("123-4/5678XXXXXX9"), equalTo(listOf(
                    Frame(1, 2), Frame(3, 0), Spare(4), Frame(5, 6), Frame(7, 8),
                    Strike, Strike, Strike, Strike, Strike, Frame(10, 9)
            )))

    @Test fun `game without spares or strikes`() =
            assertThat(gameScore("12121212121212121212"), equalTo(30))

    @Test fun `game with misses`() =
            assertThat(gameScore("9-9-9-9-9-9-9-9-9-9-"), equalTo(90))

    @Test fun `game with spares`() =
            assertThat(gameScore("5/5/5/5/5/5/5/5/5/5-"), equalTo(140))

    @Test fun `game with spares and bonus ball`() =
            assertThat(gameScore("5/5/5/5/5/5/5/5/5/5/5"), equalTo(150))

    @Test fun `game with strikes and two bonus balls`() =
            assertThat(gameScore("XXXXXXXXXXXX"), equalTo(300))


    private fun gameScore(framesString: String): Int {
        return gameScore(parse(framesString))
    }

    private fun gameScore(frames: List<AFrame>): Int {
        var score = 0
        frames.take(10).forEachIndexed { i, it ->
            if (it is Frame) score += it.pins()
            else if (it is Spare) score += it.pins() + frames[i + 1].pins1()
            else if (it == Strike) {
                score += it.pins() + frames[i + 1].pins()
                if (frames[i + 1] == Strike) {
                    score += frames[i + 2].pins1()
                }
            }
        }
        return score
    }

    private fun parse(framesString: String): List<AFrame> {
        return doParse(framesString.replace("-", "0"), 0, emptyList())
    }

    private fun doParse(s: String, count: Int, result: List<AFrame>): List<AFrame> {
        fun Char.parseInt() = Integer.parseInt(this.toString())

        if (s.isEmpty()) return result

        if (count < 10) {
            if (s[0] == 'X') {
                return doParse(s.substring(1), count + 1, result + Strike)
            }
            val score1 = s[0].parseInt()
            if (s[1] == '/') {
                return doParse(s.substring(2), count + 1, result + Spare(score1))
            } else {
                val score2 = s[1].parseInt()
                return doParse(s.substring(2), count + 1, result + Frame(score1, score2))
            }
        } else {
            val score1 = if (s[0] == 'X') 10 else s[0].parseInt()
            val score2 = if (s.length == 1) 0 else {
                if (s[1] == 'X') 10 else s[1].parseInt()
            }
            return doParse(s.substring(min(s.length, 2)), count + 1, result + Frame(score1, score2))
        }
    }


    private interface AFrame {
        fun pins1(): Int
        fun pins2(): Int
        fun pins(): Int = pins1() + pins2()
    }

    private data class Frame(val pins1: Int, val pins2: Int) : AFrame {
        override fun pins1() = pins1
        override fun pins2() = pins2
    }

    private data class Spare(val pins1: Int) : AFrame {
        override fun pins1() = pins1
        override fun pins2() = 10 - pins1
    }

    private val Strike: AFrame = object: AFrame {
        override fun pins1() = 10
        override fun pins2() = 0
        override fun toString() = "Strike"
    }

}