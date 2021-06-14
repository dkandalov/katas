package katas.kotlin.leetcode.roll_dice

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/331158/Amazon-or-Online-Assessment-2019-or-Roll-Dice
 */
class RollDiceTests {
    @Test fun examples() {
        rollDice(1, 2, 3) shouldEqual 2
        rollDice(1, 1, 6) shouldEqual 2
        rollDice(1, 6, 2, 3) shouldEqual 3
    }
}

private fun rollDice(vararg dice: Int): Int {
    return (1..6).map { target ->
        dice.sumBy { numberOfMoves(it, target) }
    }.minOrNull()!!
}

private fun numberOfMoves(die: Int, target: Int) =
    when {
        die == target     -> 0
        die + target == 7 -> 2
        else              -> 1
    }