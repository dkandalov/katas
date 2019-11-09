package katas.kotlin.leetcode.swype_lock

import kotlincommon.test.*
import org.junit.*
import kotlin.math.*

class SwypeLock {
    @Test fun `some examples`() {
        // 1 2 3
        // 4 5 6
        // 7 8 9
        validate(emptyList()) shouldEqual false
        validate((1..10).toList()) shouldEqual false
        validate(listOf(1, 1)) shouldEqual false
        validate(listOf(1, 2, 3)) shouldEqual true

        validate(listOf(1, 2)) shouldEqual true
        validate(listOf(1, 3)) shouldEqual false
//        validate(listOf(1, 4)) shouldEqual true
//        validate(listOf(1, 5)) shouldEqual true
    }
}

private fun validate(swypeLock: List<Int>): Boolean {
    if (swypeLock.size !in 1..9 || swypeLock.distinct().size != swypeLock.size) return false
    return swypeLock.windowed(size = 2).all { (digit1, digit2) ->
        (digit1 - digit2).absoluteValue == 1
    }
}