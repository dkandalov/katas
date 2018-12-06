package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import org.junit.Test

// https://www.hackerrank.com/challenges/non-divisible-subset
class NonDivisibleSubset {
    @Test fun `example from the task description`() {
        nonDivisibleSubset(k = 4, array = arrayOf(19, 10, 12, 10, 24, 25, 22)) shouldEqual 3
    }

    @Test fun `basic example`() {
        nonDivisibleSubset(k = 3, array = arrayOf(1, 7, 2, 4)) shouldEqual 3
    }
}

fun nonDivisibleSubset(k: Int, array: Array<Int>): Int =
    frequenciesOfNonDivisibleSubset(k, array).values.sum()

fun frequenciesOfNonDivisibleSubset(k: Int, array: Array<Int>): Map<Int, Int> {
    // Use reminder of dividing all values by "k" because this doesn't affect how they add up to "k".
    val map = array.fold(HashMap<Int, Int>()) { map, it ->
        val n = it % k
        map[n] = map.getOrDefault(n, 0) + 1
        map
    }

    if (map.containsKey(0)) map[0] = 1
    if ((k % 2 == 0) && map.containsKey(k / 2)) map[k / 2] = 1

    val result = HashMap<Int, Int>()
    map.forEach { (key, value) ->
        // By choosing a value we always rule out only one other value, which is called "complement" here.
        val complementKey = k - key
        val complementValue = map[complementKey] ?: -1

        if (!result.containsKey(key) && !result.containsKey(complementKey)) {
            if (value > complementValue) result[key] = value
            else result[complementKey] = complementValue
        }
    }
    return result
}