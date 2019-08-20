package katas.kotlin.leetcode.stores_and_houses

import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.math.abs

/**
 * https://leetcode.com/discuss/interview-question/350248/Google-or-Summer-Intern-OA-2019-or-Stores-and-Houses
 */
class StoresAndHousesTests {
    @Test fun examples() {
        findClosesStores(houses = arrayOf(5, 10, 17), stores = arrayOf(1, 5, 20, 11, 16)) shouldEqual listOf(5, 11, 16)
        findClosesStores(houses = arrayOf(0, 5, 10, 17), stores = arrayOf(1, 5, 20, 11, 16)) shouldEqual listOf(1, 5, 11, 16)
        findClosesStores(houses = arrayOf(2, 4, 2), stores = arrayOf(5, 1, 2, 3)) shouldEqual listOf(2, 3, 2)
        findClosesStores(houses = arrayOf(4, 8, 1, 1), stores = arrayOf(5, 3, 1, 2, 6)) shouldEqual listOf(3, 6, 1, 1)
    }

    private fun findClosesStores(houses: Array<Int>, stores: Array<Int>): List<Int> {
        stores.sort()
        return houses.map { position ->
            val i = stores.binarySearch(position)
            if (i >= 0) stores[i]
            else {
                val i1 = -i - 2
                val i2 = -i - 1
                when {
                    i1 < 0                                                   -> stores[i2]
                    i2 >= stores.size                                        -> stores[i1]
                    abs(position - stores[i1]) <= abs(position - stores[i2]) -> stores[i1]
                    else                                                     -> stores[i2]
                }
            }
        }
    }
}