package katas.kotlin.leetcode.stores_and_houses

import datsok.shouldEqual
import org.junit.Test
import kotlin.math.abs

/**
 * https://leetcode.com/discuss/interview-question/350248/Google-or-Summer-Intern-OA-2019-or-Stores-and-Houses
 *
 * You are given 2 arrays representing integer locations of stores and houses (each location in this problem is one-dimentional). For each house, find the store closest to it.
 * Return an integer array result where result[i] should denote the location of the store closest to the i-th house.
 * If many stores are equidistant from a particular house, choose the store with the smallest numerical location.
 * Note that there may be multiple stores and houses at the same location.
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