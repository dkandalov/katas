package katas.kotlin.hackerrank

import java.math.BigInteger
import java.math.BigInteger.ONE

// https://www.hackerrank.com/challenges/extra-long-factorials
fun extraLongFactorials(n: Int) {
    tailrec fun factorial(n: BigInteger, result: BigInteger = ONE): BigInteger {
        return if (n == ONE) return result
        else factorial(n - ONE, result * n)
    }
    println(factorial(BigInteger(n.toString())))
}