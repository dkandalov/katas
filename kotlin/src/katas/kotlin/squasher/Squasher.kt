package katas.kotlin.squasher

import datsok.shouldEqual
import nonstdlib.join
import nonstdlib.toList
import org.junit.jupiter.api.Test

class SquasherTests {
    @Test fun `it works`() {
        squasher("abc".iterator()).join("") shouldEqual "abc"
        squasher("*abc".iterator()).join("") shouldEqual "*abc"
        squasher("**abc".iterator()).join("") shouldEqual "^abc"
        squasher("***abc".iterator()).join("") shouldEqual "^*abc"
        squasher("****abc".iterator()).join("") shouldEqual "^^abc"
    }

    @Test fun `it works 2`() {
        squasher2("abc".iterator()).join("") shouldEqual "abc"
        squasher2("*abc".iterator()).join("") shouldEqual "*abc"
        squasher2("**abc".iterator()).join("") shouldEqual "^abc"
        squasher2("***abc".iterator()).join("") shouldEqual "^*abc"
        squasher2("****abc".iterator()).join("") shouldEqual "^^abc"
    }
}

private fun squasher(input: Iterator<Char>) = object : Iterator<Char> {
    var hasLastChar = false
    var lastChar = 0.toChar()

    override fun next() =
        if (hasLastChar) {
            hasLastChar = false
            lastChar
        } else {
            var char = input.next()
            if (char == '*') {
                lastChar = input.next()
                if (lastChar == '*') char = '^'
                else hasLastChar = true
            }
            char
        }

    override fun hasNext() = input.hasNext() || hasLastChar
}

private fun squasher2(input: Iterator<Char>): Iterator<Char> = sequence {
    while (input.hasNext()) {
        val char = input.next()
        if (char != '*') yield(char)
        else {
            val lastChar = input.next()
            if (lastChar == '*') {
                yield('^')
            } else {
                yield(char)
                yield(lastChar)
            }
        }
    }
}.iterator()

private fun <T> Iterator<T>.join(s: String) = toList().join(s)
