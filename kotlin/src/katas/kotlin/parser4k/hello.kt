package katas.kotlin.parser4k

import datsok.shouldEqual
import org.junit.jupiter.api.Test
import parser4k.Input
import parser4k.str

class Tests {
    private val hello = str("hello")

    @Test fun `hello parser4k`() {
        hello.parse(Input("foo"))?.payload shouldEqual null
        hello.parse(Input("hello foo"))?.payload shouldEqual "hello"
    }
}