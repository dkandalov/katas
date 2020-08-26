package katas.kotlin.parser4k

import datsok.shouldEqual
import org.junit.jupiter.api.Test
import parser4k.Input
import parser4k.str

class Tests {
    @Test fun `hello parser4k`() {
        val hello = str("hello")
        hello.parse(Input("foo"))?.payload shouldEqual null
        hello.parse(Input("hello foo"))?.payload shouldEqual "hello"
    }
}