package katas.kotlin

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo

infix fun <T> T.shouldEqual(that: T) {
    assertThat(this, equalTo(that))
}

infix fun <T> T.shouldNotEqual(that: T) {
    assertThat(this, !equalTo(that))
}
