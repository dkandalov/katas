package katas.kotlin.coroutines.examples_from_docs

import kotlinx.coroutines.*

fun main() {
    val context = newFixedThreadPoolContext(40, "foo")
    runBlocking {
        (1..100).map {
            async(context) {
                delay(100)
                println(it.toString() + " - " + Thread.currentThread())
            }
        }.forEach {
            it.await()
        }
        println("hello")
    }
}