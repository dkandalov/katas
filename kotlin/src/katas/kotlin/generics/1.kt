@file:Suppress("unused", "MemberVisibilityCanBePrivate", "UNUSED_VARIABLE")

package katas.kotlin.generics

open class A
open class B: A()

object Foo1 {
    val a: A = A()
    val b: B = B()

    val a2: A = B()
    //val b2: B = A() 👈 Type mismatch
}

object Foo2 {
    fun readA(): A = A()
    fun readB(): B = B()

    val a: A = readA()
    val b: B = readB()

    val a2: A = readB()
    // val b2: B = readA() 👈 Type mismatch

    val readA: () -> A = ::readA
    val readB: () -> B = ::readB

    val readA2: () -> A = ::readB
    // val readB2: () -> B = ::readA
}

object Foo3 {
    fun writeA(a: A) {
        val tmp: A = a
    }

    fun writeB(b: B) {
        val tmp: B = b
    }

    init {
        writeA(A())
        writeB(B())

        // writeB(A()) 👈 Type mismatch
        writeA(B())

        val writeA: (A) -> Unit = ::writeA
        val writeB: (B) -> Unit = ::writeB

        // val writeA2: (A) -> Unit = ::writeB
        val writeB2: (B) -> Unit = ::writeA
    }
}

object Foo4 {
}