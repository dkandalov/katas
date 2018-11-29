@file:Suppress(
    "unused", "MemberVisibilityCanBePrivate", "UNUSED_VARIABLE",
    "RemoveExplicitTypeArguments", "RedundantExplicitType"
)

package katas.kotlin.generics

open class A
open class B: A() {
    fun foo() {}
}

object Foo_ {
    init {
        A()
        B().foo()
    }
}

//     A    B
// A  a<=a  a<=b
// B  b!!a  b<=b

//          ()->A       ()->B
// ()->A  getA<=getA  getA<=getB
// ()->B  getB!!getA  getB<=getB

//          A->()       B->()
// A->()  setA<=setA  setA!!setB
// B->()  setB<=setA  setB<=setB

object Foo0 {
    init {
        val x1: A = A()
        val x2: B = B()
    }

    // replace rhs with all A's and B's
    init {
        val x1: A = A()
//        val x2: B = A() // ()->A
        val x3: A = B()   // ()->B
        val x4: B = B()
    }

    // replace lhs with all A's and B's
    init {
        val x1: A = A()
        val x2: A = B()   // setB<=setA
//        val x3: B = A() // setA!!setB
        val x4: B = B()
    }
}

object Foo1 {
    val a: A = A()
    //val b: B = A() // 👈 Type mismatch
    val a2: A = B()
    val b2: B = B()
}

object Foo2 {
    fun readA(): A = A()
    fun readB(): B = B()

    val a: A = readA()
    // val b: B = readA() // 👈 Type mismatch
    val a2: A = readB()
    val b2: B = readB()

    val x1: () -> A = ::readA
    val x2: () -> B = ::readB
}

object `Foo2'` {
    fun readReadA(): () -> A = { A() }
    fun readReadB(): () -> B = { B() }

    val x1: () -> () -> A = ::readReadA
    val x2: () -> () -> A = ::readReadB
    // val x3: () -> () -> B = ::readReadA // 👈 Type mismatch
    val x4: () -> () -> B = ::readReadB
}

object Foo3 {
    fun writeA(a: A) { val tmp: A = a }
    fun writeB(b: B) { val tmp: B = b }

    init {
        writeA(A())
        // writeB(A()) // 👈 Type mismatch
        writeA(B())
        writeB(B())

        // val writeA: (A) -> Unit = ::writeB
        val writeB: (B) -> Unit = ::writeA
    }
}

object `Foo3'` {
    fun writeWriteA(f: (A) -> Unit) {}
    fun writeWriteB(f: (B) -> Unit) {}

    val x1: ((A) -> Unit) -> Unit = ::writeWriteA
    val x2: ((A) -> Unit) -> Unit = ::writeWriteB
//    val x3: ((B) -> Unit) -> Unit = ::writeWriteA
    val x4: ((B) -> Unit) -> Unit = ::writeWriteB
}

object Foo4 {
    fun <T> read(): T = error("")

    val a: A = read<A>()
    val b: B = read<B>()
    val a2: A = read<B>()
    // val b2: B = read<A>() // 👈 Type mismatch

    fun <T> write(value: T) {}

    init {
        write<A>(A())
        write<B>(B())
        // write<B>(A()) // 👈 Type mismatch
        write<A>(B())
    }
}

object Foo5 {
    class Reader<T> {
        fun read(): T = error("👻")
    }
    class Writer<T> {
        fun write(value: T): Unit = error("")
    }
}


