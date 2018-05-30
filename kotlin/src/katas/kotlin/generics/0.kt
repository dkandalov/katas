package katas.kotlin.generics

import org.junit.Test

@Suppress("UNUSED_PARAMETER", "unused", "RemoveExplicitTypeArguments")
class Tests {
    open class Super
    open class Base: Super()
    open class Sub: Base()

    @Test fun covariantContainer() {
        class Container<out T> {
            fun get(): T = doThings()
//            fun add(t: T): Unit = doThings()
        }

        val superList = Container<Super>()
        val baseList = Container<Base>()
        val subList = Container<Sub>()

        fun <T> foo(container: Container<T>): Unit = doThings()

        foo<Super>(superList)
        foo<Super>(baseList)
        foo<Super>(subList)

//        foo<Base>(superList)
        foo<Base>(baseList)
        foo<Base>(subList)

//        foo<Sub>(superList)
//        foo<Sub>(baseList)
        foo<Sub>(subList)
    }

    @Test fun contravariantContainer() {
        class Container<in T> {
//            fun get(): T = doThings()
            fun add(t: T): Unit = doThings()
        }

        val superList = Container<Super>()
        val baseList = Container<Base>()
        val subList = Container<Sub>()

        fun <T> foo(list: Container<T>): Unit = doThings()

        foo<Super>(superList)
//        foo<Super>(baseList)
//        foo<Super>(subList)

        foo<Base>(superList)
        foo<Base>(baseList)
//        foo<Base>(subList)

        foo<Sub>(superList)
        foo<Sub>(baseList)
        foo<Sub>(subList)
    }

    @Test fun invariantContainer() {
        class Container<T> {
            fun get(): T = doThings()
            fun add(t: T): Unit = doThings()
        }

        val superList = Container<Super>()
        val baseList = Container<Base>()
        val subList = Container<Sub>()

        fun <T> foo(list: Container<T>): Unit = doThings()

        foo<Super>(superList)
//        foo<Super>(baseList)
//        foo<Super>(subList)

//        foo<Base>(superList)
        foo<Base>(baseList)
//        foo<Base>(subList)

//        foo<Sub>(superList)
//        foo<Sub>(baseList)
        foo<Sub>(subList)
    }

    private fun doThings(): Nothing = throw NotImplementedError()
}

