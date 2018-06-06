@file:Suppress("MemberVisibilityCanBePrivate", "UNUSED_PARAMETER", "unused", "RemoveExplicitTypeArguments", "UNCHECKED_CAST", "UNUSED_LAMBDA_EXPRESSION")

package katas.kotlin.generics

class Inheritance {
    interface Super
    interface Base: Super
    interface Base2: Super

    interface ContainerSuper {
        fun get(): Super = doThings()
        fun add(it: Super): Unit = doThings()
    }
    
    interface ContainerBase: ContainerSuper {
        override fun get(): Base = doThings()
        override fun add(it: Super/*Any*/) = doThings()
    }

    interface ContainerBase2: ContainerSuper {
        override fun get(): Base2 = doThings()
        override fun add(it: Super/*Any*/) = doThings()
    }

    fun fail() {
        val containerBase: ContainerBase = object: ContainerBase {}
        val container: ContainerSuper = containerBase
        container.add(object: Base2 {})
        val base: Base = containerBase.get()
    }
}

class TypeArguments {
    interface Super
    interface Base: Super
    interface Sub: Base

    class Container<T> {
        fun get(): T = doThings()
        fun add(t: T): Unit = doThings()
    }

    fun covariantContainer() {

        fun <T> f(c: Container<out T>) {
            val t: T = c.get()
//            c.add(t)
        }

        f<Super>(Container<Super>())
        f<Super>(Container<Base>())
        f<Super>(Container<Sub>())

//      f<Base>(Container<Super>())
        f<Base>(Container<Base>())
        f<Base>(Container<Sub>())

//      f<Sub>(Container<Super>())
//      f<Sub>(Container<Base>())
        f<Sub>(Container<Sub>())
    }

    fun contravariantContainer() {

        fun <T> f(c: Container<in T>) {
//            val t: T = c.get()
            c.add(null as T)
        }

        f<Super>(Container<Super>())
//      f<Super>(Container<Base>())
//      f<Super>(Container<Sub>())

        f<Base>(Container<Super>())
        f<Base>(Container<Base>())
//      f<Base>(Container<Sub>())

        f<Sub>(Container<Super>())
        f<Sub>(Container<Base>())
        f<Sub>(Container<Sub>())
    }

    fun invariantContainer() {

        fun <T> f(c: Container<T>) {
            val t: T = c.get()
            c.add(t)
        }

        f<Super>(Container<Super>())
//      f<Super>(Container<Base>())
//      f<Super>(Container<Sub>())

//      f<Base>(Container<Super>())
        f<Base>(Container<Base>())
//      f<Base>(Container<Sub>())

//      f<Sub>(Container<Super>())
//      f<Sub>(Container<Base>())
        f<Sub>(Container<Sub>())
    }
}

private fun doThings(): Nothing = throw NotImplementedError()
