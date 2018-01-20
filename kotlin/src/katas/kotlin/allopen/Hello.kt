package katas.kotlin.allopen

annotation class MyOpen

@MyOpen class A {
    fun foo() = 42
}

//class B : A() {
//    override fun foo() = 43
//}