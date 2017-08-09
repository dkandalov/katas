package katas.kotlin.dsl

interface Element

data class TextElement(val text: String): Element

@DslMarker
annotation class HtmlTagMarker

@HtmlTagMarker
abstract class Tag(val name: String): Element {
    val children = arrayListOf<Element>()
    val attributes = hashMapOf<String, String>()

    override fun toString(): String {
        return "Tag(name='$name', attributes=$attributes, children=$children)"
    }

    operator fun String.unaryPlus() = children.add(TextElement(this))
}

private class Html: Tag("html") {
    fun head(init: Head.() -> Unit) = children.add(Head().apply { init() })
    fun body(init: Body.() -> Unit) = children.add(Body().apply { init() })
}

private class Head: Tag("head") {
    fun title(init: Title.() -> Unit) = children.add(Title().apply { init() })
}

private class Title: Tag("title")

private class Body: Tag("body") {
    fun h1(init: H1.() -> Unit) = children.add(H1().apply { init() })
    fun a(href: String, init: A.() -> Unit) = children.add(A().apply { attributes["href"] = href; init() })
    fun b(init: B.() -> Unit) = children.add(B().apply { init() })
    fun p(init: P.() -> Unit) = children.add(P().apply { init() })
}

private class H1: Tag("h1")
private class A: Tag("a")
private class B: Tag("b")
private class P: Tag("p")

private fun html(init: Html.() -> Unit): Html {
    val html = Html()
    init(html)
    return html
}


fun main(args: Array<String>) {
    val h = html {
        head {
            title { +"XML encoding with Kotlin" }
        }
        body {
            h1 { +"XML encoding with Kotlin" }
            p { +"this format can be used as an alternative markup to XML" }

            a(href = "http://kotlinlang.org") { +"Kotlin" }

            p {
                +"This is some"
                // TODO b { +"mixed" }
                +"text. For more see the"
                // TODO a(href = "http://kotlinlang.org") { +"Kotlin" }
                +"project"
            }
            p { +"some text" }

            p {
                for (arg in args) +arg
            }
        }
    }

    println(h)
}
