package katas.kotlin.hackerrank

import java.io.InputStream

class OutputRecorder: (Any?) -> Unit {
    var text = ""
    override fun invoke(o: Any?) {
        text += o.toString() + "\n"
    }
}

fun String.trimToLineSequence(): Sequence<String> = trim().trimMargin().split("\n").asSequence()

fun String.toReadLineFunction(): () -> String {
    val i = trim().trimMargin().split("\n").asSequence().iterator()
    return { i.next() }
}

fun InputStream.toReadLineFunction(): () -> String {
    val bufferedReader = bufferedReader()
    return { bufferedReader.readLine() }
}

