import java.io.File
import java.util.*

fun main(args: Array<String>) {
    val words = (File("53970-0.txt").readLines() + File("18440-0.txt").readLines() + File("39702-0.txt").readLines())
            .map { it.trim().replace(Regex("[_#—]"), "") }
            .filter { it.isNotEmpty() }
            .flatMap { it.split(Regex("\\s+")) }

    val data = words.sliding(3).fold(HashMap<Pair<String, String>, MutableList<String>>()) { map, trigram ->
        val key = Pair(trigram[0], trigram[1])
        map.putIfAbsent(key, ArrayList())
        map[key]!!.add(trigram[2])
        map
    }
    //data.values.take(100).forEach { println(it) }

    val random = Random(123)
    val entryPoint = data.keys.drop(random.nextInt(data.size)).first()

    val pairs = generateSequence(entryPoint) { it: Pair<String, String> ->
        val nextWords = data[it]
        if (nextWords == null || nextWords.isEmpty()) null
        else Pair(it.second, nextWords[random.nextInt(nextWords.size)])
    }
    val text = pairs.take(2000).map{ it.first }.joinToString(" ")
    println(text)
}

private fun <E> List<E>.sliding(windowSize: Int): List<List<E>> {
    return (0..(size - windowSize)).map { subList(it, it + windowSize) }
}
