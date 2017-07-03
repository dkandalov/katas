
import katas.kotlin.sliding
import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.File
import java.util.*

/**
 * Code kata http://codekata.com/kata/kata14-tom-swift-under-the-milkwood/
 * Plain text books from http://www.gutenberg.org/wiki/Main_Page
 */
class TrigramTest {
    @Test fun `process couple books and generate text based on trigrams from all of them`() {
        val load = { fileName: String -> File("src/katas/kotlin/trigram/$fileName").readLines() }
        val words = (load("53970-0.txt") + load("18440-0.txt") + load("39702-0.txt"))
                .map { it.trim().replace(Regex("[_#—]"), "") }
                .filter { it.isNotEmpty() }
                .flatMap { it.split(Regex("\\s+")) }

        val data = words.sliding(3).fold(HashMap<Pair<String, String>, MutableList<String>>()) { map, trigram ->
            val key = Pair(trigram[0], trigram[1])
            map.putIfAbsent(key, ArrayList())
            map[key]!!.add(trigram[2])
            map
        }
        //data.values.take(100).forEach { printed(it) }

        val random = Random(123)
        val entryPoint = data.keys.drop(random.nextInt(data.size)).first()

        val pairs = generateSequence(entryPoint) { it: Pair<String, String> ->
            val nextWords = data[it]
            if (nextWords == null || nextWords.isEmpty()) null
            else Pair(it.second, nextWords[random.nextInt(nextWords.size)])
        }
        val text = pairs.take(2000).map { it.first }.joinToString(" ")
        println(text)

        assertTrue(text.startsWith(
                "example, there is more fairly representative than such images ever are, but a means of the climate admirable. " +
                "The productions of the battalion of the velocity of Light as the sequel will shew."
        ))
    }
}
