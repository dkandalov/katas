package katas.kotlin

import katas.kotlin.Durations.callbackByKey
import katas.kotlin.Durations.durationByKey
import katas.kotlin.permutation.Permutation1
import kotlincommon.measureTimeMillis
import java.time.Duration
import java.util.concurrent.ConcurrentHashMap
import kotlin.math.pow

fun <E> MutableList<E>.swap(index1: Int, index2: Int) {
    val tmp = this[index1]
    this[index1] = this[index2]
    this[index2] = tmp
}

fun <E> List<E>.permutations(): Sequence<List<E>> = with(Permutation1) { this@permutations.permutations() }

fun Int.pow(n: Int): Int = this.toDouble().pow(n.toDouble()).toInt()

fun Long.pow(n: Long): Long = this.toDouble().pow(n.toDouble()).toLong()

inline fun <T> measureDuration(onMeasurement: (millis: Long) -> Unit, f: () -> T): T {
    val start = System.currentTimeMillis()
    val result = f()
    onMeasurement(System.currentTimeMillis() - start)
    return result
}

object Durations {
    val durationByKey: MutableMap<String, Duration> = ConcurrentHashMap()
    val callbackByKey: MutableMap<String, (key: String, duration: Duration) -> Unit> = ConcurrentHashMap()

    init {
        Runtime.getRuntime().addShutdownHook(Thread {
            if (callbackByKey.isEmpty()) return@Thread
            callbackByKey.forEach { key, callback ->
                callback(key, durationByKey[key]!!)
            }
        })
    }
}

inline fun <T> measureDuration(
    key: String,
    noinline onMeasurement: (key: String, duration: Duration) -> Unit = { _, duration -> println("$key: $duration") },
    f: () -> T
): T {
    val start = System.currentTimeMillis()
    val result = f()
    val duration = Duration.ofMillis(System.currentTimeMillis() - start)
    durationByKey[key] = durationByKey.getOrDefault(key, Duration.ZERO) + duration
    callbackByKey[key] = onMeasurement
    return result
}