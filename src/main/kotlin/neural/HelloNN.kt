package neural

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.util.*
import java.util.function.Supplier

class HelloNN {
    @Test fun `neural network to learn logical AND function`() {
        val random = Random(123)

        val inputLayer = Array(2, { 0.0 })
        val outputLayer = Layer(1, inputLayer, ::sigmoid, ::sigmoidDerivative).init(random)

        val trainingExamples = listOf(
                Example(listOf(0.5, 0.5), listOf(0.5)),
                Example(listOf(0.5, 1.0), listOf(0.5)),
                Example(listOf(1.0, 0.5), listOf(0.5)),
                Example(listOf(1.0, 1.0), listOf(1.0))
        ).repeat(times = 200000).shuffle(random)

        val layers = listOf(outputLayer)
        val trainer = Trainer(layers).trainWith(trainingExamples)

        println(trainer.totalError)
        println(outputLayer.toString())

        assertThat(trainer.totalError, equalTo(0.013919690744522845))
    }

    @Test fun `neural network to convert polar to cartesian coordinates`() {
        val random = Random(123)

        val inputLayer = Array(2, { 0.0 })
        val hiddenLayer = Layer(40, inputLayer, ::tanh, ::tanhDerivative).init(random)
        val outputLayer = Layer(2, hiddenLayer.outputs, {it}, {1.0}).init(random)

        val trainingExamples = Supplier<Example?> {
            val d1 = random.nextDouble()
            val d2 = random.nextDouble()
            val inputs = listOf(d1, d2)
            val expectedOutputs = Pair(d1, d2).polarToCartesian().toList()
            Example(inputs, expectedOutputs)
        }

        val layers = listOf(hiddenLayer, outputLayer)
        val trainer = Trainer(layers).trainWith(trainingExamples, limit = 15000)
        println(trainer.totalError)

        assertThat(trainer.totalError, equalTo(0.15619307115270933))
    }


    private data class Example(val inputs: List<Double>, val expectedOutputs: List<Double>)


    private class Trainer(val layers: List<Layer>) {
        var totalError: Double = 0.0

        fun trainWith(examples: Collection<Example>): Trainer {
            val iterator = examples.iterator()
            return trainWith(Supplier {
                if (iterator.hasNext()) iterator.next() else null
            })
        }

        fun trainWith(examplesSupplier: Supplier<Example?>, limit: Long? = null): Trainer {
            var attempt = 0
            val allErrors = mutableListOf<Double>()

            var example = examplesSupplier.get()
            while (example != null && (limit == null || attempt < limit)) {
                example.inputs.forEachIndexed { i, d ->
                    layers.first().inputs[i + 1] = d
                }

                layers.forEach {
                    it.activate()
                }

                val errors = example.expectedOutputs.zip(layers.last().outputs)
                        .map { it.first - it.second }
                        .toTypedArray()
                layers.foldRight(errors) { layer, errors ->
                    layer.backPropagate(errors)
                }

                allErrors.addAll(errors)
                if (attempt % 10000 == 0) {
                    println(allErrors.sum() / allErrors.size)
                }
                attempt++
                example = examplesSupplier.get()
            }
            totalError = allErrors.sum() / allErrors.size
            return this
        }
    }


    private class Layer(val size: Int,
                        val inputsWithoutBias: Array<Double>,
                        val activation: (Double) -> Double = ::sigmoid,
                        val activationDerivative: (Double) -> Double = ::sigmoidDerivative
    ) {
        private val learningRate = 0.01
        val inputs = arrayOf(1.0) + inputsWithoutBias // prepend bias input
        private val inputsSum = Array(size, {0.0})
        private val inputThetas = Array(size, { Array(inputs.size, { 0.0 }) })
        val outputs = Array(size, { 0.0 })

        fun init(random: Random = Random()): Layer {
            0.until(size).forEach { cellIndex ->
                0.until(inputs.size).forEach { inputIndex ->
                    inputThetas[cellIndex][inputIndex] = random.nextDouble()
                }
            }
            return this
        }

        fun activate() {
            0.until(outputs.size).forEach { cellIndex ->
                var sum = 0.0
                0.until(inputs.size).forEach { inputIndex ->
                    sum += inputs[inputIndex] * inputThetas[cellIndex][inputIndex]
                }
                inputsSum[cellIndex] = sum
                outputs[cellIndex] = activation(sum)
            }
        }

        fun backPropagate(errors: Array<Double>): Array<Double> {
            0.until(size).forEach { cellIndex ->
                0.until(inputs.size).forEach { inputIndex ->
                    val delta = learningRate * activationDerivative(inputsSum[cellIndex]) * errors[cellIndex] * inputs[inputIndex]
                    inputThetas[cellIndex][inputIndex] += delta
                }
            }

            val errorsForInputLayer = Array(inputsWithoutBias.size, { inputIndex ->
                var sum = 0.0
                errors.forEachIndexed { cellIndex, error ->
                    sum += error * inputThetas[cellIndex][inputIndex + 1]
                }
                sum
            })
            return errorsForInputLayer
        }

        override fun toString(): String {
            val thetasAsString = inputThetas.joinToString("\n") { it.joinToString() }
            val outputsAsString = outputs.joinToString("\n")
            return "Layer(size=$size, thetas=\n$thetasAsString, \noutputs=\n$outputsAsString"
        }
    }

    @Test fun `convert polar to cartesian coordinates`() {
        assertThat(Pair(1.0, 1.0).polarToCartesian(), equalTo(Pair(1.4142135623730951, 45.0)))
        assertThat(Pair(1.4142135623730951, 45.0).cartesianToPolar(), equalTo(Pair(1.0000000000000002, 1.0)))

        assertThat(Pair(12.0, 5.0).polarToCartesian(), equalTo(Pair(13.0, 22.61986494804043)))
        assertThat(Pair(13.0, 22.61986494804043).cartesianToPolar(), equalTo(Pair(12.0, 5.0)))
    }
}

private fun sigmoid(x: Double): Double {
    return 1.0 / (1.0 + Math.exp(-x))
}

private fun sigmoidDerivative(x: Double): Double {
    val d = sigmoid(x)
    return d * (1.0 - d)
}

private fun tanh(x: Double): Double {
    return (Math.exp(x) - Math.exp(-x)) / (Math.exp(x) + Math.exp(-x))
}

private fun tanhDerivative(x: Double): Double {
    val d = tanh(x)
    return 1.0 - (d * d)
}

private fun Pair<Double, Double>.polarToCartesian(): Pair<Double, Double> {
    val (x, y) = this
    val distance = Math.sqrt((x * x) + (y * y))
    val angle = Math.atan(y / x).toDegree()
    return Pair(distance, angle)
}

private fun Pair<Double, Double>.cartesianToPolar(): Pair<Double, Double> {
    val (distance, angle) = this
    val x = distance * Math.cos(angle.toRadian())
    val y = distance * Math.sin(angle.toRadian())
    return Pair(x, y)
}

private fun Double.toDegree() = this * (180.0 / Math.PI)
private fun Double.toRadian() = this * (Math.PI / 180.0)

private fun <T> List<T>.repeat(times: Int): List<T> {
    val result = mutableListOf<T>()
    0.until(times).forEach { result.addAll(this) }
    return result
}
private fun <T> List<T>.shuffle(random: Random): List<T> {
    val result = mutableListOf<T>()
    result.addAll(this)
    Collections.shuffle(result, random)
    return result
}