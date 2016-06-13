package neural

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.util.*
import java.util.function.Supplier

class HelloNN {

    @Test fun `pre-configured neural network for logical AND function`() {
        val inputLayer = Layer(2)
        val outputLayer = Layer(1, inputLayer).setThetas(arrayOf(arrayOf(-3.0, 2.0, 2.0)))
        val network = NeuralNetwork(listOf(inputLayer, outputLayer))

        assertThat(network.process(0.0, 0.0)[0].round(), equalTo(0L))
        assertThat(network.process(0.0, 1.0)[0].round(), equalTo(0L))
        assertThat(network.process(1.0, 0.0)[0].round(), equalTo(0L))
        assertThat(network.process(1.0, 1.0)[0].round(), equalTo(1L))
    }

    @Test fun `pre-configured neural network for logical OR function`() {
        val inputLayer = Layer(2)
        val outputLayer = Layer(1, inputLayer).setThetas(arrayOf(arrayOf(-1.0, 2.0, 2.0)))
        val network = NeuralNetwork(listOf(inputLayer, outputLayer))

        assertThat(network.process(0.0, 0.0)[0].round(), equalTo(0L))
        assertThat(network.process(0.0, 1.0)[0].round(), equalTo(1L))
        assertThat(network.process(1.0, 0.0)[0].round(), equalTo(1L))
        assertThat(network.process(1.0, 1.0)[0].round(), equalTo(1L))
    }

    @Test fun `neural network to learn logical AND function`() {
        val random = Random(123)

        val inputLayer = Layer(2)
        val outputLayer = Layer(1, inputLayer).initThetas(random)
        val network = NeuralNetwork(listOf(inputLayer, outputLayer))

        val trainingExamples = listOf(
                Example(listOf(0.0, 0.0), listOf(0.0)),
                Example(listOf(0.0, 1.0), listOf(0.0)),
                Example(listOf(1.0, 0.0), listOf(0.0)),
                Example(listOf(1.0, 1.0), listOf(1.0))
        ).repeat(times = 200000).shuffle(random)

        val trainer = Trainer().train(trainingExamples, network)

        println(trainer.totalError)
        println(outputLayer.toString())

        assertThat(trainer.totalError, equalTo(-0.016766691280681305))
    }

    @Test fun `neural network to convert polar to cartesian coordinates`() {
        val random = Random(123)

        val inputLayer = Layer(2)
        val hiddenLayer = Layer(40, inputLayer, ::tanh, ::tanhDerivative).initThetas(random)
        val outputLayer = Layer(2, hiddenLayer, {it}, {1.0}).initThetas(random)
        val network = NeuralNetwork(listOf(inputLayer, hiddenLayer, outputLayer))

        val trainingExamples = Supplier<Example?> {
            val d1 = random.nextDouble()
            val d2 = random.nextDouble()
            Example(
                inputs = listOf(d1, d2),
                expectedOutputs = Pair(d1, d2).polarToCartesian().toList()
            )
        }

        val trainer = Trainer().train(network, trainingExamples, maxExamples = 15000)
        println(trainer.totalError)

        assertThat(trainer.totalError, equalTo(0.0011129625611150781))
    }


    private data class Example(val inputs: List<Double>, val expectedOutputs: List<Double>)


    private class Trainer() {
        var totalError: Double = 0.0

        fun train(examples: Collection<Example>, network: NeuralNetwork): Trainer {
            val iterator = examples.iterator()
            val supplier = Supplier {
                if (iterator.hasNext()) iterator.next() else null
            }
            return train(network, supplier)
        }

        fun train(network: NeuralNetwork, examplesSupplier: Supplier<Example?>, maxExamples: Long? = null): Trainer {
            val allErrors = mutableListOf<Double>()

            var i = 0
            var example = examplesSupplier.get()
            while (example != null && (maxExamples == null || i < maxExamples)) {

                val errors = network.learnFrom(example)

                allErrors.addAll(errors)
                if (i % 10000 == 0) {
                    println(allErrors.sum() / allErrors.size)
                }
                i++
                example = examplesSupplier.get()
            }
            totalError = allErrors.sum() / allErrors.size
            return this
        }
    }


    private class NeuralNetwork(val layers: List<Layer>) {
        private val inputLayer = layers.first()
        private val hiddenLayers = layers.drop(1)
        private val outputLayer = layers.last()

        fun process(vararg inputs: Double): Array<Double> {
            return process(inputs.toTypedArray())
        }

        fun process(inputs: Array<Double>): Array<Double> {
            inputLayer.setOutputs(inputs)
            hiddenLayers.forEach { it.activate() }
            return outputLayer.outputs
        }

        fun learnFrom(example: Example): Array<Double> {
            val actualOutputs = process(example.inputs.toTypedArray())

            val errors = example.expectedOutputs
                    .zip(actualOutputs)
                    .map { it.first - it.second }
                    .toTypedArray()

            hiddenLayers.foldRight(errors) { layer, errors ->
                layer.backPropagate(errors)
            }
            return errors
        }
    }


    private class Layer(val size: Int,
                        val inputLayer: Layer? = null,
                        val activation: (Double) -> Double = ::sigmoid,
                        val activationDerivative: (Double) -> Double = ::sigmoidDerivative,
                        val learningRate: Double = 0.01
    ) {
        val outputs = Array(size, { 0.0 })
        private val inputs = arrayOf(1.0) + Array(inputLayer?.size ?: 0, { 0.0 }) // prepend bias input
        private val inputThetas = Array(size, { Array(inputs.size, { 0.0 }) })
        private val inputsSum = Array(size, {0.0})

        fun initThetas(random: Random = Random()): Layer {
            0.until(size).forEach { cellIndex ->
                0.until(inputs.size).forEach { inputIndex ->
                    inputThetas[cellIndex][inputIndex] = random.nextDouble()
                }
            }
            return this
        }

        fun activate() {
            inputLayer?.outputs?.forEachIndexed { i, d ->
                inputs[i + 1] = d
            }

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
            if (inputLayer == null) return emptyArray()

            0.until(size).forEach { cellIndex ->
                0.until(inputs.size).forEach { inputIndex ->
                    val delta = learningRate * activationDerivative(inputsSum[cellIndex]) * errors[cellIndex] * inputs[inputIndex]
                    inputThetas[cellIndex][inputIndex] += delta
                }
            }

            val errorsForInputLayer = Array(inputLayer.outputs.size, { inputIndex ->
                var sum = 0.0
                errors.forEachIndexed { cellIndex, error ->
                    sum += error * inputThetas[cellIndex][inputIndex + 1]
                }
                sum
            })
            return errorsForInputLayer
        }

        fun setOutputs(newOutputs: Array<Double>): Layer {
            if (newOutputs.size != size) throw IllegalArgumentException()
            newOutputs.forEachIndexed { i, d -> outputs[i] = d }
            return this
        }

        fun setThetas(newThetas: Array<Array<Double>>): Layer {
            newThetas.forEachIndexed { cellIndex, values ->
                values.forEachIndexed { inputIndex, d ->
                    inputThetas[cellIndex][inputIndex] = d
                }
            }
            return this
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
private fun Double.round() = Math.round(this)

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