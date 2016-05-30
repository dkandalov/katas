package neural

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.util.*

class HelloNN {
    @Test fun `neural network to convert polar to cartesian coordinates`() {
        val random = Random(123)

        var inputLayer = Array<Double>(3, { 0.0 }).apply{
            this[0] = random.nextDouble() // bias
        }
        val hiddenLayer = Layer(8, inputLayer).init(234)
        val outputLayer = Layer(2, hiddenLayer.outputs).init(345)

        0.until(30000).forEach { attempt ->
            inputLayer = inputLayer.apply {
                this[1] = random.nextDouble()
                this[2] = random.nextDouble()
            }
            hiddenLayer.activate()
            outputLayer.activate()

            val targetOutputs = Array<Double>(2, { 0.0 }).apply {
                val target = Pair(inputLayer[1], inputLayer[2]).cartesianToPolar()
                this[0] = target.first
                this[1] = target.second
            }
            val outputDiffs = targetOutputs.mapIndexed { i, d -> d - outputLayer.outputs[i] }.toTypedArray()
            val weightedDiffs = outputLayer.backPropagate(outputDiffs)
            hiddenLayer.backPropagate(weightedDiffs)

            if (attempt % 100 == 0) {
                println(outputDiffs.toList())
            }
        }
    }

    private class Layer(val size: Int, val inputs: Array<Double>,
                        val activation: (Double) -> Double = { sigmoid(it) },
                        val activationDerivative: (Double) -> Double = { sigmoidDerivative(it) }
    ) {
        private val rate = 0.01
        private val inputThetas = Array(size, { Array(inputs.size, { 0.0 }) })
        val outputs = Array(size, { 0.0 })

        fun init(seed: Long? = null): Layer {
            val random = if (seed == null) Random() else Random(seed)
            inputThetas.forEach { thetas ->
                0.until(inputs.size).forEach { inputIndex ->
                    thetas[inputIndex] = random.nextDouble()
                }
            }
            return this
        }

        fun activate(): Layer {
            0.until(outputs.size).forEach { cellIndex ->
                var sum = 0.0
                0.until(inputs.size).forEach { inputIndex ->
                    sum += inputs[inputIndex] * inputThetas[cellIndex][inputIndex]
                }
                outputs[cellIndex] = activation(sum)
            }
            return this
        }

        fun backPropagate(outputDiffs: Array<Double>): Array<Double> {
            val weightedDiffs = Array(inputs.size, { inputIndex ->
                var sum = 0.0
                outputDiffs.forEachIndexed { cellIndex, outputDiff ->
                    sum += outputDiff * inputThetas[cellIndex][inputIndex]
                }
                sum
            })

            val outputDeltas = Array(size, { cellIndex ->
                activationDerivative(outputs[cellIndex]) * outputDiffs[cellIndex]
            })
            0.until(size).forEach { cellIndex ->
                inputThetas[cellIndex] = Array(inputs.size, { inputIndex ->
                    if (inputIndex == 0) inputThetas[cellIndex][inputIndex] // don't update bias theta
                    else inputThetas[cellIndex][inputIndex] + rate * outputDeltas[cellIndex]
                })
            }
            return weightedDiffs
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
    return sigmoid(x) * (1.0 - sigmoid(x))
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

private fun Double.toDegree(): Double = this * (180.0 / Math.PI)
private fun Double.toRadian(): Double = this * (Math.PI / 180.0)
