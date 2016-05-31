package neural

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.io.File
import java.util.*

class HelloNN {
    @Test fun `neural network to convert polar to cartesian coordinates`() {
        val random = Random(123)

        var inputLayer = Array(2, { 0.0 })
        val hiddenLayer = Layer(8, inputLayer).init(234)
        val outputLayer = Layer(2, hiddenLayer.outputs).init(345)

        val file = File("/tmp/nn.csv")
        file.delete()
        file.createNewFile()
        file.appendText("i,i,err1,err2\n")

        0.until(15000).forEach { attempt ->
            inputLayer = inputLayer.apply {
                this[0] = random.nextDouble()
                this[1] = random.nextDouble()
            }
            hiddenLayer.activate()
            outputLayer.activate()

            val targetOutputs = Array(2, { 0.0 }).apply {
                val target = Pair(inputLayer[0], inputLayer[1]).cartesianToPolar()
                this[0] = target.first
                this[1] = target.second
            }
            val outputDiffs = targetOutputs.mapIndexed { i, d -> d - outputLayer.outputs[i] }.toTypedArray()
            val weightedDiffs = outputLayer.backPropagate(outputDiffs)
            hiddenLayer.backPropagate(weightedDiffs)

            file.appendText(attempt.toString() + "," + attempt.toString() + "," + outputDiffs.toList().joinToString(",") + "\n")
        }
        println(hiddenLayer)
        println(outputLayer)
    }

    private class Layer(val size: Int,
                        val inputs: Array<Double>,
                        val activation: (Double) -> Double = { sigmoid(it) },
                        val activationDerivative: (Double) -> Double = { sigmoidDerivative(it) }
    ) {
        private val rate = 0.01
        private val inputsWithBias = Array(inputs.size, {
            if (it == 0) 1.0 // bias
            else inputs[it - 1]
        })
        private val inputThetas = Array(size, { Array(inputsWithBias.size, { 0.0 }) })
        val outputs = Array(size, { 0.0 })

        fun init(seed: Long? = null): Layer {
            val random = if (seed == null) Random() else Random(seed)
            inputThetas.forEach { thetas ->
                0.until(inputsWithBias.size).forEach { inputIndex ->
                    thetas[inputIndex] = random.nextDouble()
                }
            }
            return this
        }

        fun activate(): Layer {
            0.until(outputs.size).forEach { cellIndex ->
                var sum = 0.0
                0.until(inputsWithBias.size).forEach { inputIndex ->
                    sum += inputsWithBias[inputIndex] * inputThetas[cellIndex][inputIndex]
                }
                outputs[cellIndex] = activation(sum)
            }
            return this
        }

        fun backPropagate(outputDiffs: Array<Double>): Array<Double> {
            val weightedDiffs = Array(inputsWithBias.size, { inputIndex ->
                var sum = 0.0
                outputDiffs.forEachIndexed { cellIndex, outputDiff ->
                    sum += outputDiff * inputThetas[cellIndex][inputIndex]
                }
                sum
            })

            0.until(size).forEach { cellIndex ->
                val delta = activationDerivative(outputs[cellIndex]) * outputDiffs[cellIndex]
                inputThetas[cellIndex] = Array(inputsWithBias.size, { inputIndex ->
                    inputThetas[cellIndex][inputIndex] + rate * delta
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
