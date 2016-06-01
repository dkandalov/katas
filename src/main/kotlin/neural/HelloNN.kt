package neural

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.io.File
import java.util.*

class HelloNN {
    @Test fun `neural network to convert polar to cartesian coordinates`() {
        val activation_function = { x: Double ->
            (Math.exp(x) - Math.exp(-x)) / (Math.exp(x) + Math.exp(-x))
        }
        val derivative_of_activation_function = { x: Double ->
            val d = activation_function(x)
            1.0 - (d * d)
        }

        var inputLayer = Array(2, { 0.0 })
        val hiddenLayer = Layer(40, inputLayer, activation_function, derivative_of_activation_function).init(234)
        val outputLayer = Layer(2, hiddenLayer.outputs, {it}, {1.0}).init(345)

        val file = File("/tmp/nn.csv")
        file.delete()
        file.createNewFile()
        file.appendText("i,i,err1,err2\n")

        val allErrors = mutableListOf<Double>()

        val random = Random(123)
        0.until(150000).forEach { attempt ->
            inputLayer = inputLayer.apply {
                this[0] = random.nextDouble()
                this[1] = random.nextDouble() * 2 * Math.PI
            }

            hiddenLayer.activate()
            outputLayer.activate()

            val targetOutputs = Array(2, { 0.0 }).apply {
                val target = Pair(inputLayer[0], inputLayer[1]).polarToCartesian()
                this[0] = target.first
                this[1] = target.second
            }
            val outputErrors = targetOutputs.mapIndexed{ i, d -> d - outputLayer.outputs[i] }.toTypedArray()
            val errors = outputLayer.backPropagate(outputErrors)
            hiddenLayer.backPropagate(errors)
//            file.appendText(attempt.toString() + "," + attempt.toString() + "," + outputErrors.toList().joinToString(",") + "\n")

            allErrors.add(Math.sqrt(outputErrors.map{it * it}.sum()))
            if (attempt % 10000 == 0) {
                println(allErrors.sum() / allErrors.size)
            }
        }
    }

    private class Layer(val size: Int,
                        val inputsWithoutBias: Array<Double>,
                        val activation: (Double) -> Double = { sigmoid(it) },
                        val activationDerivative: (Double) -> Double = { sigmoidDerivative(it) }
    ) {
        private val learningRate = 0.01
        private val inputs = Array(inputsWithoutBias.size, {
            if (it == 0) 1.0 // bias
            else inputsWithoutBias[it - 1]
        })
        private val inputsSum = Array(size, {0.0})
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
                inputThetas[cellIndex] = Array(inputs.size, { inputIndex ->
                    val delta = learningRate * activationDerivative(inputsSum[cellIndex]) * errors[cellIndex] * inputs[inputIndex]
                    inputThetas[cellIndex][inputIndex] + delta
                })
            }

            val errorsForInputLayer = Array(inputs.size, { inputIndex ->
                var sum = 0.0
                errors.forEachIndexed { cellIndex, error ->
                    sum += error * inputThetas[cellIndex][inputIndex]
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
