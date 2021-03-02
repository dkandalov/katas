package katas.kotlin.regression

import datsok.shouldEqual
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.jfree.data.time.FixedMillisecond
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import org.junit.jupiter.api.Test
import kotlin.math.sin

class RegressionExamples {
    private val inputExamples = arrayOf(
        doubleArrayOf(1.0, 1.0),
        doubleArrayOf(2.0, 3.0),
        doubleArrayOf(8.0, 2.0),
    )
    private val outputExamples = doubleArrayOf(
        2.0,
        5.0,
        10.0,
    )

    @Test fun `how to use OLSMultipleLinearRegression`() {
        val regression = OLSMultipleLinearRegression()
        regression.newSampleData(outputExamples, inputExamples)
        val beta = regression.estimateRegressionParameters()

        val input = doubleArrayOf(3.0, 3.0)
        val predict = beta[0] + beta[1] * input[0] + beta[2] * input[1]
        predict shouldEqual 5.999999999999998
    }

    @Test fun `better API with Learner`() {
        Learner().let {
            it.learnFrom(inputExamples, outputExamples)
            it.predict(doubleArrayOf(3.0, 3.0)) shouldEqual 5.999999999999998
        }
    }
}

class SinPrediction {
    @Test fun `predict sin`() {
        val window = 100

        val learningData = (1..1000).map { sin(it.toDouble() / 100) * 100 + 100 }
        val examData = (1001..2000).map { sin(it.toDouble() / 100) * 100 + 100 }

        val examples = learningData.windowed(size = window + 1)
            .map { Example(input = it.take(window), output = it.last()) }
        val examplesForExam = examData.windowed(size = window + 1)
            .map { Example(input = it.take(window), output = it.last()) }

        val learner = Learner()
        learner.learnFrom(examples)
        val examOutputs = examplesForExam
            .map { (input, _) -> learner.predict(input.toDoubleArray()) }

        val chart = createChart(TimeSeriesCollection().apply {
            addSeries(TimeSeries("learningData").apply {
                learningData.indices.zip(learningData).map { (index, value) ->
                    add(FixedMillisecond(index.toLong()), value)
                }
            })
            addSeries(TimeSeries("examData").apply {
                examData.indices.zip(examData).map { (index, value) ->
                    add(FixedMillisecond((index + 1001).toLong()), value)
                }
            })
            addSeries(TimeSeries("examOutputs").apply {
                examOutputs.indices.zip(examOutputs).map { (index, value) ->
                    add(FixedMillisecond((index + 1100).toLong()), value)
                }
            })
        })
        chart.saveToSvg("foo.svg")
    }
}

data class Example(val input: List<Double>, val output: Double)

class Learner {
    private val regression = OLSMultipleLinearRegression()
    private var beta = doubleArrayOf()

    fun learnFrom(inputs: Array<DoubleArray>, outputs: DoubleArray) {
        require(inputs.size == outputs.size) {
            "Input size: ${inputs.size}; output size: ${outputs.size}"
        }
        regression.newSampleData(outputs, inputs)
        beta = regression.estimateRegressionParameters()
    }

    fun predict(input: DoubleArray): Double {
        require(beta.size - 1 == input.size)
        var result = 0.0
        beta.forEachIndexed { index, b ->
            result += if (index == 0) b else b * input[index - 1]
        }
        return result
    }
}

fun Learner.learnFrom(examples: List<Example>) {
    val learningInputs = examples.map { it.input.toDoubleArray() }.toTypedArray()
    val learningOutputs = examples.map { it.output }.toDoubleArray()
    learnFrom(learningInputs, learningOutputs)
}