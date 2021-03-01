package katas.kotlin.regression

import datsok.shouldEqual
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.junit.jupiter.api.Test

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

class Learner {
    private val regression = OLSMultipleLinearRegression()
    private var beta = doubleArrayOf()

    fun learnFrom(inputExamples: Array<DoubleArray>, outputExamples: DoubleArray) {
        regression.newSampleData(outputExamples, inputExamples)
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