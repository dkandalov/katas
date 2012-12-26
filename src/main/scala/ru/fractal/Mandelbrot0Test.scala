package ru.fractal

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import java.io._
import scala.io.Source

class Mandelbrot0Test {
  val ExpectedOutputFile = "src/ru/fractal/expected_output.csv"
  val ActualOutputFile = "src/ru/fractal/actual_output.csv"

  @Test def shouldMatchPreviousResult() {
    val fractal = (new Mandelbrot0).calculateFractal(FractalSettings())

    if (fileWithExpectedOutputExists) {
      val expected = readExpectedOutputFromFile()
      val actual = fractalAsString(fractal)
      assertThatAreEqual(actual, expected)
    } else {
      writeToFile(ExpectedOutputFile, fractalAsString(fractal))
      fail("Created new output file")
    }
  }

  private def assertThatAreEqual(actual: String, expected: String) {
    try {
      assertThat(actual, equalTo(expected))
    }
    catch {
      case error: AssertionError => {
        writeToFile(ActualOutputFile, actual)
        throw error
      }
    }
  }

  private def fileWithExpectedOutputExists = new File(ExpectedOutputFile).exists()

  private def readExpectedOutputFromFile(): String = Source.fromFile(ExpectedOutputFile).getLines().toList.mkString("\n")

  private def fractalAsString(fractal: Array[Array[Int]]): String = {
    (for (column <- fractal) yield column.map(roundToHundreds).mkString(",")).mkString("\n")
  }

  private def writeToFile(fileName: String, text: String) {
    val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))
    output.write(text)
    output.close()
  }

  // there is no particular reason why it's hundreds (it's just a guess)
  private def roundToHundreds(value: Int) = (value / 100) * 100 
}
