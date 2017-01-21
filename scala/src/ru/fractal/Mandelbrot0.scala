package ru.fractal

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Dimension}
import java.awt.image.BufferedImage
import scala.None
import annotation.tailrec
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import scala.Some
import java.io.{FileOutputStream, OutputStreamWriter, BufferedWriter, File}
import scala.io.Source

/**
 * User: dima
 * Date: 14/12/2012
 */

object Mandelbrot0 extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Hello, Mandelbrot0!"

    val mandelbrot = new Mandelbrot0
    val settings = FractalSettings()
    val fractal = mandelbrot.calculateFractal(settings)

    val panel = new Panel {
      override protected def paintComponent(g: scala.swing.Graphics2D) {
        val image: BufferedImage = new BufferedImage(800, 600, BufferedImage.TYPE_INT_RGB)
        for (x <- 0 until 800; y <- 0 until 600) {
          val color = fractal(x)(y)
          image.setRGB(x, y, color)
        }

        g.drawImage(image, 0, 0, null)
      }
    }

    panel.preferredSize = new Dimension(800, 600)
    contents = panel
  }

	case class FractalSettings(
	                          imageWidth: Int = 800, imageHeight: Int = 600, maxIterations: Int = 50,
	                           iterationThreshold: Double = 4.0, scaleByWidth: Double = 3.0, leftX: Double = -0.5,
	                           topY: Double = 0.0) {
		val pixelScale: Double = scaleByWidth / imageWidth
		val topLeftCorner: Complex = Complex(
			leftX - 0.5 * (imageWidth * pixelScale),
			topY - 0.5 * (imageHeight * pixelScale)
		)
	}


	case class Complex(a: Double, b: Double) {
		def +(that: Complex) =  Complex(a + that.a, b + that.b)
		def square = Complex(a * a - b * b, 2 * a * b)
		def squareOfAbs = a * a + b * b
		def abs = math.sqrt(a * a + b * b)
	}

	class Mandelbrot0 {
		def calculateFractal(settings: FractalSettings): Array[Array[Int]] = {
			import settings._
			val result = Array.ofDim[Int](imageWidth, imageHeight)

			for (xPixel <- 0 until imageWidth; yPixel <- 0 until imageHeight) {
				val constant = topLeftCorner + Complex(xPixel * pixelScale, yPixel * pixelScale)
				val value = Complex(0, 0)
				iterateOver(value, constant, iterationThreshold, maxIterations) match {
					case Some((resultValue, iteration)) =>
						result(xPixel)(yPixel) = grayScaleColorByIteration(iteration, maxIterations)
					//          result(xPixel)(yPixel) = grayScaleColorByMagnitude(resultValue, maxIterations)
					//          result(xPixel)(yPixel) = redColorByMagnitude(resultValue, maxIterations)
					case None => // do nothing
				}
			}
			result
		}

		private def grayScaleColorByIteration(iteration: Int, maxIterations: Int): Int = {
			val colorScale = iteration.toDouble / maxIterations
			val color = (255 * colorScale).toInt
			new Color(color, color, color).getRGB
		}

		val zm = math.sqrt(3 * 3 + 2 * 2) // this is based on approximate max x, y values but does it really have to be?

		private def grayScaleColorByMagnitude(value: Complex, maxIterations: Int): Int = {
			val colorScale = math.min(1, value.abs / zm)
			val color = (255 * colorScale).toInt
			new Color(color, color, color).getRGB
		}

		private def redColorByMagnitude(value: Complex, maxIterations: Int): Int = {
			val colorScale = math.min(1, value.a.abs / zm)
			val color1 = math.min(255, 255 * 2 * colorScale).toInt
			val color2 = math.max(0, (255 * colorScale - 1)).toInt
			new Color(color1, color2, color2).getRGB
		}

		@tailrec private def iterateOver(value: Complex, constant: Complex, threshold: Double, maxIterations: Int, iteration: Int = 0): Option[(Complex, Int)] = {
			if (iteration >= maxIterations) None
			else if (value.squareOfAbs >= threshold) Some((value, iteration))
			else iterateOver(value.square + constant, constant, threshold, maxIterations, iteration + 1)
		}
	}

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

}
