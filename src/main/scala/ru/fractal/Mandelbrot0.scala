package ru.fractal

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Dimension}
import java.awt.image.BufferedImage
import scala.None
import annotation.tailrec

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
}

case class FractalSettings() // TODO use it

case class Complex(a: Double, b: Double) {
  def +(that: Complex) =  Complex(a + that.a, b + that.b)
  def square = Complex(a * a - b * b, 2 * a * b)
  def squareOfAbs = a * a + b * b
}

class Mandelbrot0 {
  val imageWidth = 800
  val imageHeight = 600

  def calculateFractal(settings: FractalSettings): Array[Array[Int]] = {
    val pixelScale = 3.0 / imageWidth
    val topLeftCorner = Complex(
      -0.5 - 0.5 * (imageWidth * pixelScale),
      0 - 0.5 * (imageHeight * pixelScale)
    )

    val maxIterations = 50
    val threshold = 4.0

    val result = Array.ofDim[Int](imageWidth, imageHeight)
    for (xPixel <- 0 until imageWidth; yPixel <- 0 until imageHeight) {
      val pointC = topLeftCorner + Complex(xPixel * pixelScale, yPixel * pixelScale)
      val point = Complex(0, 0)

      iterate(point, pointC, threshold, maxIterations) match {
        case Some(iteration) =>
          result(xPixel)(yPixel) = rgbColorFromIteration(iteration, maxIterations)
        case None => // do nothing
      }
    }
    result
  }

  private def rgbColorFromIteration(iteration: Int, maxIterations: Int): Int = {
    val colorScale = iteration.toDouble / maxIterations
    val color = (255 * colorScale).toInt
    new Color(color, color, color).getRGB
  }

  @tailrec private def iterate(point: Complex, pointC: Complex, threshold: Double, maxIterations: Int, iteration: Int = 0): Option[Int] = {
    if (iteration >= maxIterations) None
    else if (point.squareOfAbs >= threshold) Some(iteration)
    else iterate(point.square + pointC, pointC, threshold, maxIterations, iteration + 1)
  }
}

