package ru.fractal

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Dimension}
import java.awt.image.BufferedImage
import org.junit.Test

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

class Mandelbrot0 {
  val imageWidth = 800
  val imageHeight = 600

  // TODO refactor this
  def calculateFractal(settings: FractalSettings): Array[Array[Int]] = {
    val pixelSize = 3.0 / imageWidth
    val leftCornerX = -0.5 - 0.5 * (imageWidth * pixelSize)
    val leftCornerY = 0 - 0.5 * (imageHeight * pixelSize)

    val maxIterations = 50
    val threshold = 4.0

    val result = Array.ofDim[Int](imageWidth, imageHeight)
    for (xPixel <- 0 until imageWidth; yPixel <- 0 until imageHeight) {
      val xc = leftCornerX + xPixel * pixelSize
      val yc = leftCornerY + yPixel * pixelSize

      var x = 0.0
      var y = 0.0
      var iteration = 0
      var unbounded = false
      while (iteration < maxIterations && !unbounded) {
        val tmp = x * x - y * y
        y = 2 * x * y
        x = tmp

        iteration += 1

        x += xc
        y += yc

        if (x * x + y * y >= threshold) {
          unbounded = true
        }
      }

      if (unbounded) {
        val colorScale = iteration.toDouble / maxIterations
        val color = (255 * colorScale).toInt
        result(xPixel)(yPixel) = new Color(color, color, color).getRGB
      }

    }
    result
  }
}

