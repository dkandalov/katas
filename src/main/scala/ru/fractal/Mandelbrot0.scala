package ru.fractal

import swing.{Panel, Button, MainFrame, SimpleSwingApplication}
import java.awt.Dimension
import java.awt.image.BufferedImage

/**
 * User: dima
 * Date: 14/12/2012
 */

object Mandelbrot0 extends SimpleSwingApplication {
	def top = new MainFrame {
		title = "Hello, Mandelbrot0!"

		val mandelbrot = new Mandelbrot0
		val panel = new Panel {
			override protected def paintComponent(g: scala.swing.Graphics2D) {
				val fractal = mandelbrot.calculateFractal(FractalSettings())

				val image: BufferedImage = new BufferedImage(800, 800, BufferedImage.TYPE_INT_RGB)
				for (x <- 0 until 800; y <- 0 until 800) {
					val color = fractal(y)(x)
					image.setRGB(x, y, color)
				}

				g.drawImage(image, 0, 0, null)
			}
		}

		panel.preferredSize = new Dimension(800, 800)
		contents = panel
	}
}

case class FractalSettings()

class Mandelbrot0 {
	def calculateFractal(settings: FractalSettings): Array[Array[Int]] = {
		Array.fill(800){Array.fill(800) { 0 }}
	}
}