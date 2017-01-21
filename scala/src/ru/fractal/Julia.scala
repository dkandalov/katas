package ru.fractal

import javax.swing.{JPanel, JFrame}
import java.awt.{Color, Graphics, Dimension}
import java.awt.event.{KeyEvent, KeyAdapter}
import annotation.tailrec


/**
 * Created at scala coding dojo on 18/12/2012
 */

object Julia {
	def main(args: Array[String]) {
		val frame = new JFrame("Julia set")
		val panel = new MyPanel
		frame.addKeyListener(new KeyAdapter {
			override def keyPressed(keyEvent: KeyEvent) {
				panel.onKey(keyEvent)
				frame.repaint()
			}
		})
		frame.add(panel)
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		frame.pack()
		frame.setVisible(true)
	}
}

case class Complex(r: Double, i: Double) {
	def *(that: Complex) = Complex(r * that.r - i * that.i, r * that.i + i * that.r)
	def sqare() = Complex(r * r - i * i, 2 * r * i)
	def +(that: Complex) = Complex(r + that.r, i + that.i)
	def abs(): Double = r * r + i * i
}

class MyPanel extends JPanel {
	setPreferredSize(new Dimension(800, 800))

	var c = Complex(0.285, 0)
	var threshold = 4.0
	var pixelScale = 10.0 / 800
	val maxIterations = 128

	def onKey(keyEvent: KeyEvent) {
		if (keyEvent.getKeyCode == KeyEvent.VK_I) c += new Complex(0.5, 0.5)
		else if (keyEvent.getKeyCode == KeyEvent.VK_K) c += new Complex(-0.5, -0.5)
		else if (keyEvent.getKeyCode == KeyEvent.VK_J) threshold += 1
		else if (keyEvent.getKeyCode == KeyEvent.VK_L) threshold -= 1
		else if (keyEvent.getKeyCode == KeyEvent.VK_UP) pixelScale *= 0.5
		else if (keyEvent.getKeyCode == KeyEvent.VK_DOWN) pixelScale /= 0.5
	}

	override def paintComponent(g: Graphics) {
		def drawPixelAt(x: Int, y: Int, color: Color) {
			g.setColor(color)
			g.drawRect(x, y, 1, 1)
		}

		for (xPixel <- 0 until 800) {
			for (yPixel <- 0 until 800) {
				val x = -4.5 + xPixel * pixelScale
				val y = -4.5 + yPixel * pixelScale

				iterate(Complex(x, y)) match {
					case Some((z, iterationCount)) => drawPixelAt(xPixel, yPixel, colorByIteration(iterationCount))
					case None => // do nothing
				}

			}
		}
	}

	private def calculateColor(complex: Complex): Color = {
		val value = math.max(0, math.min(255, (math.log10(0.001 * complex.abs())).toInt))
		val value2 = math.max(0, math.min(255, (math.log(0.000001 * complex.abs())).toInt))
		val value3 = math.max(0, math.min(255, (math.log(complex.abs())).toInt))
		new Color(value, value2, value3)
	}

	private def colorByIteration(iteration: Int): Color = {
		val value = math.max(0, math.min(255, iteration * (255.0 / maxIterations)).toInt)
		new Color(value, value, value)
//		Color.getHSBColor(iteration % 256, 255, if (iteration < maxIterations) 255 else 0)
	}

	@tailrec private def iterate(z: Complex, iterationCount: Int = 0): Option[(Complex, Int)] = {
		if (iterationCount > maxIterations) Some((z, iterationCount))
		else if (z.abs() > threshold) Some((z, iterationCount))
		else iterate(z * z + c, iterationCount + 1)
	}


}