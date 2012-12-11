package ru.fractal;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * The Julia Fractal family is my personal favorite.  This class is to show and
 * explain the math behind it.  It can be very complicated, so I will try to
 * explain it as best as I can.  This is NOT meant to be a comprehensive guide
 * to Fractals, or the Julia Set.  This is only to help you understand how it
 * works.  This is a VERY simple example, and is for educational purposes only.
 * Please do not claim this as your own, or submit it as your own work.
 * <p/>
 * Please excuse any spelling errors you may find.  I'm a programmer, not a speller.
 * <p/>
 * Complex Numbers are the key to fractals.  A complex number is in the form:
 * a+bi, where a & b are real numbers, and i = sqrt(-1).
 * <p/>
 * Generating fractals involves doing math in the Complex Plane.  You can think
 * of the Complex Plane in the following way:
 * i-values
 * |
 * |* (1+6i)
 * |
 * |  * (2+4i)
 * |
 * |      *(x+yi)
 * |_ _ _ _ _ _ real values.
 * <p/>
 * The i-values (imaginary values) go up & down the "y" axis, and the real numbers
 * go out along the "x" axis.  (Note that I only drew the one quadrant, all four
 * are used).
 * <p/>
 * The Julia Set is created by putting each point through an algorithm, and adding
 * it to the set by determining how close it is to infinity.  If the threshold^2
 * is less than the Magnitude of the Complex Number (where magnitude = a^2 + b^2),
 * the point is added to the set.  The iteration value determins how many times the
 * point is put through the algorithm.  Putting it through more times, and making
 * the threshold much smaller, leads to cleaner images, but they take longer to create.
 * <p/>
 * The values in this example give a good depiction of a Julia Set.
 *
 * @author Neil
 */
public class JuliaExample {
    private static final int WIDTH = 700;
    private static final int HEIGHT = 700;
    // The ComplexNumber to base the Julia Set off of.
    private static ComplexNumber juliaSetBase = new ComplexNumber(-0.223, 0.745);

    private static final double minX = -1.5;
    private static final double maxX = 1.5;
    private static final double minY = -1.5;
    private static final double maxY = 1.5;

    private BufferedImage image = null;

    // The maximum Magnitude of the ComplexNumber to be allowed in the Set.
    private static final double threshold = 1;
    // The number of times that the algorithm recurses.
    private static final int iterations = 50;


    public static void main(String[] args) {
        new JuliaExample();
    }

    public JuliaExample() {
        image = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB);

        boolean[][] isPointInFractal = calculateFractalPoints();

        for (int x = 0; x < WIDTH; x++) {
            for (int y = 0; y < HEIGHT; y++) {
                int color = isPointInFractal[x][y] ? Color.WHITE.getRGB() : Color.BLACK.getRGB();
                image.setRGB(x, y, color);
            }
        }

        JFrame f = new JFrame("Julia Example") {
            @Override
            public void paint(java.awt.Graphics g) {
                g.drawImage(image, 0, 0, null);
            }
        };
        f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        f.setSize(WIDTH, HEIGHT);
        f.repaint();
        f.setVisible(true);
    }

    private static boolean[][] calculateFractalPoints() {
        boolean[][] values = new boolean[WIDTH][HEIGHT];

        for (int x = 0; x < WIDTH; x++) {
            for (int y = 0; y < HEIGHT; y++) {
                /**
                 * Each pixel represents a ComplexNumber.  The pixel in the center
                 * represents 0,0 on the Complex Plane.  The following two lines
                 * treat the window as a scaled version of the bounds set above
                 * (See min and max vars above).  They scale the numbers, and
                 * shift everything around so it lies up right.
                 */
                double a = (double) x * (maxX - minX) / (double) WIDTH + minX;
                double b = (double) y * (maxY - minY) / (double) HEIGHT + minY;

                values[x][y] = isInFractal(new ComplexNumber(a, b));
            }
        }

        return values;
    }

    /**
     * Determine if the ComplexNumber cn fits in the Fractal pattern.
     * This is the basic quadratic julia set.  The formula is: f(z+1) = z^2+c
     * where z is a complex number, and where c is a constant complex number.
     */
    private static boolean isInFractal(ComplexNumber number) {
        for (int i = 0; i < iterations; i++) {
            number = number.square().add(juliaSetBase);
        }
        return number.magnitude() < threshold * threshold;
    }

    private static class ComplexNumber {
        private final double a;
        private final double b;

        public ComplexNumber(double a, double b) {
            this.a = a;
            this.b = b;
        }

        public ComplexNumber square() {
            return new ComplexNumber(a * a - b * b, 2 * a * b);
        }

        public ComplexNumber add(ComplexNumber number) {
            return new ComplexNumber(a + number.a, b + number.b);
        }

        public double magnitude() {
            return a * a + b * b;
        }
    }
}


