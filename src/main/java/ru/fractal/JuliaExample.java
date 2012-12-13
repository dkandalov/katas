package ru.fractal;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
    private static final int WINDOW_WIDTH = 700;
    private static final int WINDOW_HEIGHT = 700;

    // The ComplexNumber to base the Julia Set off of.
    private static ComplexNumber juliaSetBase = new ComplexNumber(-0.223, 0.745);

    private static double xRange = 3;
    private static double xShift = 0;
    private static double yRange = 3;
    private static double yShift = 0;

    private static double threshold = 1;
    private static int iterations = 50;
    private static int gradientRange = 2000;

    private double[][] pointMagnitude;
    private BufferedImage image = null;
    private final JFrame frame;


    public static void main(String[] args) {
        new JuliaExample();
    }

    public JuliaExample() {
        calculateAndRepaintImage();

        frame = new JFrame("Julia Example") {
            @Override
            public void paint(Graphics g) {
                g.drawImage(image, 0, 0, null);
            }
        };
        frame.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent mouseEvent) {
                xRange /= 2;
                yRange /= 2;
                xShift += (((double) mouseEvent.getX() / WINDOW_WIDTH) * xRange) - xRange / 2;
                yShift += (((double) mouseEvent.getY() / WINDOW_HEIGHT) * yRange) - yRange / 2;

                System.out.println("xShift = " + xShift);
                System.out.println("yShift = " + yShift);
                System.out.println("xRange = " + xRange);
                System.out.println("yRange = " + yRange);

                calculateAndRepaintImage();
            }
        });
        frame.addKeyListener(new KeyAdapter() {
            @Override public void keyPressed(KeyEvent keyEvent) {
                if (keyEvent.getKeyCode() == KeyEvent.VK_UP) {
                    iterations += 10;
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_DOWN) {
                    iterations -= 10;
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_RIGHT) {
                    threshold += 10;
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_LEFT) {
                    threshold -= 10;
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_D) {
                    juliaSetBase = juliaSetBase.add(new ComplexNumber(0.01, 0));
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_A) {
                    juliaSetBase = juliaSetBase.add(new ComplexNumber(-0.01, 0));
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_W) {
                    juliaSetBase = juliaSetBase.add(new ComplexNumber(0, 0.01));
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_S) {
                    juliaSetBase = juliaSetBase.add(new ComplexNumber(0, -0.01));
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_R) {
                    gradientRange += 100;
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_F) {
                    gradientRange -= 100;
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_T) {
                    xRange += 0.1;
                    yRange += 0.1;
                } else if (keyEvent.getKeyCode() == KeyEvent.VK_G) {
                    xRange -= 0.1;
                    yRange -= 0.1;
                }

                System.out.println("iterations = " + iterations);
                System.out.println("threshold = " + threshold);
                System.out.println("juliaSetBase = " + juliaSetBase);
                System.out.println("gradientRange = " + gradientRange);
                System.out.println("xRange = " + xRange);
                System.out.println("yRange = " + yRange);

                calculateAndRepaintImage();
            }
        });
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setSize(WINDOW_WIDTH, WINDOW_HEIGHT);
        frame.repaint();
        frame.setVisible(true);
    }

    private void calculateAndRepaintImage() {
        pointMagnitude = calculateFractalPoints();
        image = createImage(pointMagnitude);
        if (frame != null) frame.repaint();
    }

    private BufferedImage createImage(double[][] pointInFractal) {
        BufferedImage image = new BufferedImage(WINDOW_WIDTH, WINDOW_HEIGHT, BufferedImage.TYPE_INT_RGB);
        for (int x = 0; x < WINDOW_WIDTH; x++) {
            for (int y = 0; y < WINDOW_HEIGHT; y++) {
                float value = (float) (pointInFractal[x][y] - threshold * threshold);
                if (Float.isNaN(value)) {
                    value = 0;
                } else {
                    value = value < -gradientRange / 2 ? -gradientRange / 2 : value;
                    value = value > gradientRange / 2 ? gradientRange / 2 : value;
                    value = (value + gradientRange / 2) * 1 / gradientRange;
                }

                Color color = new Color(value, value, value);
                image.setRGB(x, y, color.getRGB());
            }
        }
        return image;
    }

    private static double[][] calculateFractalPoints() {
        double[][] values = new double[WINDOW_WIDTH][WINDOW_HEIGHT];
        double minX = -xRange / 2;
        double minY = -yRange / 2;

        for (int x = 0; x < WINDOW_WIDTH; x++) {
            for (int y = 0; y < WINDOW_HEIGHT; y++) {
                /**
                 * Each pixel represents a ComplexNumber.  The pixel in the center
                 * represents 0,0 on the Complex Plane.  The following two lines
                 * treat the window as a scaled version of the bounds set above
                 * (See min and max vars above).  They scale the numbers, and
                 * shift everything around so it lies up right.
                 */
                double a = ((double) x * xRange / (double) WINDOW_WIDTH + minX) + xShift;
                double b = ((double) y * yRange / (double) WINDOW_HEIGHT + minY) + yShift;

                values[x][y] = fractalMagnitude(new ComplexNumber(a, b));
            }
        }

        return values;
    }

    /**
     * Determine if the ComplexNumber cn fits in the Fractal pattern.
     * This is the basic quadratic julia set.  The formula is: f(z+1) = z^2+c
     * where z is a complex number, and where c is a constant complex number.
     */
    private static double fractalMagnitude(ComplexNumber number) {
        for (int i = 0; i < iterations; i++) {
            number = number.square().add(juliaSetBase);
        }
        return number.magnitude();
    }

}


