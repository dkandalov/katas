package ru.fractal;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * The Julia Fractal family is my personal favorite.  This class is to show and
 * explain the math behind it.  It can be very complicated, so I will try to
 * explain it as best as I can.  This is NOT meant to be a comrehensive guide
 * to Fractals, or the Julia Set.  This is only to help you understand how it 
 * works.  This is a VERY simple example, and is for educational purposes only.
 * Please do not claim this as your own, or submit it as your own work.
 *
 * Please excuse any spelling errors you may find.  I'm a programmer, not a speller.
 *
 * Complex Numbers are the key to fractals.  A complex number is in the form:
 * a+bi, where a & b are real numbers, and i = sqrt(-1).
 *
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
 *
 * The i-values (imaginary values) go up & down the "y" axis, and the real numbers
 * go out along the "x" axis.  (Note that I only drew the one quadrant, all four
 * are used).
 *
 * The Julia Set is created by putting each point through an algorithm, and adding
 * it to the set by determining how close it is to infinity.  If the threshold^2
 * is less than the Magnitude of the Complex Number (where magnitude = a^2 + b^2),
 * the point is added to the set.  The iteration value determins how many times the 
 * point is put through the algorithm.  Putting it through more times, and making 
 * the threshold much smaller, leads to cleaner images, but they take longer to create.
 *
 * The values in this example give a good depiction of a Julia Set.
 *
 * @author Neil
 */
public class JuliaExample {

    // The width and height of the window.
    private static final int WIDTH = 350;
    private static final int HEIGHT = 350;
    // The ComplexNumber to base the Julia Set off of.
    private static ComplexNumber c = new ComplexNumber(-0.223, 0.745);

    // Array of booleans stating which pixels make up the fractal.
    private boolean[][] values = null;

    // The bounds of the Complex Plane to graph.
    private double minX = -1.5;
    private double maxX = 1.5;
    private double minY = -1.5;
    private double maxY = 1.5;

    // The image to draw on.
    private BufferedImage image = null;

    // The maximum Magnitude of the ComplexNumer to be allowed in the Set.
    private double threshold = 1;
    // The number of times that the algorithm recurses.
    private int iterations = 50;

    /**
     * The meat of the program is in the constructor.
     * This handles the Frame, filling the array, painting,
     * and coloring.
     */
    public JuliaExample(){
        // Create a BufferedImage to paint on.
        image = new BufferedImage(WIDTH,HEIGHT,BufferedImage.TYPE_INT_RGB);

        // Fill the booean[][].
        getValues();
        // Go through the array and set the pixel color on the BufferedImage 
        // according to the values in the array.
        for(int i=0;i<WIDTH;i++){
            for(int j=0;j<HEIGHT;j++){
                // If the point is in the Set, color it White, else, color it Black.
                if(values[i][j]) image.setRGB(i, j, Color.WHITE.getRGB());
                if(!values[i][j]) image.setRGB(i, j, Color.BLACK.getRGB());
            }
        }

        // Create a Frame to display the Fractal.
        JFrame f = new JFrame("Julia Example"){
            // Override the paint method (for simplicity)
            @Override
            public void paint(java.awt.Graphics g){
                g.drawImage(image,0,0,null);
            }
        };
        // Set other Frame settings.
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setSize(WIDTH,HEIGHT);
        f.repaint();
        f.setVisible(true);
    }

    /**
     * Fill the values boolean[][] with data.
     */
    private void getValues(){
        values = new boolean[WIDTH][HEIGHT];
        // Go through each pixel.
        for(int i=0;i<WIDTH;i++){
            for(int j=0;j<HEIGHT;j++){
                /**
                 * Each pixel represents a ComplexNumber.  The pixel in the center
                 * represents 0,0 on the Complex Plane.  The following two lines
                 * treat the window as a scaled version of the bounds set above
                 * (See min and max vars above).  They scale the numbers, and 
                 * shift everything around so it lies up right.
                 */
                double a = (double)i*(maxX-minX)/(double)WIDTH + minX;
                double b = (double)j*(maxY-minY)/(double)HEIGHT + minY;
                // fill the boolean array.
                values[i][j] = isInSet(new ComplexNumber(a,b));
            }
        }
    }

    /**
     * Determine if the ComplexNumber cn fits in the Fractal pattern.
     * This is the basic quadratic julia set.  The formula is:
     * f(z+1) = z^2+c
     * where z is a complex number,
     * and where c is a constant complex number.
     * @param cn The ComplexNumber to check.
     * @return true if it is in the set, else false.
     */
    private boolean isInSet(ComplexNumber cn){
        for(int i=0;i<iterations;i++){
            // The basic Julia Set Algorithm.
            // Other Algorithms can be found online.
            cn = cn.square().add(c);
        }
        // If the threshold^2 is larger than the magnitude, return true.
        return cn.magnitude()<threshold*threshold;
    }

    /**
     * The Method the execute this program
     * @param args Command Line args
     */
    public static void main(String[] args){
        new JuliaExample();
    }
}


class ComplexNumber{

    private double a, b;
    // Create a Complex Number with the given real numbers.
    public ComplexNumber(double a, double b){
        this.a = a;
        this.b = b;
    }

    // Method for squaring a ComplexNumber
    public ComplexNumber square(){
        return new ComplexNumber(this.a*this.a - this.b*this.b, 2*this.a*this.b);
    }

    // Method for adding 2 complex numbers
    public ComplexNumber add(ComplexNumber cn){
        return new ComplexNumber(this.a+cn.a, this.b+cn.b);
    }

    // Method for calculating magnitude^2 (how close the number is to infinity)
    public double magnitude(){
        return a*a+b*b;
    }
}