package katas.java.fractal;


import javax.swing.*;
import java.awt.*;

import static java.lang.Math.*;


/**
 * Originally: Mandelbrot and Julia Set Fractal Java Applet, 6/9/01
 */
public class MandelbrotExApplet extends JPanel {
    Graphics graphics;
    boolean isJuliaSet;

    int imageWidth;
    int imageHeight;
    private int imageXShift = 10;
    private int imageYShift = 10;

    int maxIterations = 50;
    int coloringMethod = 0;
    int selectedColor = 0;

    double centerX = -0.5;
    double centerY = 0;
    double dpi;
    double power = 2;
    double xc = 0;
    double yc = 0;

    double threshold = 4.0;

    TextField tf1;
    TextField tf2;
    TextField tf3;
    TextField tf4;
    TextField tf5;
    TextField tf6;
    TextField tf7;
    Choice choice1 = new Choice();
    Choice choice2 = new Choice();
    Choice choice3 = new Choice();
    Button button1 = new Button("Generate Image");

    public static void main(String[] args) {
        JFrame jFrame = new JFrame();
        jFrame.add(new MandelbrotExApplet());
        jFrame.setSize(1000, 1000);
        jFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        jFrame.setVisible(true);
    }

    public MandelbrotExApplet() {
        imageWidth = 1000 - 20;
        imageHeight = 1000 - 180;
        dpi = imageWidth / 3;

        setLayout(null);

        choice1.addItem("Mandelbrot Set");
        choice1.addItem("Julia Set");
        choice1.select(0);
        choice1.setBounds(10, imageHeight + 20, 102, 25);
        add(choice1);

        tf4 = createEditbox("xc", xc, 122, imageHeight + 20, 20);
        tf5 = createEditbox("yc", yc, 228, imageHeight + 20, 20);
        tf1 = createEditbox("dpi", dpi, 10, imageHeight + 55, 50);
        tf6 = createEditbox("centerX", centerX, 122, imageHeight + 55, 20);
        tf7 = createEditbox("centerY", centerY, 228, imageHeight + 55, 20);

        tf2 = createEditbox("max iterations", 0.0, 10, imageHeight + 90, 72);
        tf2.setText(String.valueOf(maxIterations));

        choice2.addItem("iterations");
        choice2.addItem("magnitude");
        choice2.addItem("real");
        choice2.addItem("imaginary");
        choice2.setBounds(122, imageHeight + 90, 102, 25);
        add(choice2);

        choice3.addItem("red");
        choice3.addItem("green");
        choice3.addItem("blue");
        choice3.addItem("yellow");
        choice3.addItem("cyan");
        choice3.addItem("magenta");
        choice3.addItem("white");
        choice3.setBounds(228, imageHeight + 90, 102, 25);
        add(choice3);

        tf3 = createEditbox("power", power, 10, imageHeight + 125, 50);
        button1.setBounds(228, imageHeight + 125, 102, 25);
        add(button1);
    }

    private TextField createEditbox(String str, double value, int x, int y, int w) {
        Label label = new Label(str + ":");
        label.setBounds(x, y, w, 25);
        add(label);
        TextField tf = new TextField(20);
        tf.setBounds(x + w, y, 102 - w, 25);
        tf.setText(String.valueOf(value));
        add(tf);
        return tf;
    }

    @Override public boolean action(Event event, Object object1) {
        if (event.target == button1) {
            String str;
            str = tf1.getText();
            if (str != null && str.length() != 0) dpi = Double.valueOf(str);
            str = tf2.getText();
            if (str != null && str.length() != 0) maxIterations = Integer.parseInt(str);
            str = tf3.getText();
            if (str != null && str.length() != 0) power = Double.valueOf(str);
            str = tf4.getText();
            if (str != null && str.length() != 0) xc = Double.valueOf(str);
            str = tf5.getText();
            if (str != null && str.length() != 0) yc = Double.valueOf(str);
            str = tf6.getText();
            if (str != null && str.length() != 0) centerX = Double.valueOf(str);
            str = tf7.getText();
            if (str != null && str.length() != 0) centerY = Double.valueOf(str);

            clearImage();
            generate();

        } else if (event.target == choice1) isJuliaSet = (choice1.getSelectedIndex() == 1);
        else if (event.target == choice2) coloringMethod = choice2.getSelectedIndex();
        else if (event.target == choice3) selectedColor = choice3.getSelectedIndex();
        return true;
    }

    @Override public boolean mouseUp(Event event1, int x, int y) {
        x -= imageXShift;
        y -= imageYShift;
        tf6.setText(String.valueOf(centerX + (x - 0.5 * imageWidth) / dpi));
        tf7.setText(String.valueOf(centerY - (y - 0.5 * imageHeight) / dpi));
        graphics = getGraphics();
        drawPixelAt(x, y, new Color(255, 255, 255));
        return true;
    }

    private void generate() {
        final int n = (int) power / 2 - ((power % 2 != 0) ? 1 : 0);

        final double dx = 3.0 / imageWidth;

        final double x1 = centerX - 0.5 * imageWidth * dx;
        final double y1 = centerY - 0.5 * imageHeight * dx;
        final double x2 = centerX + 0.5 * imageWidth * dx;
        final double y2 = centerY + 0.5 * imageHeight * dx;

        final double maxX = max(abs(x1), abs(x2));
        final double maxY = max(abs(y1), abs(y2));
        final double zm = sqrt(maxX * maxX + maxY * maxY);

        for (int yPixel = 0; yPixel <= imageHeight; yPixel++) {
            if (!isJuliaSet) yc = y2 - yPixel * dx;

            for (int xPixel = 0; xPixel <= imageWidth; xPixel++) {
                double x;
                double y;

                if (isJuliaSet) {
                    x = x1 + xPixel * dx;
                    y = y2 - yPixel * dx;
                } else {
                    xc = x1 + xPixel * dx;
                    x = 0;
                    y = 0;
                }

                int iteration = 0;
                boolean unbounded = false;

                while (iteration < maxIterations) {
                    if (power >= 2 && power <= 25 && power % 1 == 0) { // faster logic
                        for (int i = 0; i < n; i++) {
                            double tmp = x * x - y * y;
                            y = 2 * x * y;
                            x = tmp;
                        }
                        // z^2
                        if (power % 2 != 0) {
                            double tmp = x * (x * x - 3 * y * y);
                            y = y * (3 * x * x - y * y);
                            x = tmp;
                        } // z^3
                    } else {
                        double tmp1 = pow(x * x + y * y, power / 2.0);
                        double tmp2 = atan2(y, x);
                        x = tmp1 * cos(power * tmp2);
                        y = tmp1 * sin(power * tmp2);
                    } // z^n
                    //   t1=x*x+y*y; x=x/t1; y=-y/t1; // z^-1
                    //   t1=Math.atan(y/x); t2=Math.pow(x*x+y*y,0.25); x=t2*Math.cos(t1/2); y=t2*Math.sin(t1/2); // sqrt(z)
                    //   t1=Math.sin(x)*cosh(y); y=Math.cos(x)*sinh(y); x=t1; // sin(z)
                    //   t1=Math.cos(x)*cosh(y); y=-Math.sin(x)*sinh(y); t1=x; // cos(z)
                    //   t1=Math.cos(2*x)+cosh(2*y); x=Math.sin(2*x)/t1; y=sinh(2*y)/t1; // tan(z)
                    //   t1=Math.exp(x); x=t1*Math.cos(y); y=t1*Math.sin(y); // e^z
                    //   t1=Math.log(x*x+y*y)/2; y=Math.atan(y/x); x=t1; // ln(z)

                    iteration++;

                    x += xc;
                    y += yc;
                    if (x * x + y * y >= threshold) {
                        unbounded = true;
                        break;
                    }
                }

                if (unbounded) {
                    double colorScale;
                    if (coloringMethod == 0) colorScale = (double) iteration / maxIterations; // iterations
                    else if (coloringMethod == 1) colorScale = min(1, sqrt(x * x + y * y) / zm); // magnitude
                    else colorScale = min(1, abs((coloringMethod == 2) ? x : y) / zm); // recenter: don't use abs

                    Color color = calculateColor(colorScale);
                    drawPixelAt(xPixel, yPixel, color);
                }
            }
        }
    }

    private void clearImage() {
        graphics = getGraphics();
        graphics.setColor(Color.black);
        graphics.fillRect(imageXShift, imageYShift, imageWidth, imageHeight);
    }

    private Color calculateColor(double colorScale) {
        int r = 0;
        int g = 0;
        int b = 0;
        if (selectedColor == 6) {
            r = (int) (255 * colorScale);
            g = r;
            b = r;
        } else {
            int c1 = (int) min(255 * 2 * colorScale, 255);
            int c2 = (int) max(255 * (2 * colorScale - 1), 0);
            switch (selectedColor) {
                case 0:
                    r = c1;
                    g = c2;
                    b = c2;
                    break; // red
                case 1:
                    r = c2;
                    g = c1;
                    b = c2;
                    break; // green
                case 2:
                    r = c2;
                    g = c2;
                    b = c1;
                    break; // blue
                case 3:
                    r = c1;
                    g = c1;
                    b = c2;
                    break; // yellow
                case 4:
                    r = c2;
                    g = c1;
                    b = c1;
                    break; // cyan
                case 5:
                    r = c1;
                    g = c2;
                    b = c1;
                    break; // magenta
            }
        }
        return new Color(r, g, b);
    }

    private void drawPixelAt(int x, int y, Color color) {
        graphics.setColor(color);
        graphics.fillRect(x + imageXShift, y + imageYShift, 1, 1);
    }

    private static double sinh(double x) {
        double t1 = exp(x);
        return (t1 - 1.0 / t1) / 2.0;
    }

    private static double cosh(double x) {
        double t1 = exp(x);
        return (t1 + 1.0 / t1) / 2.0;
    }
}