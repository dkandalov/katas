package ru.fractal;

// Mandelbrot and Julia Set Fractal Java Applet, 6/9/01

import java.applet.Applet;
import java.awt.*;

// Options: nx, ny, Julia: xc, yc
public class Mandelbrot extends Applet {
    Graphics graphics1;
    boolean julia;
    int nx, ny, imax = 50, method = 0, fcolor = 0;
    double x0 = -0.5, y0 = 0, dpi, power = 2, xc = 0, yc = 0;

    // Initial Layout
    TextField tf1, tf2, tf3, tf4, tf5, tf6, tf7;
    Choice choice1 = new Choice(), choice2 = new Choice(), choice3 = new Choice();
    Button button1 = new Button("Generate Image");

    public TextField editbox(String str, double value, int x, int y, int w) { // use template, overloading
        Label label = new Label(str + ":");
        label.setBounds(x, y, w, 25);
        add(label);
        TextField tf = new TextField(20);
        tf.setBounds(x + w, y, 102 - w, 25);
        tf.setText(String.valueOf(value));
        add(tf);
        return tf;
    }

    public void init() {
        setLayout(null);
        nx = size().width - 20;
        ny = size().height - 160;
        dpi = nx / 3;
        setSize(nx + 20, ny + 160); // resize?

        choice1.addItem("Mandelbrot Set");
        choice1.addItem("Julia Set");
        choice1.select(0);
        choice1.setBounds(10, ny + 20, 102, 25);
        add(choice1);
        tf4 = editbox("xc", xc, 122, ny + 20, 20);
        tf5 = editbox("yc", yc, 228, ny + 20, 20);

        tf1 = editbox("dpi", dpi, 10, ny + 55, 50); // max dpi: x=8.93E15, y=1.79E16
        tf6 = editbox("x0", x0, 122, ny + 55, 20);
        tf7 = editbox("y0", y0, 228, ny + 55, 20);

        tf2 = editbox("max iterations", 0.0, 10, ny + 90, 72);
        tf2.setText(String.valueOf(imax));
        choice2.addItem("iterations");
        choice2.addItem("magnitude");
        choice2.addItem("real");
        choice2.addItem("imaginary");
        choice2.setBounds(122, ny + 90, 102, 25);
        add(choice2);
        choice3.addItem("red");
        choice3.addItem("green");
        choice3.addItem("blue");
        choice3.addItem("yellow");
        choice3.addItem("cyan");
        choice3.addItem("magenta");
        choice3.addItem("white"); // rainbow
        choice3.setBounds(228, ny + 90, 102, 25);
        add(choice3);

        tf3 = editbox("power", power, 10, ny + 125, 50);
        button1.setBounds(228, ny + 125, 102, 25);
        add(button1);
//generate(); // doesn't work
    }

    // Input Actions
    public boolean action(Event event1, Object object1) {
        if (event1.target == button1) {
            String str;
            str = tf1.getText();
            if (str != null && str.length() != 0) dpi = Double.valueOf(str).doubleValue();
            str = tf2.getText();
            if (str != null && str.length() != 0) imax = Integer.parseInt(str);
            str = tf3.getText();
            if (str != null && str.length() != 0) power = Double.valueOf(str).doubleValue();
            str = tf4.getText();
            if (str != null && str.length() != 0) xc = Double.valueOf(str).doubleValue();
            str = tf5.getText();
            if (str != null && str.length() != 0) yc = Double.valueOf(str).doubleValue();
            str = tf6.getText();
            if (str != null && str.length() != 0) x0 = Double.valueOf(str).doubleValue();
            str = tf7.getText();
            if (str != null && str.length() != 0) y0 = Double.valueOf(str).doubleValue();
            generate();
        } else if (event1.target == choice1) julia = (choice1.getSelectedIndex() == 1);
        else if (event1.target == choice2) method = choice2.getSelectedIndex();
        else if (event1.target == choice3) fcolor = choice3.getSelectedIndex();
        return true;
    }

    public boolean mouseUp(Event event1, int x, int y) {
        x -= 10;
        y -= 10;
        tf6.setText(String.valueOf(x0 + (x - 0.5 * nx) / dpi));
        tf7.setText(String.valueOf(y0 - (y - 0.5 * ny) / dpi));
        graphics1 = getGraphics();
        pixel(x, y, 255, 255, 255);
        return true;
    }

    // Calculations
    public double sinh(double x) {
        double t1 = Math.exp(x);
        return (t1 - 1.0 / t1) / 2.0;
    }

    public double cosh(double x) {
        double t1 = Math.exp(x);
        return (t1 + 1.0 / t1) / 2.0;
    }

    // Generate Image
    public void pixel(int x, int y, int r, int g, int b) {
        Color color1 = new Color(r, g, b);
        graphics1.setColor(color1);
        graphics1.fillRect(x + 10, y + 10, 1, 1);
    }

    public void generate() {
        graphics1 = getGraphics();
        graphics1.setColor(Color.black); // clear previous image
        graphics1.fillRect(10, 10, nx, ny);
        boolean unbounded;
        int c1, c2, r = 0, g = 0, b = 0, i, n = (int) power / 2 - ((power % 2 != 0) ? 1 : 0);
        double x = 0, y = 0, dx = 1 / dpi, x1 = x0 - 0.5 * nx / dpi, x2 = x0 + 0.5 * nx / dpi, y1 = y0 - ny * dx / 2, y2 = y0 + ny * dx / 2;
        double t1 = Math.max(Math.abs(x1), Math.abs(x2)), t2 = Math.max(Math.abs(y1), Math.abs(y2)), zm = Math.sqrt(t1 * t1 + t2 * t2);
        for (int iy = 0; iy <= ny; ++iy) {
            if (!julia) yc = y2 - iy * dx;
            for (int ix = 0; ix <= nx; ++ix) {
                if (julia) {
                    x = x1 + ix * dx;
                    y = y2 - iy * dx;
                } else {
                    xc = x1 + ix * dx;
                    x = 0;
                    y = 0;
                }
                unbounded = false;
                for (i = 0; (i < imax) && (!unbounded); ++i) {
                    if (power >= 2 && power <= 25 && power % 1 == 0) { // faster logic
                        for (int j = 0; j < n; ++j) {
                            t1 = x * x - y * y;
                            y = 2 * x * y;
                            x = t1;
                        }
                        ; // z^2
                        if (power % 2 != 0) {
                            t1 = x * (x * x - 3 * y * y);
                            y = y * (3 * x * x - y * y);
                            x = t1;
                        } // z^3
                    } else {
                        t1 = Math.atan2(y, x);
                        t2 = Math.pow(x * x + y * y, power / 2.0);
                        x = t2 * Math.cos(power * t1);
                        y = t2 * Math.sin(power * t1);
                    } // z^n
//   t1=x*x+y*y; x=x/t1; y=-y/t1; // z^-1
//   t1=Math.atan(y/x); t2=Math.pow(x*x+y*y,0.25); x=t2*Math.cos(t1/2); y=t2*Math.sin(t1/2); // sqrt(z)
//   t1=Math.sin(x)*cosh(y); y=Math.cos(x)*sinh(y); x=t1; // sin(z)
//   t1=Math.cos(x)*cosh(y); y=-Math.sin(x)*sinh(y); t1=x; // cos(z)
//   t1=Math.cos(2*x)+cosh(2*y); x=Math.sin(2*x)/t1; y=sinh(2*y)/t1; // tan(z)
//   t1=Math.exp(x); x=t1*Math.cos(y); y=t1*Math.sin(y); // e^z
//   t1=Math.log(x*x+y*y)/2; y=Math.atan(y/x); x=t1; // ln(z)
                    x += xc;
                    y += yc;
                    if (x * x + y * y >= 4.0) unbounded = true;
                }
                if (unbounded) {
                    if (method == 0) t1 = 1.0 * i / imax; // iterations
                    else if (method == 1) t1 = Math.min(1, Math.sqrt(x * x + y * y) / zm); // magnitude
                    else t1 = Math.min(1, Math.abs((method == 2) ? x : y) / zm); // recenter: don't use abs
                    if (fcolor == 6) {
                        r = (int) (255 * t1);
                        g = r;
                        b = r;
                    } else {
                        c1 = (int) Math.min(255 * 2 * t1, 255);
                        c2 = (int) Math.max(255 * (2 * t1 - 1), 0);
                        switch (fcolor) {
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
                    pixel(ix, iy, r, g, b);
                }
            }
        }

        // Label Drawing
        graphics1.setColor(Color.white);
        graphics1.setFont(new Font("Serif", Font.BOLD, 12));
        int sc = 9, sc2 = 9 * 5 / 3, ytxt = ny + 10 - 5 + sc2;
        if (nx <= 1.0 * dpi) graphics1.drawString("" + dpi + " dpi, Origin: " + x0 + "+" + y0 + "i", 15, ytxt -= sc2);
        graphics1.drawString("Max iterations: " + imax, 15, ytxt -= sc2);
        if (julia) graphics1.drawString("Julia Set: z^" + power + "+" + xc + "+" + yc + "i", 15, ytxt -= sc2);
        else graphics1.drawString("Mandelbrot Set: z^" + power + "+zc", 15, ytxt -= sc2);
    }
}