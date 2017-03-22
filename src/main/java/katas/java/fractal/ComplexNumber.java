package katas.java.fractal;

/**
* User: dima
* Date: 13/12/2012
*/
class ComplexNumber {
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

    @Override public String toString() {
        return "ComplexNumber{a=" + a + ", b=" + b + '}';
    }
}
