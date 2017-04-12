package katas.groovy.path1

import static java.lang.Math.*

/**
 * User: dima
 * Date: May 14, 2008
 * Time: 12:27:06 PM
 */
class Point {
    int x
    int y
    String name

    Point(x, y, name) {
        this.x = x
        this.y = y
        this.name = name
    }

    double distanceTo(Point point) {
        int xDiff = abs(x - point.x)
        int yDiff = abs(y - point.y)
        sqrt(xDiff * xDiff + yDiff * yDiff)
    }

    public String toString() {
        "$name"
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof Point)) return false
        x == obj.x && y == obj.y
    }

    public int hashCode() {
        int result = 17
        result = 37*result + x
        result = 37*result + y
        result
    }
}