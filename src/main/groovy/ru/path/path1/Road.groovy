package path.path1
/**
 * User: dima
 * Date: May 15, 2008
 * Time: 10:09:33 AM
 */
class Road {
    final Point p1
    final Point p2

    Road(Point p1, Point p2) {
        this.p1 = p1
        this.p2 = p2
    }

    Point getAt(def index) {
        index == 0 ? p1 : p2
    }

    Point getAnotherPoint(Point point) {
        if (point != p1 && point != p2)
            throw new IllegalArgumentException()
        point == p1 ? p2 : p1
    }

    double getLength() {
        p1.distanceTo(p2)
    }

    public String toString() {
        "{$p1, $p2}"
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof Road)) return false

        ((p1 == obj.p1 && p2 == obj.p2) ||
         (p1 == obj.p2 && p2 == obj.p1))
    }

    public int hashCode() {
        int result = 17
        result = 37*result + p1.hashCode()
        result = 37*result + p2.hashCode()
        result
    }

}