package katas.java.connectivity;

/**
 * User: dima
 * Date: Sep 17, 2010
 */
public interface Connections {
    Connections connect(int p1, int p2);

    boolean areConnected(int p1, int p2);
}
