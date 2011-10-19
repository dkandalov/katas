package ru.connectivity;

import ru.connectivity.Connections;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Sep 17, 2010
 */
public class ConnectionsTest {
    public void pointsAreConnectedToThemselves(Connections connections) {
        assertTrue(connections.areConnected(0, 0));
        connections.connect(0, 0);
        assertTrue(connections.areConnected(0, 0));

        assertTrue(connections.areConnected(1, 1));
        connections.connect(1, 1);
        assertTrue(connections.areConnected(1, 1));
    }

    public void onePointCanBeConnectedToMultiplePoints(Connections connections) {
        assertFalse(connections.areConnected(0, 1));
        connections.connect(0, 1);
        assertTrue(connections.areConnected(0, 1));

        assertFalse(connections.areConnected(0, 2));
        connections.connect(0, 2);
        assertTrue(connections.areConnected(0, 2));

        assertTrue(connections.areConnected(0, 1));
    }

    public void isConnectedMethodShouldNotHaveInfiniteLoops(Connections connections) {
        connections.connect(0, 1);
        connections.connect(1, 0);

        assertTrue(connections.areConnected(0, 1));
        assertTrue(connections.areConnected(1, 0));
    }

    public void connectionsAreTransitive(Connections connections) {
        connections.connect(0, 1);
        connections.connect(1, 2);

        assertThat(connections.areConnected(0, 1), equalTo(true));
        assertThat(connections.areConnected(1, 2), equalTo(true));
        assertThat(connections.areConnected(0, 2), equalTo(true));
    }

    public void pointsCanBeNotConnected(Connections connections){
        connections.connect(0, 1);
        connections.connect(2, 3);

        assertThat(connections.areConnected(0, 1), equalTo(true));
        assertThat(connections.areConnected(1, 0), equalTo(true));
        assertThat(connections.areConnected(2, 3), equalTo(true));
        assertThat(connections.areConnected(3, 2), equalTo(true));

        assertThat(connections.areConnected(0, 2), equalTo(false));
        assertThat(connections.areConnected(0, 3), equalTo(false));
        assertThat(connections.areConnected(1, 2), equalTo(false));
        assertThat(connections.areConnected(1, 3), equalTo(false));
    }

    public void quickUnion_ForBigInput(Connections connections) {
        connections.
                connect(3, 4).
                connect(4, 9).
                connect(8, 0).
                connect(2, 3).
                connect(5, 6);

        assertTrue(connections.areConnected(2, 9));
        connections.
                connect(2, 9).
                connect(5, 9).
                connect(7, 3).
                connect(4, 8);

        assertTrue(connections.areConnected(5, 6));
        connections.
                connect(5, 6).
                connect(0, 2).
                connect(6, 1)
                ;

        System.out.println("-------");
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                System.out.println(i + ", " + j);
                assertTrue(connections.areConnected(i, j));
            }
        }
    }

}
