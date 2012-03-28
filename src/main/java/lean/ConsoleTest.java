package lean;

import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ConsoleTest {
    @Ignore
    @Test
    public void iteration1_acceptance_test() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Apples"), 100);
        assertEquals(console.calculateListItemPrice("Cherries"), 175);
        assertEquals(console.calculateListItemPrice("Bananas"), 325);
    }

    @Ignore
    @Test
    public void iteration2_acceptance_test() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Apples"), 100);
        assertEquals(console.calculateListItemPrice("Apples"), 200);
        assertEquals(console.calculateListItemPrice("Cherries"), 275);
        assertEquals(console.calculateListItemPrice("Apples"), 375);
        assertEquals(console.calculateListItemPrice("Cherries"), 430);
    }

    @Ignore
    @Test
    public void iteration3_acceptance_test() {
        assertEquals(new Console().calculateListItemPrice("Apples,Cherries,Bananas"), 325);
        assertEquals(new Console().calculateListItemPrice("Cherries,Cherries"), 130);
        // with spaces
        assertEquals(new Console().calculateListItemPrice("Apples, Cherries, Bananas"), 325);

        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Cherries"), 75);
        assertEquals(console.calculateListItemPrice("Cherries"), 120);
        assertEquals(console.calculateListItemPrice("Bananas"), 270);
        assertEquals(console.calculateListItemPrice("Bananas"), 270);
    }

    @Test
    public void iteration4() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Cherries"), 75);
        assertEquals(console.calculateListItemPrice("Pommes"), 175);
        assertEquals(console.calculateListItemPrice("Cherries"), 230);
        assertEquals(console.calculateListItemPrice("Bananas"), 380);
        assertEquals(console.calculateListItemPrice("Bananas"), 380);
        assertEquals(console.calculateListItemPrice("Apples"), 480);
    }
}
