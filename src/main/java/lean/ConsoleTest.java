package lean;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ConsoleTest {
    @Test public void pricesWithoutDiscount() {
        assertEquals(new Console().calculateListItemPrice("Apples"), 100);
        assertEquals(new Console().calculateListItemPrice("Cherries"), 75);
        assertEquals(new Console().calculateListItemPrice("Bananas"), 150);
    }

    @Test public void shouldAcceptCsvInput() {
        assertEquals(new Console().calculateListItemPrice("Apples,Cherries,Bananas"), 325);
        assertEquals(new Console().calculateListItemPrice("Apples, Cherries, Bananas"), 325);
    }

    @Test public void shouldHaveDiscountOnCherries() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Cherries"), 75);
        assertEquals(console.calculateListItemPrice("Cherries"), 130);
    }

    @Test public void shouldHaveDiscountOnBananas() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Bananas"), 150);
        assertEquals(console.calculateListItemPrice("Bananas"), 150);
        assertEquals(console.calculateListItemPrice("Bananas"), 300);
        assertEquals(console.calculateListItemPrice("Bananas"), 300);
    }

    @Test public void shouldHaveDiscountOnPommes() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Pommes"), 100);
        assertEquals(console.calculateListItemPrice("Pommes"), 200);
        assertEquals(console.calculateListItemPrice("Pommes"), 200);
    }

    @Test public void shouldHaveDiscountOnMele() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Mele"), 100);
        assertEquals(console.calculateListItemPrice("Mele"), 150);
    }

    @Test public void shouldHaveDiscountOnApples() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Mele"), 100);
        assertEquals(console.calculateListItemPrice("Pommes"), 200);
        assertEquals(console.calculateListItemPrice("Apples"), 300);
        assertEquals(console.calculateListItemPrice("Apples"), 300);
    }

    @Test public void shouldHaveDiscountOnFruit() {
        Console console = new Console();
        assertEquals(console.calculateListItemPrice("Mele"), 100);
        assertEquals(console.calculateListItemPrice("Pommes"), 200);
        assertEquals(console.calculateListItemPrice("Apples"), 300);
        assertEquals(console.calculateListItemPrice("Cherries"), 375);
        assertEquals(console.calculateListItemPrice("Bananas"), 325);
    }
}
