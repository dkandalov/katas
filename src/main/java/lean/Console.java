package lean;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * User: dima
 * Date: 28/03/2012
 */
public class Console {

    private long price = 0;
    private int cherriesCount = 0;
    private int bananaCount = 0;
    private int pommesCount = 0;
    private int meleCount = 0;

    public static void main(String[] args) throws IOException {
        Console console = new Console();
        console.start();
    }

    private void start() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader((System.in)));
        String line;
        while (!(line = reader.readLine()).equals("exit")) {
            long price = calculateListItemPrice(line);
            System.out.println(price);
        }
    }

    public long calculateListItemPrice(String line) {
        String[] items = line.split(",\\s?");
        for (String item : items) processLine(item);

        return price;
    }
    
    private long processLine(String line) {
        
        if (line.equals("Apples") || line.equals("Mele") || line.equals("Pommes")) {
            if (line.equals("Pommes")) pommesCount++;
            if (line.equals("Mele")) meleCount++;
            price += 100;
        } else if (line.equals("Cherries")) {
            cherriesCount++;
            price += 75;
        } else if (line.equals("Bananas")) {
            bananaCount++;
            price += 150;
        }

        applyDiscount();

        return price;
    }

    private void applyDiscount() {
        if (cherriesCount == 2) {
            price -= 20;
            cherriesCount = 0;
        }
        if (bananaCount == 2) {
            price -= 150;
            bananaCount = 0;
        }
        if (pommesCount == 3) {
            price = price - 100;
            pommesCount = 0;
        }
        if (meleCount == 2) {
            price = price - 100;
            meleCount = 0;
        }
    }
}
