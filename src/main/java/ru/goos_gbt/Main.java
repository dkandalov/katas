package ru.goos_gbt;

import javax.swing.*;

/**
 * User: dima
 * Date: 04/03/2012
 */
public class Main {
    public static final String MAIN_WINDOW_NAME = "Auction Sniper";
    private MainWindow ui;

    public static void main(String xmppHostname, String sniperId, String sniperPassword, String itemId) throws Exception {
        Main main = new Main();
    }

    public Main() throws Exception {
        startUserInterface();
    }

    private void startUserInterface() throws Exception {
        SwingUtilities.invokeAndWait(new Runnable() {
            @Override
            public void run() {
                ui = new MainWindow();
            }
        });
    }

}
