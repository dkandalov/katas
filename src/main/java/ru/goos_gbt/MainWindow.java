package ru.goos_gbt;

import javax.swing.*;
import java.awt.*;

/**
* User: dima
* Date: 06/03/2012
*/
public class MainWindow extends JFrame {
    public static final String SNIPERS_TABLE_NAME = "Snipers table";

    public static final String STATUS_JOINING = "Joining";
    public static final String STATUS_LOST = "Lost";
    public static final String STATUS_BIDDING = "Bidding";
    public static final String STATUS_WINNING = "Winning";
    public static final String STATUS_HAS_WON = "Has Won";

    public final SnipersTableModel snipersTableModel = new SnipersTableModel();

    public MainWindow() {
        super("Auction sniper");
        setName(Main.MAIN_WINDOW_NAME);
        fillContentPane(makeSnipersTable());
        pack();
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
    }

    private void fillContentPane(JTable snipersTable) {
        Container contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(new JScrollPane(snipersTable), BorderLayout.CENTER);
    }

    private JTable makeSnipersTable() {
        JTable snipersTable = new JTable(snipersTableModel);
        snipersTable.setName(SNIPERS_TABLE_NAME);
        return snipersTable;
    }

    public void showStatus(String status) {
        snipersTableModel.setStatusText(status);
    }

    public void sniperStateChanged(SniperSnapshot sniperSnapshot) {
        snipersTableModel.sniperStateChanged(sniperSnapshot);
    }
}
