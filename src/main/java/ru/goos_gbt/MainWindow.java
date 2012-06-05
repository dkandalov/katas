package ru.goos_gbt;

import javax.swing.*;
import java.awt.*;

/**
* User: dima
* Date: 06/03/2012
*/
public class MainWindow extends JFrame {
    public static final String SNIPERS_TABLE_NAME = "Snipers table";

    public final SnipersTableModel snipersTableModel;

    public MainWindow(SnipersTableModel snipersTableModel) {
        super("Auction sniper");

        this.snipersTableModel = snipersTableModel;

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
}
