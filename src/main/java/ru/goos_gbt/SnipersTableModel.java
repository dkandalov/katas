package ru.goos_gbt;

import javax.swing.table.AbstractTableModel;

/**
 * User: dima
 * Date: 20/03/2012
 */
public class SnipersTableModel extends AbstractTableModel {
    private String statusText = MainWindow.STATUS_JOINING;

    @Override public int getRowCount() {
        return 1;
    }

    @Override public int getColumnCount() {
        return 1;
    }

    @Override public Object getValueAt(int rowIndex, int columnIndex) {
        return statusText;
    }

    public void setStatusText(String newStatusText) {
        statusText = newStatusText;
        fireTableRowsUpdated(0, 0);
    }
}
