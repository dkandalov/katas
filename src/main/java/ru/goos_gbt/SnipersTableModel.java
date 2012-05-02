package ru.goos_gbt;

import javax.swing.table.AbstractTableModel;

/**
 * User: dima
 * Date: 20/03/2012
 */
public class SnipersTableModel extends AbstractTableModel {

    private static final SniperState STARTING_UP = new SniperState("", 0, 0);

    public enum Column {
        ITEM_IDENTIFIER,
        LAST_PRICE,
        LAST_BID,
        SNIPER_STATUS;

        public static Column at(int offset) {
            return values()[offset];
        }

    }

    private SniperState sniperState = STARTING_UP;
    private String statusText = MainWindow.STATUS_JOINING;

    @Override public int getRowCount() {
        return 1;
    }

    @Override public int getColumnCount() {
        return Column.values().length;
    }

    @Override public Object getValueAt(int rowIndex, int columnIndex) {
        switch (Column.at(columnIndex)) {
            case ITEM_IDENTIFIER:
                return sniperState.itemId;
            case LAST_PRICE:
                return sniperState.lastPrice;
            case LAST_BID:
                return sniperState.lastBid;
            case SNIPER_STATUS:
                return statusText;
            default:
                throw new IllegalStateException("No column at " + columnIndex);
        }
    }

    public void setStatusText(String newStatusText) {
        statusText = newStatusText;
        fireTableRowsUpdated(0, 0);
    }

    public void sniperStatusChanged(SniperState sniperState, String statusText) {
        this.sniperState = sniperState;
        this.statusText = statusText;
        fireTableRowsUpdated(0, 0);
    }
}
