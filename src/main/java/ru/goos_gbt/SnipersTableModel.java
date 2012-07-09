package ru.goos_gbt;

import javax.swing.table.AbstractTableModel;
import java.util.ArrayList;
import java.util.List;

/**
 * User: dima
 * Date: 20/03/2012
 */
public class SnipersTableModel extends AbstractTableModel implements SniperListener {

    private static final String[] STATUS_TEXT = {
            "Joining",
            "Bidding",
            "Winning",
            "Lost",
            "Won"
    };

    public enum Column {
        ITEM_IDENTIFIER("Item") {
            @Override public Object valueIn(SniperSnapshot snapshot) {
                return snapshot.itemId;
            }
        },
        LAST_PRICE("Last Price") {
            @Override public Object valueIn(SniperSnapshot snapshot) {
                return snapshot.lastPrice;
            }
        },
        LAST_BID("Last Bid") {
            @Override public Object valueIn(SniperSnapshot snapshot) {
                return snapshot.lastBid;
            }
        },
        SNIPER_STATE("State") {
            @Override public Object valueIn(SniperSnapshot snapshot) {
                return textFor(snapshot.state);
            }
        };

        public final String name;

        private Column(String name) {
            this.name = name;
        }

        public static Column at(int offset) {
            return values()[offset];
        }

        public abstract Object valueIn(SniperSnapshot snapshot);
    }

    private final List<SniperSnapshot> snapshots = new ArrayList<>();

    public void addSniper(SniperSnapshot sniperSnapshot) {
        snapshots.add(sniperSnapshot);
        fireTableRowsInserted(snapshots.size() - 1, snapshots.size() - 1);
    }

    @Override public int getRowCount() {
        return snapshots.size();
    }

    @Override public int getColumnCount() {
        return Column.values().length;
    }

    @Override public Object getValueAt(int rowIndex, int columnIndex) {
        return Column.at(columnIndex).valueIn(snapshots.get(rowIndex));
    }

    @Override public String getColumnName(int columnIndex) {
        return Column.at(columnIndex).name;
    }

    @Override public void sniperStateChanged(SniperSnapshot newSnapshot) {
        int row = rowMatching(newSnapshot);
        snapshots.set(row, newSnapshot);
        fireTableRowsUpdated(row, row);

//        for (int i = 0; i < snapshots.size(); i++) {
//            SniperSnapshot snapshot = snapshots.get(i);
//            if (newSnapshot.itemId.equals(snapshot.itemId)) {
//                snapshots.set(i, newSnapshot);
//                fireTableRowsUpdated(i, i);
//                return;
//            }
//        }
//        throw new Defect("SniperSnapshot with itemId = " + newSnapshot.itemId + " doesn't exist");
    }

    private int rowMatching(SniperSnapshot snapshot) {
        for (int i = 0; i < snapshots.size(); i++) {
            if (snapshot.isForSameItemAs(snapshots.get(i))) {
                return i;
            }
        }
        throw new Defect("Cannot find match for " + snapshot);
    }

    private static String textFor(SniperState state) {
        return STATUS_TEXT[state.ordinal()];
    }
}
