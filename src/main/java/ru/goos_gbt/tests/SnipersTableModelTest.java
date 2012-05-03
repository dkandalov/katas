package ru.goos_gbt.tests;

import org.hamcrest.Matcher;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import ru.goos_gbt.MainWindow;
import ru.goos_gbt.SniperSnapshot;
import ru.goos_gbt.SniperState;
import ru.goos_gbt.SnipersTableModel;

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.beans.SamePropertyValuesAs.samePropertyValuesAs;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: 02/05/2012
 */
@RunWith(JMock.class)
public class SnipersTableModelTest {
    private final Mockery context = new Mockery();
    private TableModelListener listener = context.mock(TableModelListener.class);
    private final SnipersTableModel model = new SnipersTableModel();

    @Before public void attachModelListener() {
        model.addTableModelListener(listener);
    }

    @Test public void hasEnoughColumns() {
        assertThat(model.getColumnCount(), equalTo(SnipersTableModel.Column.values().length));
    }

    @Test public void setsSniperValuesInColumns() {
        context.checking(new Expectations(){{
            one(listener).tableChanged(with(aRowChangeEvent()));
        }});

        model.sniperStateChanged(new SniperSnapshot("item id", 555, 666, SniperState.BIDDING));

        assertColumnEquals(SnipersTableModel.Column.ITEM_IDENTIFIER, "item id");
        assertColumnEquals(SnipersTableModel.Column.LAST_PRICE, 555);
        assertColumnEquals(SnipersTableModel.Column.LAST_BID, 666);
        assertColumnEquals(SnipersTableModel.Column.SNIPER_STATE, MainWindow.STATUS_BIDDING);
    }

    private void assertColumnEquals(SnipersTableModel.Column column, Object expected) {
        int rowIndex = 0;
        int columnIndex = column.ordinal();
        assertEquals(expected, model.getValueAt(rowIndex, columnIndex));
    }

    private Matcher<TableModelEvent> aRowChangeEvent() {
        return samePropertyValuesAs(new TableModelEvent(model, 0));
    }
}
