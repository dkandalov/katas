package ru.goos_gbt.tests;

import org.hamcrest.CustomMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import ru.goos_gbt.Defect;
import ru.goos_gbt.SniperSnapshot;
import ru.goos_gbt.SnipersTableModel;

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.beans.SamePropertyValuesAs.samePropertyValuesAs;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static ru.goos_gbt.SnipersTableModel.Column.*;

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
        SniperSnapshot joining = SniperSnapshot.joining("item123");
        SniperSnapshot bidding = joining.bidding(555, 666);
        context.checking(new Expectations(){{
            allowing(listener).tableChanged(with(anyInsertionEvent()));
            one(listener).tableChanged(with(aChangeInRow(0)));
        }});

        model.addSniper(joining);
        model.sniperStateChanged(bidding);

        assertRowMatchesSnapshot(0, bidding);
    }

    @Test public void setsUpColumnHeadings() {
        for (SnipersTableModel.Column column : SnipersTableModel.Column.values()) {
            assertEquals(column.name, model.getColumnName(column.ordinal()));
        }
    }

    @Test public void notifiesListenersWhenAddingASniper() {
        SniperSnapshot joining = SniperSnapshot.joining("item123");
        context.checking(new Expectations() {{
            one(listener).tableChanged(with(anInsertionAtRow(0)));
        }});

        assertEquals(0, model.getRowCount());

        model.addSniper(joining);

        assertEquals(1, model.getRowCount());
        assertRowMatchesSnapshot(0, joining);
    }

    @Test public void holdsSniperInAdditionOrder() {
        context.checking(new Expectations() {{
            ignoring(listener);
        }});

        model.addSniper(SniperSnapshot.joining("item 0"));
        model.addSniper(SniperSnapshot.joining("item 1"));

        assertEquals("item 0", cellValue(0, SnipersTableModel.Column.ITEM_IDENTIFIER));
        assertEquals("item 1", cellValue(1, SnipersTableModel.Column.ITEM_IDENTIFIER));
    }

    /*@Test public void updatesCorrectRowForSniper() { TODO uncomment
        context.checking(new Expectations() {{
            ignoring(listener);
        }});

        model.addSniper(SniperSnapshot.joining("item 0"));
        model.addSniper(SniperSnapshot.joining("item 1"));
        model.sniperStateChanged(SniperSnapshot.joining("item 0").bidding(123, 234));

        assertEquals("item 0", cellValue(0, SnipersTableModel.Column.ITEM_IDENTIFIER));
        assertEquals(123, cellValue(0, SnipersTableModel.Column.LAST_PRICE));
        assertEquals(234, cellValue(0, SnipersTableModel.Column.LAST_BID));
    }*/

    @Test(expected = Defect.class)
    public void throwsDefectIfNoExistingSniperForAnUpdate() {
        context.checking(new Expectations() {{
            ignoring(listener);
        }});

        model.addSniper(SniperSnapshot.joining("item 0"));
        model.sniperStateChanged(SniperSnapshot.joining("item 2").bidding(123, 234));
    }

    @SuppressWarnings("unchecked")
    private <T> T cellValue(int rowIndex, SnipersTableModel.Column column) {
        return (T) model.getValueAt(rowIndex, column.ordinal());
    }

    private void assertRowMatchesSnapshot(int row, SniperSnapshot snapshot) {
        assertColumnEquals(row, ITEM_IDENTIFIER, snapshot.itemId);
        assertColumnEquals(row, LAST_PRICE, snapshot.lastPrice);
        assertColumnEquals(row, LAST_BID, snapshot.lastBid);
        assertColumnEquals(row, SNIPER_STATE, snapshot.state);
    }

    private Matcher<TableModelEvent> anInsertionAtRow(final int row) {
        return new TypeSafeMatcher<TableModelEvent>() {
            @Override public boolean matchesSafely(TableModelEvent event) {
                return event.getFirstRow() == row;
            }

            @Override public void describeTo(Description description) {
                description.appendText("TableModelEvent with inserted row = " + row);
            }
        };
    }

    private static Matcher<TableModelEvent> anyInsertionEvent() {
        return new CustomMatcher<TableModelEvent>("Table insertion event") {
            @Override public boolean matches(Object item) {
                return ((TableModelEvent)item).getType() == TableModelEvent.INSERT;
            }
        };
    }

    private void assertColumnEquals(int rowIndex, SnipersTableModel.Column column, Object expected) {
        int columnIndex = column.ordinal();
        assertEquals(expected.toString().toLowerCase(), model.getValueAt(rowIndex, columnIndex).toString().toLowerCase());
    }

    private Matcher<TableModelEvent> aChangeInRow(int rowIndex) {
        return samePropertyValuesAs(new TableModelEvent(model, rowIndex, rowIndex, TableModelEvent.ALL_COLUMNS, TableModelEvent.UPDATE));
    }
}
