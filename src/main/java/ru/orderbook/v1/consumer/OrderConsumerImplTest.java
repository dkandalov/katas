package ru.orderbook.v1.consumer;

import org.junit.Test;
import ru.orderbook.v1.iface.Order;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static ru.orderbook.v1.iface.Action.*;

/**
 * User: dima
 * Date: 17/2/11
 */
public class OrderConsumerImplTest {
    @Test
    public void shouldAddOrdersToOrderBook() {
        OrderConsumerImpl consumer = new OrderConsumerImpl();

        consumer.handleEvent(ADD, new Order(1, "AAA", true, 123, 2));
        consumer.handleEvent(ADD, new Order(2, "AAA", true, 123, 3));
        consumer.handleEvent(ADD, new Order(3, "AAA", false, 234, 8));
        consumer.handleEvent(ADD, new Order(4, "AAA", false, 235, 8));

        assertThat(consumer.orderBookFor("AAA"), equalTo(
                new OrderBook("AAA")
                        .withBidLevel(123, 5, 2)
                        .withAskLevel(234, 8, 1)
                        .withAskLevel(235, 8, 1)
        ));
    }

    @Test
    public void shouldRemoveOrdersFromOrderBook() {
        OrderConsumerImpl consumer = new OrderConsumerImpl();

        consumer.handleEvent(ADD, new Order(1, "AAA", true, 123, 2));
        consumer.handleEvent(ADD, new Order(2, "AAA", true, 123, 3));
        consumer.handleEvent(ADD, new Order(3, "AAA", false, 234, 8));
        consumer.handleEvent(ADD, new Order(4, "AAA", false, 235, 8));
        assertThat(consumer.orderBookFor("AAA"), equalTo(
                new OrderBook("AAA")
                        .withBidLevel(123, 5, 2)
                        .withAskLevel(234, 8, 1)
                        .withAskLevel(235, 8, 1)
        ));

        consumer.handleEvent(REMOVE, new Order(1, null, false, 0, 0));
        consumer.handleEvent(REMOVE, new Order(2, null, false, 0, 0));
        consumer.handleEvent(REMOVE, new Order(3, null, false, 0, 0));
        assertThat(consumer.orderBookFor("AAA"), equalTo(
                new OrderBook("AAA")
//                        .withBidLevel(123, 0, 0) // was copied as "ask" instead of "bid"
//                        .withAskLevel(234, 0, 0)
                        .withAskLevel(235, 8, 1)
        ));
    }

    @Test
    public void shouldEditOrdersInOrderBook() {
        OrderConsumerImpl consumer = new OrderConsumerImpl();

        consumer.handleEvent(ADD, new Order(1, "AAA", true, 123, 2));
        consumer.handleEvent(ADD, new Order(2, "AAA", true, 123, 3));
        assertThat(consumer.orderBookFor("AAA"), equalTo(
                new OrderBook("AAA")
                        .withBidLevel(123, 5, 2)
        ));

        consumer.handleEvent(EDIT, new Order(2, null, false, 124, 8));

        assertThat(consumer.orderBookFor("AAA"), equalTo(
                new OrderBook("AAA")
                        .withBidLevel(123, 2, 1)
                        .withBidLevel(124, 8, 1)
        ));
    }

    @Test
    public void testCaseFromProvidedCodeShouldYieldCorrectResults() {
        OrderConsumerImpl consumer = new OrderConsumerImpl();

        consumer.handleEvent(ADD, new Order(1L, "MSFT.L", true, 5, 200));
        consumer.handleEvent(ADD, new Order(2L, "VOD.L", true, 15, 100));
        consumer.handleEvent(ADD, new Order(3L, "MSFT.L", false, 5, 300));
        consumer.handleEvent(ADD, new Order(4L, "MSFT.L", true, 7, 150));
        consumer.handleEvent(REMOVE, new Order(1L, null, true, -1, -1));
        consumer.handleEvent(ADD, new Order(5L, "VOD.L", false, 17, 300));
        consumer.handleEvent(ADD, new Order(6L, "VOD.L", true, 12, 150));
        consumer.handleEvent(EDIT, new Order(3L, null, true, 7, 200));
        consumer.handleEvent(ADD, new Order(7L, "VOD.L", false, 16, 100));
        consumer.handleEvent(ADD, new Order(8L, "VOD.L", false, 19, 100));
        consumer.handleEvent(ADD, new Order(9L, "VOD.L", false, 21, 112));
        consumer.handleEvent(REMOVE, new Order(5L, null, false, -1, -1));

        assertThat(consumer.orderBookFor("MSFT.L"), equalTo(
                new OrderBook("MSFT.L")
                        .withBidLevel(7, 150, 1) // was incorrect assertion (just not attentive)
                        .withAskLevel(7, 200, 1)
                // didn't consider that I was keeping Levels with zero count.. later added code to remove them
        ));
        assertThat(consumer.orderBookFor("VOD.L"), equalTo(
                new OrderBook("VOD.L")
                        .withBidLevel(15, 100, 1) // was incorrect assertion (just not attentive)
                        .withBidLevel(12, 150, 1) // was incorrect assertion (just not attentive)
                        .withAskLevel(16, 100, 1)
                        .withAskLevel(19, 100, 1)
                        .withAskLevel(21, 112, 1)
        ));
    }
}
