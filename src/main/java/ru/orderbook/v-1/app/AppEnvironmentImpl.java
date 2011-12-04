package cmc.cmc1.orderbook.app;


import cmc.cmc1.orderbook.iface.*;

import java.util.LinkedHashSet;
import java.util.Set;


public class AppEnvironmentImpl implements AppEnvironment {
    private final Set<OrderConsumer> consumers = new LinkedHashSet<OrderConsumer>();
    private final LogLevel logLevel;
    /**
     * Implementation of the {@link Log} facade which uses the standard out.
     */
    protected final Log log = new Log() {
        @Override
        public void log(LogLevel logLevel, String msg) {
            if (!isEnabled(logLevel)) {

            }
            System.out.println(logLevel + ": " + msg);
        }

        private boolean isEnabled(LogLevel logLevel) {
            return logLevel.compareTo(AppEnvironmentImpl.this.logLevel) >= 0;
        }
    };

    public AppEnvironmentImpl(LogLevel logLevel) {
        this.logLevel = logLevel;
    }

    @Override
    public void registerHandler(OrderConsumer handler) {
        consumers.add(handler);
    }

    @Override
    public final void run() {
        notifyStart();
        try {
            feedOrders();
        } catch (Exception e) {
            log.log(LogLevel.ERROR, e.getMessage());
        } finally {
            notifyFinish();
        }
    }

    /**
     * Sends a stream of orders to the {@link OrderConsumer}s.
     *
     * @throws Exception if there is an error.
     * @see #notifyOrder(Action, Order)
     */
    protected void feedOrders() throws Exception {
        /*
         * This implementation creates a dummy array with commands and sends them to the order
         * handlers.
         */
        class Command {
            final Action action;
            final Order order;

            public Command(Action action, long orderId, String symbol, boolean isBuy, int price,
                           int quantity) {
                this.action = action;
                this.order = new Order(orderId, symbol, isBuy, price, quantity);
            }

        }
        Command[] commands =
                new Command[]{new Command(Action.ADD, 1L, "MSFT.L", true, 5, 200),
                        new Command(Action.ADD, 2L, "VOD.L", true, 15, 100),
                        new Command(Action.ADD, 3L, "MSFT.L", false, 5, 300),
                        new Command(Action.ADD, 4L, "MSFT.L", true, 7, 150),
                        new Command(Action.REMOVE, 1L, null, true, -1, -1),
                        new Command(Action.ADD, 5L, "VOD.L", false, 17, 300),
                        new Command(Action.ADD, 6L, "VOD.L", true, 12, 150),
                        new Command(Action.EDIT, 3L, null, true, 7, 200),
                        new Command(Action.ADD, 7L, "VOD.L", false, 16, 100),
                        new Command(Action.ADD, 8L, "VOD.L", false, 19, 100),
                        new Command(Action.ADD, 9L, "VOD.L", false, 21, 112),
                        new Command(Action.REMOVE, 5L, null, false, -1, -1)};

        for (Command command : commands) {
            notifyOrder(command.action, command.order);
        }

    }

    /**
     * Invokes {@link OrderConsumer#handleEvent(Action, Order)} for every registered consumer with
     * specified <code>action</code> and <code>order</code>.
     */
    protected final void notifyOrder(Action action, Order order) {
        for (OrderConsumer consumer : consumers) {
            consumer.handleEvent(action, order);
        }
    }

    private final void notifyStart() {
        for (OrderConsumer consumer : consumers) {
            consumer.startProcessing(log);
        }
    }

    private final void notifyFinish() {
        for (OrderConsumer consumer : consumers) {
            consumer.finishProcessing();
        }
    }
}
