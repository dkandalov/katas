package katas.java.cmc.orderbook.v1.app;


import katas.java.cmc.orderbook.v1.iface.*;

import java.util.LinkedHashSet;
import java.util.Set;


public abstract class AbstractAppEnvironment implements AppEnvironment {
    private final Set<OrderConsumer> consumers = new LinkedHashSet<OrderConsumer>();
    private final LogLevel logLevel;
    /**
     * Implementation of {@link Log} which uses the standard out.
     */
    protected final Log log = new Log() {
        @Override
        public void log(LogLevel logLevel, String msg) {
            if (isEnabled(logLevel)) {
                System.out.println(logLevel + ": " + msg);
            }
        }

        private boolean isEnabled(LogLevel logLevel) {
            return logLevel.compareTo(AbstractAppEnvironment.this.logLevel) >= 0;
        }
    };

    public AbstractAppEnvironment(LogLevel logLevel) {
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
            e.printStackTrace();
            log.log(LogLevel.ERROR, e.getMessage());
        } finally {
            notifyFinish();
        }
    }

    /**
     * Sends a stream of orders to {@link OrderConsumer}s.
     *
     * @throws Exception if there is an error.
     * @see #notifyOrder(Action, Order)
     */
    protected abstract void feedOrders() throws Exception;

    /**
     * Invokes {@link OrderConsumer#handleEvent(Action, Order)} for every registered consumer with
     * specified <code>action</code> and <code>order</code>.
     */
    protected void notifyOrder(Action action, Order order) {
        for (OrderConsumer consumer : consumers) {
            consumer.handleEvent(action, order);
        }
    }

    private void notifyStart() {
        for (OrderConsumer consumer : consumers) {
            consumer.startProcessing(log);
        }
    }

    private void notifyFinish() {
        for (OrderConsumer consumer : consumers) {
            consumer.finishProcessing();
        }
    }
}
