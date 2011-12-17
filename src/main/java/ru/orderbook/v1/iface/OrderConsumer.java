package ru.orderbook.v1.iface;

/**
 * This interface is not thread-safe.
 */
public interface OrderConsumer {
    /**
     * Called by environment before any events are processed.
     *
     * @param log The log to be used during processing.
     */
    void startProcessing(Log log);

    /**
     * Handles specific event with order data. The meaningful properties of the order depends on the
     * action. Note that the rest properties are with unspecified, but in usual cases invalid
     * values. meaningful props are:
     * <ul>
     * <li>For REMOVE: orderId</li>
     * <li>For EDIT: orderId, quantity and price</li>
     * <li>For ADD: orderId, symbol, isBuy, quantity, and price</li>
     *
     * @param action The action.
     * @param o      The order DTO.
     */
    void handleEvent(Action action, Order order);

    /**
     * Called by the environment when no more events will be handled.
     */
    void finishProcessing();
}
