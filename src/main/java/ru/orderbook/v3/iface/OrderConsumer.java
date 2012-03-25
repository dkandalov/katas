package ru.orderbook.v3.iface;

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
     * Handles specific event with order data.
     * Depending on action different set of order properties are meaningful:
     * <ul>
     * <li>For REMOVE: orderId</li>
     * <li>For EDIT: orderId, quantity and price</li>
     * <li>For ADD: orderId, symbol, isBuy, quantity, and price</li>
     * </ul>
     *
     * @param action The action.
     * @param order  The order DTO.
     */
    void handleEvent(Action action, Order order);

    /**
     * Called by the environment after processing all events.
     */
    void finishProcessing();
}
