package ru.orderbook.v4_golf.iface;

/**
 * Specifies the behavior of the application environment.
 * <br />Typical usage:
 * <pre>
 * <code>AppEnvironment env = ...
 * env.registerHandler(handler1);
 * env.registerHandler(handler2);
 * env.run();
 * </code>
 * </pre>
 */
public interface AppEnvironment {
    void registerHandler(OrderConsumer handler);

    void run();
}
