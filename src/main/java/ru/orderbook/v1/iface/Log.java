package ru.orderbook.v1.iface;

/**
 * Logging facade for the application.
 */
public interface Log {
    void log(LogLevel logLevel, String msg);
}
