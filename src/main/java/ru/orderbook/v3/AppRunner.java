package ru.orderbook.v3;

import ru.orderbook.v3.app.AppEnvironmentImpl;
import ru.orderbook.v3.consumer.OrderConsumerImpl;
import ru.orderbook.v3.iface.AppEnvironment;
import ru.orderbook.v3.iface.LogLevel;

public class AppRunner {

    /**
     * @param args
     */
    public static void main(String[] args) {
        AppEnvironment environment = new AppEnvironmentImpl(LogLevel.INFO);
        environment.registerHandler(new OrderConsumerImpl());
        environment.run();
    }
}
