package ru.orderbook.v4;

import ru.orderbook.v4.app.FakeAppEnvironment;
import ru.orderbook.v4.consumer.OrderConsumerImpl;
import ru.orderbook.v4.iface.AppEnvironment;
import ru.orderbook.v4.iface.LogLevel;

public class AppRunner {

    /**
     * @param args
     */
    public static void main(String[] args) {
        AppEnvironment environment = new FakeAppEnvironment(LogLevel.INFO);
        environment.registerHandler(new OrderConsumerImpl());
        environment.run();
    }
}
