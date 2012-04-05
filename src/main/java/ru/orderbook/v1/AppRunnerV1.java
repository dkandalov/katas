package ru.orderbook.v1;

import ru.orderbook.v1.app.AppEnvironmentImpl;
import ru.orderbook.v1.consumer.OrderConsumerImpl;
import ru.orderbook.v1.iface.AppEnvironment;
import ru.orderbook.v1.iface.LogLevel;

public class AppRunnerV1 {
    public static void main(String[] args) {
        AppEnvironment environment = new AppEnvironmentImpl(LogLevel.INFO);
        environment.registerHandler(new OrderConsumerImpl());
        environment.run();
    }
}
