package ru.orderbook.v1;

import ru.orderbook.v1.app.XmlAppEnvironment;
import ru.orderbook.v1.consumer.OrderConsumerImpl;
import ru.orderbook.v1.iface.AppEnvironment;
import ru.orderbook.v1.iface.LogLevel;

public class AppRunnerV1 {
    private static final String PATH = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/";

    public static void main(String[] args) {
//        AppEnvironment environment = new FakeAppEnvironment(LogLevel.INFO);
        AppEnvironment environment = new XmlAppEnvironment(LogLevel.INFO, PATH + "orders1.xml");
        environment.registerHandler(new OrderConsumerImpl());
        environment.run();
    }
}
