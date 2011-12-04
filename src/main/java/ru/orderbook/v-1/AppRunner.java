package cmc.cmc1.orderbook;

import cmc.cmc1.orderbook.app.AppEnvironmentImpl;
import cmc.cmc1.orderbook.consumer.OrderConsumerImpl;
import cmc.cmc1.orderbook.iface.AppEnvironment;
import cmc.cmc1.orderbook.iface.LogLevel;

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
