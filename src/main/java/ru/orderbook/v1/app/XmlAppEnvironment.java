package ru.orderbook.v1.app;

import ru.orderbook.v1.iface.LogLevel;

/**
 * User: dima
 * Date: 05/04/2012
 */
public class XmlAppEnvironment extends AbstractAppEnvironment {
    private final String fileName;

    public XmlAppEnvironment(LogLevel logLevel, String fileName) {
        super(logLevel);
        this.fileName = fileName;
    }

    @Override
    protected void feedOrders() throws Exception {
        // TODO
    }
}
