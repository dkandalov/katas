package katas.groovy.orderbook.v4_golf.app;

import katas.java.cmc.orderbook.v1.app.AbstractAppEnvironment;
import katas.java.cmc.orderbook.v1.iface.Action;
import katas.java.cmc.orderbook.v1.iface.LogLevel;
import katas.java.cmc.orderbook.v1.iface.Order;

/**
 * User: dima
 * Date: 05/04/2012
 */
public class FakeAppEnvironment extends AbstractAppEnvironment {
    public FakeAppEnvironment(LogLevel logLevel) {
        super(logLevel);
    }

    @Override
    protected void feedOrders() throws Exception {
        class Command {
            final Action action;
            final Order order;

            public Command(Action action, long orderId, String symbol, boolean isBuy, int price, int quantity) {
                this.action = action;
                this.order = new Order(orderId, symbol, isBuy, price, quantity);
            }

        }
        Command[] commands =
                new Command[]{
                        new Command(Action.ADD, 1L, "MSFT.L", true, 5, 200),
                        new Command(Action.ADD, 2L, "VOD.L", true, 15, 100),
                        new Command(Action.ADD, 3L, "MSFT.L", false, 5, 300),
                        new Command(Action.ADD, 4L, "MSFT.L", true, 7, 150),
                        new Command(Action.REMOVE, 1L, null, true, -1, -1),
                        new Command(Action.ADD, 5L, "VOD.L", false, 17, 300),
                        new Command(Action.ADD, 6L, "VOD.L", true, 12, 150),
                        new Command(Action.EDIT, 3L, null, true, 7, 200),
                        new Command(Action.ADD, 7L, "VOD.L", false, 16, 100),
                        new Command(Action.ADD, 8L, "VOD.L", false, 19, 100),
                        new Command(Action.ADD, 9L, "VOD.L", false, 21, 112),
                        new Command(Action.REMOVE, 5L, null, false, -1, -1)};

        for (Command command : commands) {
            notifyOrder(command.action, command.order);
        }
    }
}
