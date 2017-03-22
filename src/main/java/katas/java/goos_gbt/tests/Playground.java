package katas.java.goos_gbt.tests;

/**
 * User: dima
 * Date: 05/06/2012
 */
public class Playground {
    public static void main(String[] args) {
        new ApplicationRunner().startBiddingIn(new FakeAuctionServer("itemId"));
    }
}
