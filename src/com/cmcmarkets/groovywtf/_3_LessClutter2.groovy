package com.cmcmarkets.groovywtf

private static class Trade {
  String symbol
  String instrumentId
  String accountId
}

// Java
Trade trade = new Trade();
trade.setSymbol("AAA");
trade.setInstrumentId("123");
trade.setAccountId("456");

// Groovy
trade.with {
  symbol = "AAA"
  instrumentId = "123"
  accountId = "456"
}





