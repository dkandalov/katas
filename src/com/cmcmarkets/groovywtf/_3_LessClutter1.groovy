package com.cmcmarkets.groovywtf

import static java.math.RoundingMode.HALF_UP

// Java
public BigDecimal calcPrice(String symbol, BigDecimal price) {
  if (symbol.equals("AAA"))
    return new BigDecimal(1).divide(price, 10, HALF_UP);
  else
    return price;
}

// Groovy
BigDecimal calcPrice2(String symbol, BigDecimal price) {
  if (symbol == "AAA")
    1 / price
  else
    price
}

assert calcPrice("AAA", 123) == 0.0081300813
assert calcPrice2("AAA", 123) == 0.0081300813
assert calcPrice2("AAA", 123) == calcPrice("AAA", 123)




