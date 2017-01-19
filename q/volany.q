/ volatility for list of stocks
//
/ use as:
/ q)volany `AAPL`GOOG`IBM
/ sym | close
/ ----| ----------
/ GOOG| 0.0135363
/ AAPL| 0.0123217
/ IBM | 0.01005901
//
/ for all stocks:
/ q)volany sym

volany:{[portfolio]
 desc select dev log 1 _ ratios close by sym from daily
 where sym in portfolio}
