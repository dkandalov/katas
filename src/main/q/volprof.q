
\l db/taq

/ volprof -------------------------------------------------
select sum size by 5 xbar time.minute from trade where sym=`AAPL

/ same, but get sum size by date, then average:
t:select sum size by date,5 xbar time.minute
 from trade where sym=`AAPL
select avg size by minute from t

/ if plot available:
plot select avg size by minute from t
