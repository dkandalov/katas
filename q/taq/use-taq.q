/ buildtaq.q should be run first to generate "db/taq" database

\l db/taq

show count trade
show count quote
show select count i by date from trade
show select from trade where date=last date, sym=`AMD

show "----- using timing (with slash-t) ------"
\t select from trade where sym=`IBM
\t do[100; select from trade where date=2013.05.21,sym=`IBM] / ~60ms
\t do[100; select from trade where sym=`IBM,date=2013.05.21] / ~1100ms

show "----- weighted average ------"
show 2 3 7 wavg 10 20 30
show select size wavg price by sym from trade

t:select from trade where date=last date,sym=`IBM
show select size wavg price from t
show select size wavg price by time.minute from t  / trades grouped for every minute TODO what is ".minute"?
show select size wavg price by 5 xbar time.minute from t  / trades grouped for every 5 minutes
show select lprice:last price, wprice:size wavg price by (15 xbar time.minute) from t


show "------ correlation -------"
show daily
t:exec close by get sym from daily
show t
show t`HPQ
show (t`HPQ) cor t`ORCL

/ correlation matrix
c:t cor/:\: t
show c
show desc c`HPQ

show "----- matching trades and quotes ------"
t:select time, price from trade where date=last date,sym=`IBM
q:select time, bid, ask from quote where date=last date,sym=`IBM
show aj[`time;t;q]  / latest quote for each trade
show aj[`time;q;t]  / latest trade for each quote


show "-------------"
show 5 # select close from daily where sym=`IBM  / '5 #' takes first 5 elements
show 5 # select ratios close from daily where sym=`IBM
show 5 # select 1 _ ratios close from daily where sym=`IBM  / '1 _' drops first element
show 5 # select log 1 _ ratios close from daily where sym=`IBM
show select dev log 1 _ ratios close from daily where sym=`IBM  / 'dev' - standard deviation
show select dev log 1 _ ratios close by sym from daily


exit 0