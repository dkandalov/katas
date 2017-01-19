
times:10:01:01 + 0 3 7
price:101 + 0 3 7
t:([]times;price)               / trade

w:(times-2;times+1)             / window

len:8
times:10:01:01+til len
bid:100+-2 -1 2 3 2 4 6 7
bsize:10+len?50
ask:100+2 3 3 4 4 7 8 9
asize:10+len?50
q:([]times;bid;ask;bsize;asize) / quote

t
q
w

wj[w;enlist`times;t;(q;(::;`bid);(::;`ask))]

wj[w;enlist`times;t;(q;(min;`bid);(max;`ask))]

wj[w;enlist`times;t;(q;(last;`bid);(last;`ask))]

r:wj[w;enlist`times;t;(q;(::;`bsize);
 (::;`bid);(::;`asize);(::;`ask))]

select times,price,
 avgbid:bsize wavg' bid,
 avgask:asize wavg' ask from r
