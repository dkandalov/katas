items:`nut`bolt`cam`cog
sales:6 8 0 3
prices:10 20 15 20

lol:(items;sales;prices)         / list of lists
show lol
show count lol
show count each lol


tab:([]items;sales;prices)       / table
/ could define it as "tab:([items]sales;prices)" then items would be the key
show "--tab--"
show tab
show 1!tab / table with first column as a key
show "--tab, tab--"
show tab, tab
show flip tab
show tab 0 2
show tab `sales

show "------ Selects ------"
show select from tab
show select from tab where prices > 12
show select sales, prices, total:sales*prices from tab
show select items, total:sales*prices, tottot:sums sales*prices from tab

exit 0