\l /Users/dima/IdeaProjects/katas/src/main/q/assert.q

/ ":" is assignment
show "1) -------------"
x:2 5 4 7 5
expect[count x; toEqual[5]]
expect[2 * 5 - 3; toEqual[4]]
expect[(2 * 5) - 3; toEqual[7]]

show "2) -------------"
f:{2+3*x} / x,y,z - are default names for function parameters
expect[f 10; toEqual[32]]
expect[til 5; toEqual[0 1 2 3 4]]  / <-- great error message by Q, just says "type" not sure what's wrong with it
show f til 5

addtax:{[rate;sale] sale * 1 + rate % 100}
show addtax [7.5; 100 200 500]

show "3) -------------"
x:2 5 4 7 5
show x 3
show x 0 3 2 3
(x 2 4):123 234
show x

show "4) -------------"
x:(til 4; "hello")
show x
show reverse x
show reverse each x / "each" here is "adverb"
/ show reverse reverse each x
show 1 2 3 +/: 10 20
show 1 2 3 +\: 10 20
show 1 2 3 *\: 10 20

show "5) -------------"
r:1 1
x:10
while[x-:1;r,:sum -2#r]
show r

show "x) -------------"
show "hello Q"

exit 0