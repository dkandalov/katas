USE: math.functions
3 sq 4 sq + sqrt
! --- Data stack:
! 5.0

USE: ascii
"You" "Hello " swap append >upper
! --- Data stack:
! "HELLO YOU"

{ 1 4 17 9 11 } 0 [ + ] reduce
! --- Data stack:
! 42

100 [1,b] 0 [ + ] reduce
! --- Data stack:
! 5050

10 [1,b] [ sq ] map
! --- Data stack:
! { 1 4 9 16 25 36 49 64 81 100 }

42 [ 10 /i ] [ 10 mod ] bi
! --- Data stack:
! 4
! 2

USE: math.parser
234 number>string [ 1string string>number ] each
! --- Data stack:
! 2
! 3
! 4
