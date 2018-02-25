USING: kernel sequences ;
IN: examples.strings

: palindrome? ( s -- b ) dup <reversed> sequence= ;
