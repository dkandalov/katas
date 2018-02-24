USING: kernel sequences ;
IN: examples.greeter

: greeting ( name -- greeting ) "Hello, " swap append ;
