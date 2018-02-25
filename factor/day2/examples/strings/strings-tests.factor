USING: examples.strings tools.test ;
IN: examples.strings.tests

{ t } [ "a" palindrome? ] unit-test
{ f } [ "ab" palindrome? ] unit-test
{ t } [ "aba" palindrome? ] unit-test
{ t } [ "racecar" palindrome? ] unit-test
