/*
  This program should throw an error if short
  circuiting is not implemented correctly
*/

list int q
int n

start: entry
goto test

test: fi (?q) start count
if (?q || (^q != 1)) count end

count: from test
  n += 10 - #q
  push n q
goto test

end: from test
exit
