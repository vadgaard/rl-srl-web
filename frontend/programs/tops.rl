// A program to test nested top expressions

int n
list int q
list list int p

start: entry
  n ^= 3
  push n q
  n ^= 9
  push n q
  push q p

  n += top (top p)

exit
