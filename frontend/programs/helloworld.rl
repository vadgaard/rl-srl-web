string r
string s
string t

l0: entry
  skip
goto l1

l1: from l0
  s += Hello World!
goto l6

l6: from l1
  skip
goto l7

l7: from l6
  t += World!
goto l4

l4: from l7
  skip
goto l5

l5: from l4
  r += s - t
goto l2

l2: from l5
  skip
goto l3

l3: from l2
  skip
exit
