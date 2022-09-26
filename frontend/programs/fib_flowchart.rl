int n int v int w

start: entry
  n ^= 0
  w ^= 1
goto l1

l1: fi v = 0 start l2
  v += w
if v < w end l2

l2: from l1
  swap v w
  n -= 1
if n = 0 end l1

end: fi n l1 l2
exit
