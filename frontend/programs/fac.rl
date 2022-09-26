// Compute n!

int n int v

start: entry
  n ^= 5
  v ^= 1
goto loop

loop: fi (v = 1) start loop
  v *= n
  n -= 1
if (n = 0) end loop

end: from loop
exit
