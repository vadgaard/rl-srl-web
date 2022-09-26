// Compute the accumulated sum

int n
int v

start: entry
  n ^= 10
goto loop

loop: fi (v = 0) start loop
  v += n
  n -= 1
if (n) loop end

end: from loop
exit
