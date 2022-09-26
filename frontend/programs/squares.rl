// Compute the n first squares

int n
list int q

start: entry
goto loop

loop: fi (empty q) start loop
  n += size q ** 2
  push n q
if (size q = 10) end loop

end: from loop
exit
