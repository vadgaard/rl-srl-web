// Convert binary to decimal

int n int i
list int q

start: entry
  init q [10]

  q[2] += 1
  q[5] += 1
  q[9] += 1
  q[8] += 1
goto loop

loop: fi (i = 0) start loop
  n += (2 * q[size q - i - 1]) ** i
  i += 1
if (i = size q) end loop

end: from loop
  i -= size q
exit
