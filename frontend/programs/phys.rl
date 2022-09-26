int v // cm/s
int y // cm
int s // s
int S

start: entry
  y ^= 16400 // cm
  S ^= 10000 // Scale factor

  y *= S
  goto loop

loop: fi v=0 start loop
  y -= v/S
  v += 981 // cm/s^2
  s += 1
  if y <= 0 end loop

end: from loop
  exit

