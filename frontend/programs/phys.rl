// Physics simulation of an object falling

int v // cm/s
int y // cm
int s // s
int S

start: entry
    y ^= 16400 // cm
    S ^= 10000 // Scale factor

    y *= S
goto loop_block

loop_block: fi (v = 0) start loop_block
    y -= v/S
    v += 981 // cm/s^2
    s += 1
if (y <= 0) end loop_block

end: from loop_block
exit
