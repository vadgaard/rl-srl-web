// Compute the n'th fibonacci pair

int n
int v int w

start: entry
    n ^= 16
    w ^= 1
goto loop_block

loop_block: fi (v = 0) start loop_block
    v += w
    swap v w
    n -= 1
if (n = 0 || v > w) end loop_block

end: from loop_block
exit
