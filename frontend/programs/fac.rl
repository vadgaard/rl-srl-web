// Compute n!

int n int v

start: entry
    n ^= 5
    v ^= 1
goto loop_block

loop_block: fi (v = 1) start loop_block
    v *= n
    n -= 1
if (n = 0) end loop_block

end: from loop_block
exit
