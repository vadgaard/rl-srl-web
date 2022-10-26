// Compute the accumulated sum

int n
int v

start: entry
    n ^= 10
goto loop_block

loop_block: fi (v = 0) start loop_block
    v += n
    n -= 1
if n loop_block end

end: from loop_block
exit
