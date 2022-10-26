// Compute the n first squares

int n
list int q

start: entry
goto loop_block

loop_block: fi (empty q) start loop_block
    n += size q ** 2
    push n q
if (size q = 10) end loop_block

end: from loop_block
exit
