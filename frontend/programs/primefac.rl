// Compute the prime factors of n

int v int n
list int p

start: entry
    n += 1829
    v += 2
goto loop_from

loop_from: fi (empty p and v = 2) start loop_until
if (n % v = 0) if_true if_false

if_true: from loop_from
    n /= v
    push v p
    v ^= top p
goto if_assert

if_false: from loop_from
    v += 1
goto if_assert

if_assert: fi (not (empty p) and top p = v) if_true if_false
goto loop_loop

loop_loop: from if_assert
    skip
goto loop_until

loop_until: from loop_loop
if (n = 1) end loop_from

end: from loop_until
    v -= top p
    n -= 1
exit
