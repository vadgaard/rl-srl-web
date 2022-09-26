start: entry
goto dup

dup: from start
goto end

dup: from start
goto end

end: from dup
exit
