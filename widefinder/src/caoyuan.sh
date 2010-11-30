#!/bin/sh
for X in 8 12 16 32 64 128 256; do
  echo "Process number: $X"
  time erl -smp +h 10240 -noshell -run tbray9 start o1000k.ap $X -s erlang halt > /dev/null
done
