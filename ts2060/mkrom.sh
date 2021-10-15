#! /bin/bash

reset
z80asm -l main.asm -o tc2068-0.rom
tail -c 8192 ../a.bin >> tc2068-0.rom
head -c 8192 ../a.bin > tc2068-1.rom
