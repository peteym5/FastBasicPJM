REM Pascal Switches
REM -d: address diagnostic mode
REM  -code: $ address start address of the program
REM  -data: $ address memory address for variables, arrays
REM  -stack: $ address memory address for the software stack (64 bytes)
REM  -page: $ address address on the zero page for variables (24 bytes)
 
setlocal

set PATH=%PATH%;..\cc65\fastbasic;..\cc65\bin

fastbasic-fp "TETRIS FAST.BAS" "TETRIS FAST.ASM"

cl65 -t atari -C fastbasic.cfg "TETRIS FAST.ASM" -o "TETRISFB.XEX" fastbasic-fp.lib

pause
