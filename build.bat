REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del *.p
del *.lst
del *.sym

call zxasm chessBoard
call zxasm tetris

REM call will auto run emulator EightyOne if installed
REM comment in or out usin rem which one to run

REM call chessBoard.p
call tetris.p