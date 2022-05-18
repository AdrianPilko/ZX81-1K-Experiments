REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del tetris16K.p
del *.lst
del *.sym

REM call zxasm chessBoard
REM call zxasm tetris
call zxasm tetris16K

REM call will auto run emulator EightyOne if installed
REM comment in or out usin rem which one to run

REM call chessBoard.p
REM call tetris.p
call tetris16K.p