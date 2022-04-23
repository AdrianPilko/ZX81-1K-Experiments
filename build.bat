REM clean up before calling assembler 
del chessBoard.p
del chessBoard.lst
del chessBoard.sym

call zxasm chessBoard

REM call chessBoard.p will auto run emulator EightyOne if installed
call chessBoard.p