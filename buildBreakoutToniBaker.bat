REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del breakoutToniBaker.p
del *.lst
del *.sym

call zxasm breakoutToniBaker

REM call will auto run emulator EightyOne if installed
REM comment in or out usin rem which one to run
call breakoutToniBaker.p