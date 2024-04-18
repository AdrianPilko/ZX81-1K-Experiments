REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del clock.p
del *.lst
del *.sym

call zxasm clock
call clock.p