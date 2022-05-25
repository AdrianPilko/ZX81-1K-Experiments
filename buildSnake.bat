REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del snake16K.p
del *.lst
del *.sym

call zxasm snake16K
call snake16K.p