REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del doodle16K.p
del *.lst
del *.sym

call zxasm doodle16K
call doodle16K.p