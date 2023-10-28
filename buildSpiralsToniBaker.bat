REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del spiralsToniBaker.p
del *.lst
del *.sym

call zxasm spiralsToniBaker

REM call will auto run emulator EightyOne if installed
REM comme0nt in or out usin rem which one to run
IF EXIST "spiralsToniBaker.p" (
  call spiralsToniBaker.p
) ELSE (
  PAUSE
)