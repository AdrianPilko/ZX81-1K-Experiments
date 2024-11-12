
;; Minimal machine code from Dr.Beep's book: The Ulitmate 1K ZX81 coding book
;; Typed in here to recreate including comments. The book is the best in depth 
;; description of the ZX81 I've ever read, both low and high res on standard zx81

; 12 bytes bytes from $4000 to $400b free reuable for own code

   org $4009

; in LOWRES more sysvar are used, but in tis way the shortest code over sysvar
; to start machine code. This saves 11 bytes of BASIC

; DON NOT CHANGE AFTER BASIC+3 (=DFILE)

basic    ld h,dfile/256  ; high(MSB) of dfile
         jr init1
         db 236          ; BASIC over DFILE data
         db 212, 28      ; GOTO USR 0
         db 126,143,0,18 ; short FP nr of $4009

eline    dw last
chadd    dw last-1
         db 0,0,0,0,0,0  ; x not useable
berg     db 0            ; x before loading
mem      db 0,0          ; x OVERWRITTEN ON LOAD

init1    ld l, dfile mod 256 ; low byte of dfile
         jr init2

lastk    db 255,255,255
margin   db 55

nxtlin   dw basic        ; the called BASIC-line

flagx    equ init2+2
init2    ld (basic+3),hl ; repair dfile pointer
         ld l, vars mod 256 ; lsb-end of screen
         db 0
         db 0            ; x used by ZX81
         ld h, vars/256  ; msb-end of screen

frames   db $37          ; after load: ld (hl), n
         db $e9          ; set jp (hl) as end of screen-marker
 
         xor a
         ex af, af'      ; delay interrupts
         jp gamecode     ; goto your gamecode, th can be everywhere

cdflag   db 64

; DO NOT CHANGE SYSVAR ABOVE!

; free codeable memory
gamecode 
         jr gamecode

; the display file, Code the lines needed.
dfile    db 118
         db "D"-27,"E"-27,"M"-27,"O"-27,0,"D"-27,"R"-27
         db $1b,"B"-27,"E"-27,"E"-27,"P"-27,118

;; more than 32 characters on a display line!!!
;; if instead of end of line (118 = halt instruction) you just 
;; put 2 characters, then ZX81 will display them
;; dr.beep's book suggests the 34th character cannot be inverse
;; however my emulator does display that. not tried on real ZX81 yet!
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,"H"-27+128         
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,"H"-27+128        
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,1,1,1,1,1,1,1
         db 1,1,1,"H"-27+128       
         db 0,128,0,128,0,128,0,128,0,128
         db 0,128,0,128,0,128,0,128,0,128
         db 0,128,0,128,0,128,0,128,0,128
         db 0,128,118
         db $e9   ; jp (hl) fills rest of screen
vars     db 128          ; becomes end of screen
last     equ $
end

