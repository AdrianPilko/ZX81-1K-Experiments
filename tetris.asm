;;;;;;;;;;;;;;;;;;;;;
;;; zx81 1K code
;;; This is minimal code that will run on the limited memory of standard unexpanded 1K zx81
;;; It's a clone of tetris (in case that wasn't clear from filename;)
;;; The code heavily!! dependant on the definition of the screen memory in screenTetris.asm
;;;;;;;;;;;;;;;;;;;;;

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"
#include "line1.asm"

	jp intro_title		; jump to print title text
	

;; the underscore characters here are mapped onto real zx81 
;; characters in charcodes.asm, they are more human readble 
;; shortcuts, than decimal equivalent 
title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_1,_K,__,_T,_E,_T,_R,_I,_S,$ff

;; intro screen
intro_title
	; no need for clear screen as screenTetris.asm has already set 
    ; everything to zero    
	ld bc,0                     ; printstring from offset from DF__CC stored in bc
	ld de,title_screen_txt      ; load text into de for printstring
	call printstring	
	
	
;; initialise "variables" and memory

;; main game loop

;; generate shape

;; user input to retate shape

;; scroll shapes down

;; detect if line(s) has/have been completed and remove, and drop remaining down

;; increase score if line(s) removed

;; detect if game over condition met, it collision between shape entering at top


	jp intro_title   ; never return to basic, new game always starts from title screen
		
; this prints at top any offset (stored in bc) from the top of the screen D_FILE
printstring
	ld hl,(DF_CC)
	add hl,bc	
printstring_loop
	ld a,(de)
	cp $ff
	jp z,printstring_end
	ld (hl),a
	inc hl
	inc de
	jr printstring_loop
printstring_end	
	ret

#include "line2.asm"
#include "screenTetris.asm"      			; definition of the screen memory, in colapsed version for 1K        
#include "endbasic.asm"
