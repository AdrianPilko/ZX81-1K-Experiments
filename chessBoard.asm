;;;;;;;;;;;;;;;;;;;;;
;;; zx81 1K code
;;; This is minimal code that will run on the limited memory of standard unexpanded 1K zx81
;;; prints a chess board and some text at the top
;;; The code heavily!! dependant on the definition of the screen memory in screen.asm
;;;;;;;;;;;;;;;;;;;;;

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"
#include "line1.asm"

	jp intro_title		; jump to print title text
	

title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_1,_K,$ff

	
intro_title
	; no need for clear screen as screen.asm has already set everything to zero
	ld bc,0
	ld de,title_screen_txt
	call printstring	
	ld de, $09

scroll_loop
	ld b, e				; row set for PRINTAT
	ld c, 0				; column set for PRINTAT
	push de	
	call PRINTAT		; ROM routine to set current cursor position, from row b and column e
	ld a, e
	and 1				; odd even of e countdown (0 or 1)
	jp nz, printEvenLine	
	ld bc,4				; loop 4 times, each loop prints 2 characters to give full 8 by 8 chessboard
	ld a, 0
innerLoop1	
	ld a, 8				; character code for grey square
	call PRINT			; call ROM routine to print a character
	ld a, 128	 		; character code for black square
	call PRINT			; call ROM routine to print a character
	dec bc
	ld a, c
	cp 0
	jp nz, innerLoop1
	jp loopOuterControl
printEvenLine
	ld bc,4
innerLoop2	
	ld a, 128			; character code for black square
	call PRINT			; call ROM routine to print a character
	ld a, 8				; character code for grey square
	call PRINT			; call ROM routine to print a character
	dec bc
	ld a, c
	cp 0
	jp nz, innerLoop2
loopOuterControl
	pop de
	dec e
	ld a, e
	cp 1
	jp nz,scroll_loop	
	ret	; this does return to basic, but due to the screen setup it crashes if run again
		; the zx81 will attempt to change the screen memory dynamically as it lists the program
		; so all the offsets we setup here based on a small screen config will be
		; wrong
		
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
#include "screen.asm"      			; definition of the screen memory, in colapsed version for 1K        
#include "endbasic.asm"
