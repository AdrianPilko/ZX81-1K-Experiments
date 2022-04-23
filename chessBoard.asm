;;;;;;;;;;;;;;;;;;;;;
;;; minimal zx81 code
;;;;;;;;;;;;;;;;;;;;;


#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"
#include "line1.asm"

#define SCREEN_SCROLL_MEM_OFFSET 365
	
	jp intro_title
	
var_scroll_road_from
	DEFB 0,0
var_scroll_road_to
	DEFB 0,0

;title_screen_txt
;	DEFB	_Z,_X,_8,_1,__,_R,_A,_C,_I,_N,_G,$ff

	
intro_title
	ld hl,(D_FILE) ;initialise road start memory address
	ld de, SCREEN_SCROLL_MEM_OFFSET
	add hl, de	
	ld (var_scroll_road_from), hl
	ld de, 33
	add hl, de
	ld (var_scroll_road_to), hl
	call CLS	
	ld e, 8
	
scroll_loop
	ld b, e
	ld c, 0
	push de	
	call PRINTAT
	ld a, e
	and 1	; odd even of e countdown (0 or 1)
	jp nz, printEvenLine	
	ld bc,4
	ld a, 0
innerLoop1	
	ld a, 0	
	call PRINT
	add a, 128	
	call PRINT
	dec bc
	ld a, c
	cp 0
	jp nz, innerLoop1
	jp loopOuterControl
printEvenLine
	ld bc,4
	ld a, 128		
innerLoop2	
	ld a, 128
	call PRINT
	sub 128	
	call PRINT
	dec bc
	ld a, c
	cp 0
	jp nz, innerLoop2
loopOuterControl
	pop de
	dec e
	ld a, e
	cp 0
	jp nz,scroll_loop	
	
	ret


; this prints at top any offset (stored in bc) from the top of the screen D_FILE
;printstring
;	ld hl,(D_FILE)
;	add hl,bc	
;printstring_loop
;	ld a,(de)
;	cp $ff
;	jp z,printstring_end
;	ld (hl),a
;	inc hl
;	inc de
;	jr printstring_loop
;printstring_end	
;	ret

#include "line2.asm"
#include "screen.asm"               
#include "endbasic.asm"
