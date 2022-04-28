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

#define SHAPE_CHAR   128        ; black square
	jp intro_title		        ; jump to print title text
	

;; the underscore characters here are mapped onto real zx81 
;; characters in charcodes.asm, they are more human readble 
;; shortcuts, than decimal equivalent 
title_screen_txt
	DEFB	__,_T,_E,_T,_R,_I,_S,$ff
currentShape    
    DEFB 0

;; intro screen
intro_title
	; no need for clear screen as screenTetris.asm has already set 
    ; everything to zero    
	ld bc,0                     ; printstring from offset from DF__CC stored in bc
	ld de,title_screen_txt      ; load text into de for printstring
	call printstring	
	jp main
	
;; initialise "variables" and memory
shapes      ; base shape stored in upright positions, as they start at top, 2column * 4 rows to make logic easier
            ; e.g. normal L is  10  rev L  01  square 11  T  01  4inrow 01
            ;                   10         01         11     11         01  
            ;                   10         01         00     01         01
            ;                   11         11         00     00         01
;    DEFB  1,1,1,1,0,0,0,0       ; square
;    DEFB  1,0,1,0,1,0,1,1       ; normal L
;    DEFB  0,1,0,1,0,1,1,1       ; reverse L
;    DEFB  0,1,1,1,0,1,0,0       ; T
;    DEFB  0,1,0,1,0,1,0,1       ; 4 in row
; alternative shape definition (bit packed)
   DEFB %00111100,%00101011,%00010111,%00011101,%00010101 
;
;
    
shape_row_index     ; the current row of the top of the falling shape
    DEFB 0
shape_col_index     ; the current column of the top left of the falling shape
    DEFB 0
outerCount  
    DEFB 0,0
currentShapeOffset    
    DEFB 0
 
    ;; main game loop
main
    ld a, 13
    ld (shape_row_index),a
    ;; generate shape   
    
    ;generate a random number between 0 and 4 
    ;ld de, 0
    ;ld bc, 0
    ;ld hl, 0
    ;ld a, 0 
    ;xor a
    ;call random             ; BUG - never returns?????                 
    and $03
    ld a, r
    and $03
    ld (currentShapeOffset), a
dropLoop        
    ld a, (shape_row_index)
    add a, 10
    ld (shape_row_index), a
    
    ld a, (currentShapeOffset)
    ld hl, shapes
    ld d, 0                            
    ld e, a                            ; add the random (0 to 3) offset to hl to get value of shape
    add hl, de
    ld a, (hl)    
    ld (currentShape), a
    ; draw shape at next row    
    ld hl, (DF_CC)
    ld de, (shape_row_index)            ; add offset to top of screen memory to skip title    
;; this will only draw shape at top need to add current position offset
    add hl, de                          ; to where we want to draw shape
    ld c, %10000000                     ; mask for shape (initialised, but will be rotated  )
    ld e, 4
drawShapeOuter    
    ld b, 2                             ; b now stores max length of definition of shape (i.e. 1 byte)
drawShapeInner
    ld a, (currentShape)    
    and c                               ; set to block or no block based on (shapes)     
    jp z, drawSpace
    ld (hl), SHAPE_CHAR    
    jr carryOn
drawSpace    
    ld (hl), 0
carryOn
    inc hl
    xor a
    ld a, c    
    rra                                 ; rotate mask to right by one bit
    ld c, a
    djnz drawShapeInner                 ; dnjz decrements b and jumps if not zero
    ld (outerCount), de                 ; store loop count temp
    ld de, 8    
    add hl, de                          ; gets current screen position to next row
    ld de, (outerCount)                 ; retreive  loop count temp
    dec e   
    ld a, e
    cp 0  
    jp nz, drawShapeOuter

	ld bc, $05ff
waitloop
	dec bc
	ld a,b
	or c
	jr nz, waitloop
    
   ;draw shape moving down the screen
    
    ;;; TODO
    ld a, (shape_row_index)
    cp 213                           ; this code will have to change to take into acount highest shape
    jp nz, dropLoop
   ;calculate the position in screen memory, starts off D_FILE + 16 (for first line of game screen)
   ; each row is +10 
   
   
;; user input to retate shape

;; scroll shapes down

;; detect if line(s) has/have been completed and remove, and drop remaining down

;; increase score if line(s) removed

;; detect if game over condition met, it collision between shape entering at top


	jp main   ; never return to basic, new game always starts from title screen


random 
	ld hl,(FRAMES)
random_seed 
	ld de,0
	add hl,de
	dec hl
	ld a,h
	and $03
	ld h,a
	ld (random_seed+1),hl
	ld a,(hl)
foundRandom 
	sub b
	jr nc,foundRandom
	adc a,b
	ret
    
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
