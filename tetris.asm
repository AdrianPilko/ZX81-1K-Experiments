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

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

#define SHAPE_CHAR   128        ; black square
	jp intro_title		        ; jump to print title text
	
;; initialise "variables" and memory

;; the underscore characters here are mapped onto real zx81 
;; characters in charcodes.asm, they are more human readble 
;; shortcuts, than decimal equivalent 
title_screen_txt
	DEFB	_T,_E,_T,_R,_I,_S,$ff
game_over_txt1
	DEFB	_G,_A,_M,_E,$ff    
game_over_txt2
    DEFB	_O,_V,_E,_R,$ff    
currentShape    
    DEFB 0
shapes      ; base shape stored in upright positions, as they start at top, 2column * 4 rows to make logic easier
            ; e.g. normal L is  10  rev L  01  square 00  T  00  4inrow 01
            ;                   10         01         11     01         01  
            ;                   10         01         11     11         01
            ;                   11         11         00     01         01
; shape definition (bit packed)
   DEFB %00001111,%10101011,%01010111,%00011101,%01010101 

screen_area_blank_txt
	DEFB	__,__,__,__,__,__,__,$ff
    
shape_row_index     ; the current row of the top of the falling shape
    DEFB 0
shape_col_index     ; the current column of the top left of the falling shape
    DEFB 0
outerCount  
    DEFB 0,0
currentShapeOffset    
    DEFB 0
shapeTrackLeftRight
    DEFB 0    
shape_row
    DEFB 0
bottomLevel    
    DEFB 19
initScreenIndex
    DEFB 0,0
        
;; intro screen
intro_title
	; no need for clear screen as screenTetris.asm has already set 
    ; everything to zero    
	ld bc,1                     ; printstring from offset from DF__CC stored in bc
	ld de,title_screen_txt      ; load text into de for printstring
	call printstring	
    
    ; clear the play area (is need for all after first game as play area will be filled with previous blocks
    ld a, 0
    ld (shape_row),a    
    ld a, 11    
    ld (initScreenIndex),a    
initPlayAreaLoop        
    ld bc, (initScreenIndex)
	ld de,screen_area_blank_txt
    call printstring    
    ld a,(initScreenIndex)    
    add a, 10            
    ld (initScreenIndex),a    
    ld a, (shape_row)
    inc a
    ld (shape_row),a    
    cp 22                            ; this code will have to change to take into account collision with shapes and done above
    jp nz, initPlayAreaLoop
    
initialiseVariables
    ld a, 5
    ld (shapeTrackLeftRight),a 
    ld a, 1
    ld (shape_row),a    

    ;; main game loop
main
    ld a, 1
    ld (shape_row),a
    ld a, 5
    ld (shapeTrackLeftRight),a 
    
    ld a, 13
    ld (shape_row_index),a
    ;; generate shape       
    ld a, r
    and %00000011
    ld (currentShapeOffset), a

dropLoop  

deleteOldShape
    ;before we add to shape row index we need to delete the current shape position
    ld a, (currentShapeOffset)
    ld hl, (DF_CC)
    ld de, (shape_row_index)            ; add offset to top of screen memory to skip title    
    ;; this will only draw shape at top need to add current position offset
    add hl, de                          ; to where we want to draw shape
    ld e, 4
deleteOldShapeLoopOuter    
    ld b, 2                             ; b now stores max length of definition of shape (i.e. 1 byte)
deleteOldShapeLoopInner
    ld (hl), 0
    inc hl
    djnz deleteOldShapeLoopInner                 ; dnjz decrements b and jumps if not zero
    ld (outerCount), de                 ; store loop count temp
    ld de, 8    
    add hl, de                          ; gets current screen position to next row
    ld de, (outerCount)                 ; retreive  loop count temp
    dec e   
    ld a, e
    cp 0  
    jp nz, deleteOldShapeLoopOuter
   
    ; read the keyboard input and adust the offset     
	ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard shift to v
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 1, a
	; check bit set for key press left  (Z)
	jp z, shapeRight								; jump to move shape left
	ld a, KEYBOARD_READ_PORT_SPACE_TO_B			; read keyboard space to B
	in a, (KEYBOARD_READ_PORT)					; read from io port		
	bit 2, a									; check bit set for key press right (M)
	jr z, shapeLeft							; jump to move shape right	
	jp noShapeMove								; dropped through to no move
shapeLeft
    ld a, (shapeTrackLeftRight)
    dec a
    cp 1
    jp z, noShapeMove     
    ld (shapeTrackLeftRight),a 
    ld a, (shape_row_index)
    inc a                  
    ld (shape_row_index), a
    
	jp noShapeMove	
shapeRight
    ld a, (shapeTrackLeftRight)
    inc a
    cp 8
    jp z, noShapeMove 
    ld (shapeTrackLeftRight),a 
    
    ld a, (shape_row_index)
    dec a     
    ld (shape_row_index), a 
    jp noShapeMove
    
noShapeMove	
      
    
drawShape
    ld a, (shape_row_index)
    add a, 10                  ; always need ten as the offset, the left right just adds bit to this   
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
    jp z, drawNothing
    ; detect if hl is already drawn on (ie a block already in that location, if so stop
    ;; we also need to draw the shape from the bottom upwards, because we want to detect the collision earlier
    ;; and actually we should to a "trial draw of shape then if no collisions actually draw it!!
    ld a, (hl)
    and SHAPE_CHAR                      ; this will result in "true" if block exists already in that position
    cp 0
    jp nz, checkIfTopHit
    ld (hl), SHAPE_CHAR    

drawNothing
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

preWaitLoop
	ld bc, $09ff
waitloop
	dec bc
	ld a,b
	or c
	jr nz, waitloop
       
    ld a, (shape_row)
    inc a
    ld (shape_row),a    
    cp 19                            ; only gets here if no shapes at bottom
    jp nz, dropLoop

;; user input to rotate shape

;; detect if line(s) has/have been completed and remove, and drop remaining down

;; increase score if line(s) removed

;; detect if game over condition met, it collision between shape entering at top


	jp main   ; never return to basic, new game always starts from title screen

checkIfTopHit       ; check the condition if the top is reached
    ld a, (shape_row)    
    cp 4                ; depends on shape so need multiple compares
    jp z, gameOver    
    cp 3                ; depends on shape so need multiple compares
    jp z, gameOver
    cp 2                ; depends on shape so need multiple compares
    jp z, gameOver
    cp 1                ; depends on shape so need multiple compares
    jp main

gameOver
    ld bc,22
    ld de,game_over_txt1    
    call printstring	    
    ld bc,32
    ld de,game_over_txt2
    call printstring	
  	ld bc, $ffff
waitloopRetryGame
	dec bc
	ld a,b
	or c
	jr nz, waitloopRetryGame  
    jp intro_title
    
    
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
