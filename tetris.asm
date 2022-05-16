;;;;;;;;;;;;;;;;;;;;;
;;; zx81 1K code
;;; This is minimal code that will run on the limited memory of standard unexpanded 1K zx81
;;; It's a clone of tetris (in case that wasn't clear from filename;)
;;; The code heavily!! dependant on the definition of the screen memory in screenTetris.asm
;;;;;;;;;;;;;;;;;;;;;
;;; as of 16/5/2022 size of assembled p file is 934bytes (has to be < 950 ish)

; TODO  
;   scoring
;   side to side collision detect
;   rotate shapes (IS SUPER HARD!)
;   speed up game each time line removed
;   the delete old shape needs to use the proper shape definition, not overwrite 8 blocks
;   straight shape doesn't go all way to left, due to wayit's defined and drawn

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sysTetris.asm"                ;; removed some of unneeded definitions
#include "line1.asm"

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

#define SHAPE_CHAR_0   128        ; black square
#define SHAPE_CHAR_1 136        ; grey square
#define BOTTOM 20

	jp intro_title		        ; jump to print title text
	
;; the underscore characters here are mapped onto real zx81 
;; characters in charcodes.asm, they are more human readble 
;; shortcuts, than decimal equivalent 
;game_over_txt1
;	DEFB	_G,_A,_M,_E,$ff    
game_over_txt2
    DEFB	_O,_V,_E,_R,$ff        
currentShape    
    DEFB 0
shapes      ; Shapes are known as Tetromino (see wikipedia), use 8 bits per shape
            ; base shape 2 column * 4 rows to make logic easier, interpreted as such in the code and definition 
            ;  "square"   "L"    "straight"   "T"  "skew left" "skew right"
            ;       00     00       10        00     00          00 
            ;       00     10       10        01     10          01     
            ;       11     10       10        11     11          11     
            ;       11     11       10        01     01          10     
; shape definition (bit packed)
;        square       L R/L   straight     T L/R   skew L   skew R
   DEFB %00001111,  %00101011,%10101010,%00011101,%00101101, %00011110   
   DEFB             %00010111,           %00101110,    

screen_area_blank_txt
	DEFB	__,__,__,__,__,__,__,$ff
    
shape_row_index     ; the current row of the top of the falling shape
    DEFB 0
shape_col_index     ; the current column of the top left of the falling shape
    DEFB 0
outerCount  
    DEFB 0,0
currentShapeOffset    
    DEFB 0,0
shapeTrackLeftRight
    DEFB 0,0    
shape_row
    DEFB 0
initScreenIndex 
    DEFB 0,0
flagForBottomHit
    DEFB 0
checkColOffsetStartRow
    DEFB 0,0
checkRowIndex
    DEFB 0
checkColIndex
    DEFB 0        
lineCompleteFlag
    DEFB 0
lineRemoved
    DEFB 0,0
lineToSuffleFrom
    DEFB 0,0
copyOfCheckColOffsetStartRow
    DEFB 0,0
rotationCount           ; zero means not rotated!    
    DEFB 0
innerDrawLoopInit
    DEFB 0
;; intro screen
intro_title    
	; screenTetris.asm has already set everything including the title
    ; clear the play area (is need for all after first game as play area will be filled with previous blocks
    ld b, BOTTOM
    ld a, 11    
    ld (initScreenIndex),a    
initPlayAreaLoop        
    push bc
    ld bc, (initScreenIndex)
	ld de,screen_area_blank_txt
    call printstring    
    ld a,(initScreenIndex)    
    add a, 10            
    ld (initScreenIndex),a    
    pop bc
    djnz initPlayAreaLoop
    
;; main game loop
main
    ld a, 0
    ld (flagForBottomHit), a
    ld a, 1
    ld (shape_row),a
    ld a, 5
    ld (shapeTrackLeftRight),a     
    ld a, 13
    ld (shape_row_index),a
    ld a, 0
    ld (rotationCount), a           

tryAnotherR                             ; generate random number to index shape memory
    ld a, r                             ; we want a number 0 to 4 inclusive 
    and %00001111
    cp 8
    jp nc, tryAnotherR                  ; loop when nc flag set ie not less than 5 try again    
    ld (currentShapeOffset), a

dropLoop                                ; delete old shape move current shape down one

  
deleteOldShape
    ;before we add to shape row index we need to delete the current shape position
    ld hl, (DF_CC)
    ld de, (shape_row_index)            ; add offset to top of screen memory to skip title    
    ;; this will only draw shape at top need to add current position offset
    add hl, de                          ; to where we want to draw shape
    
    
 ;; alter loop counts when rotating so draw horizontal or vertical, not just vertical                                    l
    ld a, (rotationCount)
    cp 1
    jr z, drawDeleteHorizLoopCountSetup
    ld e, 4
    ld a, 2
    ld (innerDrawLoopInit), a
    jr deleteOldShapeLoopOuter
drawDeleteHorizLoopCountSetup
    ld e, 2
    ld a, 4
    ld (innerDrawLoopInit), a
    
deleteOldShapeLoopOuter        
    ld a, (innerDrawLoopInit)         
    ld b, a             ; directly loading into b from memory fails?? MS byte not used error??    
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
	jp z, shapeLeft								; jump to move shape left
	ld a, KEYBOARD_READ_PORT_SPACE_TO_B			; read keyboard space to B
	in a, (KEYBOARD_READ_PORT)					; read from io port		
	bit 2, a									; check bit set for key press right (M)
	jr z, shapeRight							; jump to move shape right	
	jp noShapeMove								; dropped through to no move
shapeRight
    ld a, (shapeTrackLeftRight)
    dec a
    cp 1
    jp z, noShapeMove     
    ld (shapeTrackLeftRight),a 
    ld a, (shape_row_index)
    inc a                  
    ld (shape_row_index), a    
	jp noShapeMove	
shapeLeft
    ld a, (shapeTrackLeftRight)
    inc a
    cp 8
    jp z, noShapeMove 
    ld (shapeTrackLeftRight),a 
    
    ld a, (shape_row_index)
    dec a     
    ld (shape_row_index), a 
   
noShapeMove	

    ;;; read the rotate shape after the left right is done,
    ;; we draw the shape then next time the delete shape code runs will delete rotated
	ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard shift to v
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 2, a
	; check bit set for key press rotate  use X key 
	jp nz, drawShape								
    ld a, 1
    ld (rotationCount), a      
    
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
    
    ;; alter loop counts when rotating so draw horizontal or vertical, not just vertical                                    l
    ld a, (rotationCount)
    cp 1
    jr z, drawHorizLoopCountSetup
    ld e, 4
    ld a, 2
    ld (innerDrawLoopInit), a
    jr drawShapeOuter
drawHorizLoopCountSetup
    ld e, 2
    ld a, 4
    ld (innerDrawLoopInit), a
    
drawShapeOuter    
    ld a, (innerDrawLoopInit)         
    ld b, a             ; directly loading into b from memory fails?? MS byte not used error??
drawShapeInner
    ld a, (currentShape)    
    and c                               ; set to block or no block based on (shapes)     
    jp z, drawNothing
    ; detect if hl is already drawn on (ie a block already in that location, if so stop
    ;; we also need to draw the shape from the bottom upwards, because we want to detect the collision earlier
    ;; and actually we should to a "trial draw of shape then if no collisions actually draw it!!
    
    push hl
    ld de, 10   
    add hl, de
    ld a, (hl)
    and SHAPE_CHAR_0                      ; this will result in "true" if block exists already in that position    
    cp 0                                
    pop hl
                                        
    jp z, drawTheDamnSquare             ; set a flag to say if move shape one more down will be collision
    ld a, 1    
    ld (flagForBottomHit), a

drawTheDamnSquare    
    ld a, (currentShapeOffset)
    and %00000011
    cp 0
    jp z, loadAlternateShape1
    ld (hl), SHAPE_CHAR_0
    jr drawNothing
loadAlternateShape1
    ld (hl), SHAPE_CHAR_1
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
	ld bc, $0fff
waitloop
    dec bc
    ld a,b
    or c
    jr nz, waitloop
    ld a,(flagForBottomHit)         ; on current shape draw we detected that if the shape dropped one
                                    ; more line it would hit the something
    cp 1                            ; if flagForBottomHit is set then this will set zero flag
                                    ; so we need to check if rows are complete first
    jp z, checkForCompleteLinesInit
  
    ld a, (shape_row)
    inc a
    ld (shape_row),a    
    cp BOTTOM-2                            ; only gets here if no shapes at bottom
    jp nz, dropLoop
    jp main                   
    
    
       
checkForCompleteLinesInit   
    ld a, 11                        ; offset to first block in screen play area 
    ld (checkColOffsetStartRow), a    
    ld a, 1
    ld (checkRowIndex), a               
checkLoopSetup
    ld a, 1
    ld (lineCompleteFlag),a
    ld hl,(DF_CC)
    ld a, (checkColOffsetStartRow)
    add a, 10
    ld (checkColOffsetStartRow), a    
    ld bc, (checkColOffsetStartRow)    
    add hl,bc
    ld a, 0
    ld (checkColIndex), a     
checkLine        
    ld a, (hl)
    and SHAPE_CHAR_0  
    inc hl
    cp SHAPE_CHAR_0
    jp nz, setlineNOTComplete
afterSetlineNOTComplete
    
    ld a, (checkColIndex)
    inc a
    ld (checkColIndex), a
    cp 7
    jp nz, checkLine                ; always complete check loop fully
    
    ld a, (lineCompleteFlag)
    cp 1
	jp z,removelineIsComplete
    
    jp checkCompleteLoopInc
    
setlineNOTComplete
    ld a, 0
    ld (lineCompleteFlag),a
    jp afterSetlineNOTComplete   
    
removelineIsComplete          
    push hl ; preserve for after printstring
    push de    
    push bc
  
    ; move all lives about this down by one
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ld a, (checkColOffsetStartRow)
    ld (copyOfCheckColOffsetStartRow), a

playAreaShuffle
    ld hl, (DF_CC)
    ld bc, (copyOfCheckColOffsetStartRow)    	; checkColOffsetStartRow is an offset from DF_CC, 
                                        ; not address of screen memory
	add hl,bc
    ld (lineRemoved), hl                ; lineRemoved is now the 16bit copy of address the 
                                        ; start of play area for the line romoved    
    ld a, (copyOfCheckColOffsetStartRow)      ; subtract 10 from checkColOffsetStartRow
    sub 10                              ; this gets us the offset to the previous line...
    ld (copyOfCheckColOffsetStartRow),a      ; subtract 10 from checkColOffsetStartRow
    ld hl, $00                          ; have to zero this here otherwise left overs in lineToSuffleFrom top 8bits 
    ld (lineToSuffleFrom), hl
    ld (lineToSuffleFrom) , a           ; ...the line we're shuffling down from 
    ld bc,(lineToSuffleFrom)
    ld hl, (DF_CC)
	add hl,bc
    ld (lineToSuffleFrom), hl           ; lineToSuffleFrom is a 16 bit value now the offset 
                                        ; from start of  screen memory
    ld hl, (lineRemoved)            
    ld bc, (lineToSuffleFrom)
    ld e, 0
loopFor_7_Shuffle           
    ld a,(bc)        
    ld (hl), a   ;; this instruction crashes!!       ; load screen position at hl with a    
    inc hl                              ; move position in screen memory we're writing to on one 
    inc bc                              ; move the position we're moving from on one
                                        ; loop count down to zero                                        
    inc e
    ld a, e
    cp 7
    jp nz, loopFor_7_Shuffle     

    ; need to loop until reached top with copy of checkColOffsetStartRow    
    ld a,(copyOfCheckColOffsetStartRow)  
    cp 21
    jp nz, playAreaShuffle
   
    pop bc
    pop de         
    pop hl
 
checkCompleteLoopInc
    ld a, (checkRowIndex)
    inc a
    ld (checkRowIndex), a
    cp BOTTOM
    jp nz, checkLoopSetup

checkIfTopWillBeHit                     ; call if bottom was hit and if this means no space at top
                                        ; check the if the top is reached then game over
    ld a, (shape_row)    
    cp 2                                ; depends on shape so need multiple compares
    jp z, gameOver
    cp 1                                ; depends on shape so need multiple compares
    jp z, gameOver
    jp main

    
gameOver
    ;ld bc,22
    ;ld de,game_over_txt1    
    ;call printstring	    
    ;ld bc,32
    ;ld de,game_over_txt2
    ;call printstring	
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
