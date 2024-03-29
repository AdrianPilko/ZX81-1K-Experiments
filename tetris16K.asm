;;;;;;;;;;;;;;;;;;;;;
;;; zx81 16K code (gave up for now with getting tetris working in 1K!!)
;;; It's a clone of tetris (in case that wasn't clear from filename;)
;;; The code heavily!! dependant on the definition of the screen memory in screenTetris.asm
;;;;;;;;;;;;;;;;;;;;;

; TODO  / bugs
;   when shape next to edge no logic to prevent rotation, so sticks to wall or worse goes through
;   some shapes can move sideways into others,  incorrectly merging


; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"                ;; removed some of unneeded definitions
#include "line1.asm"

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

#define SHAPE_CHAR_0   128        ; black square
#define SHAPE_CHAR_1 136        ; grey square
#define BOTTOM 22

VSYNCLOOP     EQU      5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp intro_title		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;; the underscore characters here are mapped onto real zx81 
;; characters in charcodes.asm, they are more human readble 
;; shortcuts, than decimal equivalent 
game_over_txt1
	DEFB	_G,_A,_M,_E,$ff    
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
   DEFB %00111100,  %00101011,%10101010,%00011101,%00101101, %00011110   ; should be drawn vertically
   DEFB %11001100,  %00101110,%00001111,%11100100,%01101100, %11000110   ; should be drawn horiz
   DEFB %00111100,  %11010100,%10101010,%10111000,%10110100, %00011110   ; should be drawn vertically
   DEFB %11001100,  %11101000,%00001111,%01001110,%01101100, %11000110   ; should be drawn horiz   

;   DEFB             %00010111,           %00101110,    

screen_area_blank_txt
	DEFB	__,__,__,__,__,__,__,$ff

waitLoopDropFasterFlag
    DEFB 0
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
displayLineIncrement
    DEFB 0,0
displayOuterIncrement    
    DEFB 0,0    
score_mem_hund
	DEFB 0	    
score_mem_tens
	DEFB 0    
deleteShapeFlag
    DEFB 0
speedUp
    DEFB 0
;; intro screen
intro_title    
    ; screenTetris16K.asm has already set everything including the title
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

    ld a, $df
    ld (speedUp),a
    
;; main game loop
main
    ld a, 1
    ld (shape_row),a
    ld a, 5
    ld (shapeTrackLeftRight),a     
    ld a, 13
    ld (shape_row_index),a
    xor a
    ld (flagForBottomHit), a    
    ld (rotationCount), a    

   
tryAnotherR                             ; generate random number to index shape memory
    ld a, r                             ; we want a number 0 to 4 inclusive 
    and %00000111
    cp 6    
    jp nc, tryAnotherR                  ; loop when nc flag set ie not less than 5 try again    
    ld (currentShapeOffset), a

    xor a
    ld (waitLoopDropFasterFlag),a

; read keyboard input, delete old shape move current shape down one
dropLoop                                
    ld a, 1
    ld (deleteShapeFlag),a          ;  drawShape checks this flag to see if is deleting 
    call drawShape
    xor a                           ;  xor a is always zero and saves 1 byte compared to ld a, 0
    ld (deleteShapeFlag),a          ;  clear the flag

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


    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard space to B
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a									; check bit set for key press right (C)
    jr z, dropShapeAllTheWay                    ; causes wait loop to be zero when C pressed
    jp noShapeMove								; dropped through to no move
    
dropShapeAllTheWay    
    ld a, 1
    ld (waitLoopDropFasterFlag), a
    jp noShapeMove								; dropped through to no move

shapeRight

    ;; need to account for rotation when checking if shape can go further to right
    ; and also special case for vertical drawn straight shape to go fully to right

    ld a, (rotationCount)
    and %00000001       ;; work out if rotation count is odd or even    
    jr nz, handleShapeRightForHorizontal             ;; if odd then treat as a horizontal shape

    ;; ok so check if shape is straight, if so gets less width. 
    ;straight shape offsets are 2, 8, 14, 20
    ld a, (currentShapeOffset)    
    cp 2
    jp z, handleShapeRight_StrVert
    ld a, (currentShapeOffset)
    cp 8
    jp z, handleShapeRight_StrVert    
    ld a, (currentShapeOffset)
    cp 14
    jp z, handleShapeRight_StrVert
    ld a, (currentShapeOffset)
    cp 20
    jp z, handleShapeRight_StrVert

    ld a, (shapeTrackLeftRight)
    dec a
    cp 1
    jp z, noShapeMove     
    ld (shapeTrackLeftRight),a 
    ld a, (shape_row_index)
    inc a                  
    ld (shape_row_index), a    
    jp noShapeMove	

handleShapeRight_StrVert
    ld a, (shapeTrackLeftRight)
    dec a
    cp 0
    jp z, noShapeMove     
    ld (shapeTrackLeftRight),a 
    ld a, (shape_row_index)
    inc a                  
    ld (shape_row_index), a    
    jp noShapeMove	
    
handleShapeRightForHorizontal

    ;; ok so check if shape is straight, if so gets less width. 
    ;straight shape offsets are 2, 8, 14, 20
    ld a, (currentShapeOffset)    
    cp 2
    jp z, handleShapeRight_StrHoriz
    ld a, (currentShapeOffset)
    cp 8
    jp z, handleShapeRight_StrHoriz    
    ld a, (currentShapeOffset)
    cp 14
    jp z, handleShapeRight_StrHoriz
    ld a, (currentShapeOffset)
    cp 20
    jp z, handleShapeRight_StrHoriz

    ld a, (shapeTrackLeftRight)
    dec a
    cp 2
    jp z, noShapeMove     
    ld (shapeTrackLeftRight),a 
    ld a, (shape_row_index)
    inc a                  
    ld (shape_row_index), a    
    jp noShapeMove	
    
handleShapeRight_StrHoriz
    ld a, (shapeTrackLeftRight)
    dec a
    cp 3
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
    jp nz, drawShapeHook    
    ld a, (rotationCount)
    inc a
    cp 4
    jp nz, storeIncrementedRotation
    ld a, 0
    ld (rotationCount), a
    ; need to subract 18 from shape offset to get back to original rotation
    ld a, (currentShapeOffset)
    sub 18    
    ld (currentShapeOffset),a
    ;; need to take 2 off shape row when it's rotated, as no longer printing vertically
    ld a, (shape_row)
    sub 2
    ld (shape_row),a

    call drawShape
    jp  preWaitloop

storeIncrementedRotation    
    ld (rotationCount), a      
    ld a, (currentShapeOffset)
    add a, 6    
    ld (currentShapeOffset),a
    call drawShape
    jp  preWaitloop

drawShapeHook    
    call drawShape
preWaitloop	
    ld a, (score_mem_tens)
    cp 153
    jr z, addOneToHund
    jr skipAddHund
addOneToHund
    ld a, 0
    ld (score_mem_tens), a
    ld a, (score_mem_hund)             
    add a, 1
    daa    
    ld (score_mem_hund), a
skipAddHund	

printScoreInGame
    ld bc, 6
    ld de, score_mem_tens
    call printNumber    
      

    ld a, (waitLoopDropFasterFlag)
    cp 0
    jp z,dropNormalSpeed

    ld b, 0      ; set to zero no wait, drop fast 
       
dropNormalSpeed     
    ld b,VSYNCLOOP
waitloop	
waitForTVSync	
	call vsync
	djnz waitForTVSync
    

    ld a,(flagForBottomHit)         ; on current shape draw we detected that if the shape dropped one
                                    ; more line it would hit the something
    cp 1                            ; if flagForBottomHit is set then this will set zero flag
                                    ; so we need to check if rows are complete first
    jp z, checkForCompleteLinesInit

    ld a, (shape_row)
    inc a
    ld (shape_row),a    
    cp BOTTOM                            ; only gets here if no shapes at bottom
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

    ld a, (speedUp)     ;; increase difficulty with each line removed
    dec a
    ld (speedUp),a

    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
    add a,1	
    daa									; z80 daa instruction realigns for BCD after add or subtract  
    ld (score_mem_tens),a				; add one to score, scoring is binary coded decimal (BCD)
    ; move all lives about this down by one
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ld a, (checkColOffsetStartRow)
    ld (copyOfCheckColOffsetStartRow), a

playAreaShuffle
    ld hl, (DF_CC)
    ld bc, (copyOfCheckColOffsetStartRow)    	; checkColOffsetStartRow is an offset from DF_CC, 
                                                ; not address of screen memory
    add hl,bc
    ld (lineRemoved), hl                        ; lineRemoved is now the 16bit copy of address the 
                                                ; start of play area for the line romoved    
    ld a, (copyOfCheckColOffsetStartRow)        ; subtract 10 from checkColOffsetStartRow
    sub 10                                      ; this gets us the offset to the previous line...
    ld (copyOfCheckColOffsetStartRow),a         ; subtract 10 from checkColOffsetStartRow
    ld hl, $00                                  ; have to zero this here otherwise left overs in lineToSuffleFrom top 8bits 
    ld (lineToSuffleFrom), hl
    ld (lineToSuffleFrom) , a                   ; ...the line we're shuffling down from 
    ld bc,(lineToSuffleFrom)
    ld hl, (DF_CC)
    add hl,bc
    ld (lineToSuffleFrom), hl                   ; lineToSuffleFrom is a 16 bit value now the offset 
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
    ld bc,22
    ld de,game_over_txt1    
    call printstring	    
    ld bc,32
    ld de,game_over_txt2
    call printstring	
    ld bc, $ffff
    ld a, 0
    ld (score_mem_tens), a
    ld (score_mem_hund), a	    
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

;check if TV synchro (FRAMES) happend
vsync	
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
	ret


printNumber
    ld hl,(DF_CC)
    add hl,bc	
printNumber_loop
    ld a,(de)
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a      
    ret  

drawShape  
    ld a,(deleteShapeFlag)     
    cp 1
    jp z, dontIncrementShapeRowIndex    ;; if we're deleting shape then skip increment shape_row_index

    ld a, (shape_row_index)
    add a, 10                  ; always need ten as the offset, the left right just adds bit to this   
    ld (shape_row_index), a

dontIncrementShapeRowIndex

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
    and %00000001       ;; work out if rotation count is odd or even    
    jr nz, drawHorizLoopCountSetup
    ld e, 4
    ld a, 2
    ld (innerDrawLoopInit), a
    ld a,10
    ld (displayLineIncrement), a
    sub 2
    ld (displayOuterIncrement),a 
    jr drawShapeOuter
drawHorizLoopCountSetup
    ld e, 2
    ld a, 4         ; drawing horizontally 
    ld (innerDrawLoopInit), a
    ld a,8  
    ld (displayLineIncrement), a
    sub 2
    ld (displayOuterIncrement),a 
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
    ;ld de, (displayLineIncrement)
    ld de, 10
    add hl, de
    ld a, (hl)
    and SHAPE_CHAR_0                      ; this will result in "true" if block exists already in that position    
    ;cp 0                                ; don't need cp 0, as the and sets the flags (saved 2bytes wooo!)
    pop hl
                                        
    jp z, drawTheDamnSquare             ; set a flag to say if move shape one more down will be collision
       
    ld a, (deleteShapeFlag)     ;; if we're deleting the old shape then don't trigger collision
    cp 1
    jp z, drawTheDamnSquare
    ld a, 1    
    ld (flagForBottomHit), a
    
drawTheDamnSquare    
    ld a,(deleteShapeFlag)     ;; if we're deleting the old shape then don't draw anything
    cp 1
    jp z, loadBlank

    ld a, (currentShapeOffset)
    and %00000011
    ;cp 0                       ; don't need cp 0, as the and sets the flags (saved 2bytes wooo!)
    jp z, loadAlternateShape1    
    ld (hl), SHAPE_CHAR_0
    jr drawNothing
    
loadAlternateShape1
    ld (hl), SHAPE_CHAR_1
    jr drawNothing
loadBlank
    ld (hl), 0      ; this clears the block with space
drawNothing
    inc hl
    xor a
    ld a, c    
    rra                                 ; rotate mask to right by one bit
    ld c, a
    djnz drawShapeInner                 ; dnjz decrements b and jumps if not zero
    ld (outerCount), de                 ; store loop count temp
    ld de, (displayOuterIncrement)    
    add hl, de                          ; gets current screen position to next row
    ld de, (outerCount)                 ; retreive  loop count temp
    dec e   
    ld a, e
    cp 0  
    jp nz, drawShapeOuter

    ret    

   
#include "line2.asm"
#include "screenTetris16K.asm"      			; definition of the screen memory, in colapsed version for 1K        
#include "endbasic.asm"
