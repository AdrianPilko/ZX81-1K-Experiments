;;;;;;;;;;;;;;;;;;;;;
;;; zx81 1K code 
;;; It's a clone of tetris (in case that wasn't clear from filename;)
;;; Adrian Pilkington (youtube: ByteForever) 2024
;;; reworked tetris16K.asm in an attempt to actually fit in 1K !!
;;;
;;; Using "model" minimal machine code skeleton, from Dr.Beep's 
;;; book: The Ulitmate 1K ZX81 coding book
;;;;;;;;;;;;;;;;;;;;;
; TODO  / bugs
;   when shape next to edge no logic to prevent rotation, so sticks to wall or worse goes through
;   some shapes can move sideways into others,  incorrectly merging

;;;;;;;;;;;;;;;;;;
;; current assembled size 1145 bytes!!!
;;;;;;;;;;;;;;;;;;

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
         jp intro_title  ; main code entry point

cdflag   db 64

; DO NOT CHANGE SYSVAR ABOVE!

; keyboard port for shift key to v
KEYBOARD_READ_PORT_SHIFT_TO_V equ $FE

; keyboard space to b
KEYBOARD_READ_PORT_SPACE_TO_B equ $7F 

; starting port numbner for keyboard, is same as first port for shift to v
KEYBOARD_READ_PORT equ $FE 

SHAPE_CHAR_0  equ    128        ; black square
SHAPE_CHAR_1  equ    136        ; grey square
BOTTOM        equ    22         ; bottom row number
VSYNCLOOP     equ    6         ; used for frame delay loop
DF_CC         equ    dfile+1
    
;; intro screen
intro_title            
    ;; this is to zero all the memory containing the game "variables" we do it from end to start using lddr
    ;; if any varaibles are added or should be initialised to anything but zero then that needs to be
    ;; handled.
    ld hl, zero     ; zero is initialised to zero and never changed in the code!
    ld bc, 30       ; we have 32 bytes of memory that needs zero'ing from...
    ld de, deleteShapeFlag  ; ...deleteShapeFlag down to waitLoopDropFasterFlag
    lddr
  
    ld b, BOTTOM
    ld a, 11    
initPlayAreaLoop        
      push bc
         ld b, 0
         ld c, a
         push af
            ld de,screen_area_blank_txt
            call printstring
         pop af
         add a, 10            
      pop bc
    djnz initPlayAreaLoop
    
;; main game loop
main
    xor a     ; set a to zero
    ld (flagForBottomHit), a    
    ld (rotationCount), a
    inc a     ; a is now 1
    ld (shape_row),a
    ld a, 5
    ld (shapeTrackLeftRight),a     
    ld a, 13
    ld (shape_row_index),a    
   
tryAnotherR                             ; generate random number to index shape memory
    ld a, r                             ; we want a number 0 to 4 inclusive 
    and %00000111
    cp 6    
    jr nc, tryAnotherR                  ; loop when nc flag set ie not less than 5 try again    
    ld (currentShapeOffset), a

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

    jp noShapeMove								; dropped through to no move
    
shapeRight

    ;; need to account for rotation when checking if shape can go further to right
    ;; and also special case for vertical drawn straight shape to go fully to right

    ld a, (rotationCount)
    and %00000001       ;; work out if rotation count is odd or even    
    jr nz, handleShapeRightForHorizontal             ;; if odd then treat as a horizontal shape

    ;; ok so check if shape is straight, if so gets less width. 
    ;straight shape offsets are 2, 8, 14, 20
    ld a, (currentShapeOffset)    
    cp 2
    jr z, handleShapeRight_StrVert
    cp 8
    jr z, handleShapeRight_StrVert    
    cp 14
    jr z, handleShapeRight_StrVert
    cp 20
    jr z, handleShapeRight_StrVert

    ld a, (shapeTrackLeftRight)
    dec a
    cp 1
    jp z, noShapeMove      
    jr incShapeRowThenNoShape

handleShapeRight_StrVert
    ld a, (shapeTrackLeftRight)
    dec a
    cp 0
    jp z, noShapeMove      
    jr incShapeRowThenNoShape
    
handleShapeRightForHorizontal

    ;; ok so check if shape is straight, if so gets less width. 
    ;straight shape offsets are 2, 8, 14, 20
    ld a, (currentShapeOffset)    
    cp 2
    jp z, handleShapeRight_StrHoriz
    cp 8
    jp z, handleShapeRight_StrHoriz
    cp 14
    jp z, handleShapeRight_StrHoriz
    cp 20
    jr z, handleShapeRight_StrHoriz

    ld a, (shapeTrackLeftRight)
    dec a
    cp 2
    jp z, noShapeMove     
    jr incShapeRowThenNoShape	
    
handleShapeRight_StrHoriz
    ld a, (shapeTrackLeftRight)
    dec a
    cp 3
    jp z, noShapeMove     
    jr incShapeRowThenNoShape
shapeLeft
    ld a, (shapeTrackLeftRight)
    inc a
    cp 8
    jp z, noShapeMove 
    ld (shapeTrackLeftRight),a 

    ld a, (shape_row_index)
    dec a     
    ld (shape_row_index), a 
    jr noShapeMove

incShapeRowThenNoShape
    ld (shapeTrackLeftRight),a 
    ld a, (shape_row_index)
    inc a                  
    ld (shape_row_index), a   
    
noShapeMove	

    ;;; read the rotate shape after the left right is done,
    ;; we draw the shape then next time the delete shape code runs will delete rotated
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard shift to v
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 2, a
    ; check bit set for key press rotate  use X key 
    jr nz, drawShapeHook    
    ld a, (rotationCount)
    inc a
    cp 4
    jr nz, storeIncrementedRotation
    xor a
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
    jr  preWaitloop

storeIncrementedRotation    
    ld (rotationCount), a      
    ld a, (currentShapeOffset)
    add a, 6    
    ld (currentShapeOffset),a
    call drawShape
    jr  preWaitloop

drawShapeHook    
    call drawShape
preWaitloop	
    ld a, (score_mem_tens)
    cp 153
    jr z, addOneToHund
    jr skipAddHund
addOneToHund
    xor a
    ld (score_mem_tens), a
    ld a, (score_mem_hund)             
    inc a
    daa    
    ld (score_mem_hund), a
skipAddHund	

printScoreInGame
    ld bc, 6
    ld de, score_mem_tens
    call printNumber    
      
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
    ld hl,DF_CC
    ld a, (checkColOffsetStartRow)
    add a, 10
    ld (checkColOffsetStartRow), a    
    ld bc, (checkColOffsetStartRow)    
    add hl,bc
    xor a
    ld (checkColIndex), a     
checkLine        
    ld a, (hl)
    and SHAPE_CHAR_0  
    inc hl
    cp SHAPE_CHAR_0
    jr nz, setlineNOTComplete
afterSetlineNOTComplete
    
    ld a, (checkColIndex)
    inc a
    ld (checkColIndex), a
    cp 7
    jr nz, checkLine                ; always complete check loop fully

    ld a, (lineCompleteFlag)
    cp 1
    jr z,removelineIsComplete

    jp checkCompleteLoopInc
    
setlineNOTComplete
    xor a
    ld (lineCompleteFlag),a
    jp afterSetlineNOTComplete   
    
removelineIsComplete          
    push hl ; preserve for after printstring
    push de    
    push bc

    ;;ld a, (speedUp)     ;; increase difficulty with each line removed
    ;;dec a
    ;;ld (speedUp),a

    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
    inc a	
    daa									; z80 daa instruction realigns for BCD after add or subtract  
    ld (score_mem_tens),a				; add one to score, scoring is binary coded decimal (BCD)
    ; move all lives about this down by one
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ld a, (checkColOffsetStartRow)
    ld (copyOfCheckColOffsetStartRow), a

playAreaShuffle
    ld hl, DF_CC
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
    ld hl, DF_CC
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
    jr nz, loopFor_7_Shuffle     

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
waitloopRetryGame
    dec bc
    ld a,b
    or c
    jr nz, waitloopRetryGame  
    jp intro_title
   
; this prints at top any offset (stored in bc) from the top of the screen D_FILE
printstring
    ld hl,DF_CC
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
	ld a,(frames)
	ld c,a
sync
	ld a,(frames)
	cp c
	jr z,sync
	ret


printNumber
    ld hl,DF_CC
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
    ld hl, DF_CC
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
     
; the playing area is a shrunk down ZX81 display. 
; in addition to the play area we have "out of memory" embeded
; which, if it crashes on startup we know it's run out, otherwise 
; that will be overwritten byt he game if alls ok
dfile
    db 118,"S"-27,"C"-27,"O"-27,"R"-27,"E"-27,3,3,3,132  ; 0, 136 first chr$118 marks the start of DFILE     
    db 118,5,136,136,136,136,136,136,136,133  ; 1, play area offset from DF_CC 12 to 18 
    db 118,5,136,136,136,136,136,136,136,133  ; 2, "" 22 to 28  
    db 118,5,136,136,136,136,136,136,136,133  ; 3  "" 32 to 38
    db 118,5,136,136,136,136,136,136,136,133  ; 4     42 to 48  etc
    db 118,5,136, "O"-27, "U"-27, "T"-27,136, "O"-27, "F"-27,133  ; 5  
    db 118,5, "M"-27, "E"-27, "M"-27, "O"-27, "R"-27, "Y"-27,136,133  ; 6  
    db 118,5,136,136,136,136,136,136,136,133  ; 7    ;; all the 136 in here get overriten with the 
    db 118,5,136,136,136,136,136,136,136,133  ; 8    ;; blank space "0" character, but this was to test 
    db 118,5,136,136,136,136,136,136,136,133  ; 9    ;; that the screen has blanked fully
    db 118,5,136,136,136,136,136,136,136,133  ; 10   ;; 
    db 118,5,136,136,136,136,136,136,136,133  ; 11   ;; if the memory is full then thre game will crash
    db 118,5,136,136,136,136,136,136,136,133  ; 12   ;; beforte it gets to clear play area  hence the 
    db 118,5,136,136,136,136,136,136,136,133  ; 13   ;; message no-one should see "OUT OF MEMORY"
    db 118,5,136,136,136,136,136,136,136,133  ; 14   ;; if the game fits in 1K
    db 118,5,136,136,136,136,136,136,136,133  ; 15
    db 118,5,136,136,136,136,136,136,136,133  ; 16
    db 118,5,136,136,136,136,136,136,136,133  ; 17
    db 118,5,136,136,136,136,136,136,136,133  ; 18
    db 118,5,136,136,136,136,136,136,136,133  ; 19
    db 118,5,136,136,136,136,136,136,136,133  ; 20
    db 118,5,136,136,136,136,136,136,136,133  ; 21
    db 118,5,136,136,136,136,136,136,136,133  ; 22
    db 118,130,131,131,131,131,131,131,131,129; 23
    db 118                                    ; 24
vars
    db 128          ; becomes end of screen

game_over_txt1
	db "G"-27,"A"-27,"M"-27,"E"-27,$ff    
game_over_txt2
    db "O"-27,"V"-27,"E"-27,"R"-27,$ff        
currentShape    
    db 0
shapes      ; Shapes are known as Tetromino (see wikipedia), use 8 bits per shape
            ; base shape 2 column * 4 rows to make logic easier, interpreted as such in the code and definition 
            ;  "square"   "L"    "straight"   "T"  "skew left" "skew right"
            ;       00     00       10        00     00          00      
            ;       00     10       10        01     10          01      
            ;       11     10       10        11     11          11      
            ;       11     11       10        01     01          10      

; shape definition (bit packed)
;        square       L R/L   straight     T L/R   skew L   skew R
   db %00111100,  %00101011,%10101010,%00011101,%00101101, %00011110   ; should be drawn vertically
   db %11001100,  %00101110,%00001111,%11100100,%01101100, %11000110   ; should be drawn horiz
   db %00111100,  %11010100,%10101010,%10111000,%10110100, %00011110   ; should be drawn vertically
   db %11001100,  %11101000,%00001111,%01001110,%01101100, %11000110   ; should be drawn horiz   

screen_area_blank_txt
    db 0,0,0,0,0,0,0,$ff

waitLoopDropFasterFlag
    db 0
shape_row_index     ; the current row of the top of the falling shape
    db 0
shape_col_index     ; the current column of the top left of the falling shape
    db 0
outerCount  
    dw 0
currentShapeOffset    
    dw 0
shapeTrackLeftRight
    dw 0    
shape_row
    db 0
initScreenIndex 
    db 0
flagForBottomHit
    db 0
checkColOffsetStartRow
    dw 0
checkRowIndex
    db 0
checkColIndex
    db 0        
lineCompleteFlag
    db 0
lineRemoved
    dw 0
lineToSuffleFrom
    dw 0
copyOfCheckColOffsetStartRow
    dw 0
rotationCount           ; zero means not rotated!    
    db 0
innerDrawLoopInit
    db 0
displayLineIncrement
    dw 0
displayOuterIncrement    
    dw 0    
score_mem_hund
    db 0	    
score_mem_tens
    db 0    
deleteShapeFlag
    db 0
;speedUp
;    db 0
zero
    db 0  
last     equ $
end

