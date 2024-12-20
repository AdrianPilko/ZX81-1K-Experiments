;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; p_file_size 722 bytes (but screen expands to ((22*11)+20)=262, so p_file_size+262)
;; size to load has to be < 949, which it is, but max memory is 1024 bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tetris clone aiming to fit in 1K for the ZX81
;;;
;;; keys: o left, p right, q rotate clockwise w to rotate block anticlockwise
;;;
;;; Adrian Pilkington (youtube: ByteForever) 2024
;;; reworked tetris16K.asm in an attempt to actually fit in 1K !!
;;;
;;; Using "model" minimal machine code skeleton, from Dr.Beep's 
;;; book: The Ulitmate 1K ZX81 coding book
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO  / bugs

;   1) when shape next to edge no logic to prevent rotation, so sticks to wall or worse goes through
;   2) some shapes can move sideways into others,  incorrectly merging
;   3) it would currently break fiting in 1K, but the play area should
;      in "proper" tetris be 10 blocks wide, not 7
;   4) in real tetris you get bonus for multiline completion - I think 4 lines is called "a tetris"
;   5) if you press rotate keys (q or w) near bottom it can sometimes cause a restart of the game
;      I thought this was due to the new rotate anticlockwise but was not it was already a bug
;
;
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
SHAPE_CHAR_0  equ    128        ; black square
BLANK_SQUARE  equ    0          ; empty 
BOTTOM        equ    22         ; bottom row number
LINE_LENGTH   equ    10         
VSYNCLOOP     equ    6         ; used for frame delay loop
DF_CC         equ    dfile+1
    
;; game main start point
intro_title            
    ;; this is to zero all the memory containing the game "variables" we do it from end to start using lddr
    ;; if any varaibles are added or should be initialised to anything but zero then that needs to be
    ;; handled.
    ld hl, zero     ; zero is initialised to zero and never changed in the code!
    ld bc, 23       ; we have 23 bytes of memory that needs zero'ing from...
    ld de, deleteShapeFlag  ; ...deleteShapeFlag down to waitLoopDropFasterFlag
    lddr

    call fillPlayerArea

    xor a
    ld hl, score  ;; this is position of score left digit on screen
setScoreToZero   
    ld (hl),28  ; value of "0" is 28 
    inc hl
    cp (hl)
    jr nz, setScoreToZero

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
    call drawShapeNoIncRow

    call getKey  ; stores value of key in a
    cp 26                  ; O key for left
    jr z, shapeLeft 
    cp 25                  ; P 
    jr z, shapeRight 
    jr noShapeMove								; dropped through to no move
    
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
    jr z, noShapeMove      
    jr incShapeRowThenNoShape

handleShapeRight_StrVert
    ld a, (shapeTrackLeftRight)
    dec a
    ;;cp 0   no need for cp 0 as zsero flag set by dec a
    jr z, noShapeMove      
    jr incShapeRowThenNoShape
    
handleShapeRightForHorizontal

    ;; ok so check if shape is straight, if so gets less width. 
    ;straight shape offsets are 2, 8, 14, 20
    ld a, (currentShapeOffset)    
;    cp 2
;    jp z, handleShapeRight_StrHoriz
;    cp 8
;    jp z, handleShapeRight_StrHoriz
;    cp 14
;    jp z, handleShapeRight_StrHoriz
;    cp 20
;    jr z, handleShapeRight_StrHoriz

    ld a, (shapeTrackLeftRight)
    dec a
    cp 2
    jr z, noShapeMove     
    jr incShapeRowThenNoShape	
    
;handleShapeRight_StrHoriz
;    ld a, (shapeTrackLeftRight)
;    dec a
;    cp 3
;    jp z, noShapeMove     
;    jr incShapeRowThenNoShape
shapeLeft
    ld a, (shapeTrackLeftRight)
    inc a
    cp 8
    jr z, noShapeMove 
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
    call getKey
    cp 10                  ; Q key to rotate clockwise
    ld b, 1
    jr z, doRotationLeft
    cp 11                   ; W key to rotate anticlockwise
    ld b, 3                 ; causes rotate clockwise by 3, which is equivallent to rotate anticlockwise 
    jr z, doRotationLeft
    jr justDrawShape        ; no rotate key pressed               

                                        ; partial trace table
doRotationLeft                          ; rotationCount  currentShapeOffset chosen at random once per drop 
    ld a, (rotationCount)               ;      0                 0
    inc a                               ;      1                 6
    cp 4                                ;      2                 12
    jr z, doStuffIfRotMax               ;      3                 18
                                        ;      4->xor a-> 0      sub 18 -> 0                 
    ld (rotationCount), a               ;;;;;;;;;;;;;;      
    ld a, (currentShapeOffset)          ;      0                 1
    add a, 6                            ;      1                 7
    ld (currentShapeOffset),a           ;      2                 13
    jr skippedRotMax                    ;      3                 19
doStuffIfRotMax                         ;      4->xor a->0       sub 18 -> 1            
    xor a
    ld (rotationCount), a
    ; need to subract 18 from shape offset to get back to original rotation
    ld a, (currentShapeOffset)
    sub 18    
    ld (currentShapeOffset),a
skippedRotMax    
    ;; need to take 2 off shape row when it's rotated, as no longer printing vertically
    ld a, (shape_row)
    sub 2
    ld (shape_row),a

    djnz doRotationLeft

justDrawShape        
    call drawShape

preWaitloop	          
    call waitForTVSync	

    ld a,(flagForBottomHit)         ; on current shape draw we detected that if the shape dropped one
                                    ; more line it would hit the something
    cp 1                            ; if flagForBottomHit is set then this will set zero flag
                                    ; so we need to check if rows are complete first
    jr z, checkForCompleteLinesInit

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
    ld hl,DF_CC
    ld a, (checkColOffsetStartRow)
    add a, 10
    ld (checkColOffsetStartRow), a    
    ld bc, (checkColOffsetStartRow)    
    add hl,bc
    ld b, 7     
checkLine        
    ld a, (hl)
    inc hl
    cp SHAPE_CHAR_0
    jr nz, checkCompleteLoopInc
    djnz checkLine
    ;; if we've checked the whole line and not found a gap then line complete

add1ToScore   ; we use the screen memory to store the score to save bytes
    ld hl, dfile+5 ; one position behind score   
    db 17
incTens
    ld (hl), 28
    dec hl   ; move to next digit
    inc (hl) ; add one point
    ld a, (hl)
    cp 38      ; check if greater than "9" 
    jr z, incTens
       
    ; move all below this down by one
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ld a, (checkColOffsetStartRow)
    ld (copyOfCheckColOffsetStartRow), a

playAreaShuffle
    ld hl, DF_CC
    ld bc, (copyOfCheckColOffsetStartRow)    	; checkColOffsetStartRow is an offset from DF_CC, 
                                                ; not address of screen memory
    add hl,bc
    ld d, h   ; store the line to remove in de
    ld e, l   ; have to copy the registers individually due to no z80 ld de, hl
                                                ; start of play area for the line romoved    
    ld a, (copyOfCheckColOffsetStartRow)        ; subtract 10 from checkColOffsetStartRow
    sub 10                                      ; this gets us the offset to the previous line...
    ld (copyOfCheckColOffsetStartRow),a         ; subtract 10 from checkColOffsetStartRow
    ld hl, lineToSuffleFrom+1                    ; have to zero this here otherwise left overs in lineToSuffleFrom top 8bits 
    ld (hl), 0
    ld (lineToSuffleFrom) , a                   ; ...the line we're shuffling down from 
    ld bc,(lineToSuffleFrom)
    ld hl, DF_CC
    add hl,bc
    ;; copy one line to other to shuffle down
    ;; de already contains the line that's to be removed
    ;; hl already contains the line to shuffle from
    ld bc, 7
    ldir      
    
    ; need to loop until reached top with copy of checkColOffsetStartRow    
    ld a,(copyOfCheckColOffsetStartRow)  
    cp BOTTOM-1
    jr nz, playAreaShuffle
 
checkCompleteLoopInc          ;; check if full play area finished
    ld a, (checkRowIndex)
    inc a
    ld (checkRowIndex), a
    cp BOTTOM
    jr nz, checkLoopSetup

checkIfTopWillBeHit                     ; call if bottom was hit and if this means no space at top
                                        ; check the if the top is reached then game over
    ld a, (shape_row)    
    cp 2                                ; depends on shape so need multiple compares
    jr z, gameOver
    cp 1                                ; depends on shape so need multiple compares
    jp nz, main 
    
gameOver
    ;;; set high score using dr.beeps method
    ld hl, score-1
    ld de, highScore-1
    ld bc, 5
scoreSame
    dec c
    inc de
    inc hl
    ld a,(de)
    cp (hl)
    jr z, scoreSame
    call c,$a6e

    ld b, 10
endGameFlashArea
    push bc 
      call waitForTVSync
    pop bc
    djnz endGameFlashArea	  
    jp intro_title


drawShape  
    ld a, (shape_row_index)
    add a, 10                  ; always need ten as the offset, the left right just adds bit to this   
    ld (shape_row_index), a

drawShapeNoIncRow

    ld a, (currentShapeOffset)
    ld hl, shapes
    ld d, 0                            
    ld e, a                            ; add the random (0 to 3) offset to hl to get value of shape
    add hl, de
    ld a, (hl)    
    ld (currentShape), a
    ; setup to draw shape at shape_row_index from the start of screen memory    
    ld hl, DF_CC
    ld de, (shape_row_index)            ; add offset to top of screen memory to skip title    
    add hl, de                          ; to where we want to draw shape
    ld c, %10000000                     ; mask for shape (initialised, but will be rotated  )

    ;; alter loop counts when rotating so draw horizontal or vertical, not just vertical                                    l
    ld a, (rotationCount)
    and %00000001       ;; work out if rotation count is odd or even    
    jr nz, drawHorizLoopCountSetup
    ld a, 2
    ld (innerDrawLoopInit), a
    ld a,10
    ld (displayLineIncrement), a
    sub 2
    ld (displayOuterIncrement),a 
    jr drawShapeOuter
drawHorizLoopCountSetup
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
    ;; detect if hl is already drawn on (ie a block already in that location, if so stop
    ;; we also need to draw the shape from the bottom upwards, because we want to detect the collision earlier
    ;; and actually we should to a "trial draw of shape then if no collisions actually draw it!!

    push hl
       ld de, 10
       add hl, de
       ld a, (hl)
       and SHAPE_CHAR_0                      ; this will result in "true" if block exists already in that position    
    pop hl
                                        
    jp z, drawTheDamnSquare             ; set a flag to say if move shape one more down will be collision
       
    ld a, (deleteShapeFlag)     ;; if we're deleting the old shape then don't trigger collision
    cp 1
    jp z, drawTheDamnSquare
    ld a, 1
    ld (flagForBottomHit), a
    
drawTheDamnSquare    
    ld (hl), 0      ; this clears the block with space
    ld a,(deleteShapeFlag)     ;; if we're deleting the old shape then don't draw anything
    cp 1
    jr z, drawNothing   
    ld (hl), SHAPE_CHAR_0
    
drawNothing
    inc hl
    xor a                               ; clear flags for rra, looks like not needed as loading a but is required
    ld a, c    
    rra                                 ; rotate mask to right by one bit
    ld c, a
    djnz drawShapeInner                 ; dnjz decrements b and jumps if not zero
    ld (outerCount), de                 ; store loop count temp
    ld de, (displayOuterIncrement)    
    add hl, de                          ; gets current screen position to next row
    ld de, (outerCount)                 ; retreive  loop count temp
    dec e                               ; dec sets Z so no need for cp  
    jp nz, drawShapeOuter
    
    xor a
    ld (deleteShapeFlag),a
    ret    

getKey
    ;; changed to use the method of reading keys that uses the ROM routine $7bd
    ;; then we test for the key number in a. ALso using more standard keys 
    ;; (O, P left right, q and space are rotate)
    ld bc,(lastk)
    ld a, c
    inc a
    call nz,$7bd
    ret     

fillPlayerArea 
    ld bc, BOTTOM*LINE_LENGTH
    ld hl, line1  
    ld de, line2
    ldir           ; replicate the first line down full area    
    ld bc, 9
    ld (hl),131
    ld d, h
    ld e, l
    inc de
    ldir           ; fill bottom row
    ld (hl), 118   ; end of line character
    ret



;;;;;;;;;; use dr.beeps method, saves memory
waitForTVSync
    ld hl,frames
	ld a, (hl)
	sub VSYNCLOOP
wfr
    cp (hl)
	jr nz, wfr
    ret

vars
    db 118
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
   db %00111100,  %00101011, %10101010, %00011101, %00101101, %00011110   ; should be drawn vertically
   db %11001100,  %00101110, %00001111, %11100100, %01101100, %11000110   ; should be drawn horiz
   db %00111100,  %11010100, %10101010, %10111000, %10110100, %00011110   ; should be drawn vertically
   db %11001100,  %11101000, %00001111, %01001110, %01101100, %11000110   ; should be drawn horiz   

shape_row_index     ; the current row of the top of the falling shape
    db 0   
    db 0     ; needs to be 2 bytes and second one zero as we ld de to enable add hl,de
outerCount  
    dw 0
currentShapeOffset    
    dw 0
shapeTrackLeftRight
    dw 0    
shape_row
    db 0
flagForBottomHit
    db 0
checkColOffsetStartRow
    dw 0
checkRowIndex
    db 0
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
deleteShapeFlag
    db 0
zero
    db 0 

; The playing area is a shrunk down ZX81 display. the subroutine fillPlayerArea
; initialises tghe full screen. This keeps the assembled file to well below 
; 949 bytes which is the maximum safe to load on an unexpancded ZX81
dfile
       db 118
score    
       db 28,28,28,28,0          ; dr.beep suggested 4 digit score comment on youtube
highScore                        ; rather than needing the S and HS as it's fairly obvious
       db 28,28,28,28,118        ; dr.beep suggested 4 digit highscore comment on youtube
line1  db 8,0,0,0,0,0,0,0,8,118
line2  db 118

last     equ $
end

