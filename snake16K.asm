;;;;;;;;;;;;;;;;;;;;;
;;; zx81 16K code 
;;; It's a clone of snake (in case that wasn't clear from filename;)
;;;;;;;;;;;;;;;;;;;;;

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"                ;; removed some of unneeded definitions
#include "line1.asm"

; for start key 
#define KEYBOARD_READ_PORT_A_TO_G	$FD
; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

#define SHAPE_CHAR_0   128        ; black square
#define SHAPE_CHAR_FOOD 136
#define SHAPE_CHAR_NO_PRINT   0        ; black square
#define SNAKE_MOVEMENT_LEFT 1
#define SNAKE_MOVEMENT_RIGHT 2
#define SNAKE_MOVEMENT_UP 3
#define SNAKE_MOVEMENT_DOWN 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp intro_title		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
introWaitLoop
	ld bc,$00ff ;max waiting time
introWaitLoop_1
	dec bc
	ld a,b
	or c
	jr nz, introWaitLoop_1
	jp read_start_key
	
intro_title
	call CLS	
	ld bc,1
	ld de,titleBanner
	call printstring	
	ld bc,34
	ld de,titleBanner
	call printstring		
	ld bc,111
	ld de,title_screen_txt
	call printstring
	ld bc,202    
	ld de,keys_screen_txt_1
	call printstring		

	ld bc,246
	ld de,keys_screen_txt_2
	call printstring		
    
	ld bc,337
	ld de,game_objective_txt
	call printstring	
	ld bc,436
	ld de,last_Score_txt
	call printstring	
	;ld b, 14			; b is row to print in
	;ld c, 13			; c is column
    ;ld a, (last_score_mem_hund) ; load hundreds
	;call printByte    
	;ld b, 14			; b is row to print in
	;ld c, 15			; c is column
	;ld a, (last_score_mem_tens) ; load tens		
	;call printByte	
	ld bc,537	
	ld de,credits_and_version_1
	call printstring		
	ld bc,569	
	ld de,credits_and_version_2
	call printstring	
	
	ld bc,727
	ld de,titleBanner
	call printstring		
	ld bc,760
	ld de,titleBanner
	call printstring	

read_start_key
	ld a, KEYBOARD_READ_PORT_A_TO_G	
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop
    ;; else drop into initVariables

initVariables    
    call drawInitialScreen
    
    ld bc,3
    ld de,scoreText   
    call printstring	
    
    xor a
    ld (score_mem_tens), a
    ld bc, 10
    ld de, score_mem_tens
    call printNumber    
    
    
    ld a, 4 
    ld (snakeTailIndex), a
    
    ld hl, $0aff
    ld (waitSpeed), hl
    

    ld a, 10                    ; bit wasteful of memory, should put in loop?!
    ld (snakeCoordsRow), a       
    ld (snakeCoordsRow+1), a    
    ld (snakeCoordsRow+2), a    
    ld (snakeCoordsRow+3), a    
    ld (snakeCoordsRow+4), a    
    
    ld a, 15			  
    ld (snakeCoordsCol), a
    dec a
    ld (snakeCoordsCol+1), a
    dec a
    ld (snakeCoordsCol+2), a
    dec a
    ld (snakeCoordsCol+3), a
    dec a
    ld (snakeCoordsCol+4), a
    
    ; default movement is right
    ld a, SNAKE_MOVEMENT_RIGHT
    ld (snakeMovementFlags), a
    ld (snakeMovementFlags+1), a
    ld (snakeMovementFlags+2), a
    ld (snakeMovementFlags+3), a
    ld (snakeMovementFlags+4), a
    ld (snakeMovementFlags+5), a
    ld (snakeMovementFlags+6), a
 
  
    
    ld a, SNAKE_MOVEMENT_RIGHT
    ld (movedFlag),a
    
    ld hl, Display
    ld de, 346
    add hl, de
    ld (absoluteScreenMemoryPosition), hl    
    ld hl, (absoluteScreenMemoryPosition)
    ld (hl), SHAPE_CHAR_0        ; draw inital snake
    dec hl    
    ld (hl), SHAPE_CHAR_0        ; draw inital snake
    dec hl
    ld (hl), SHAPE_CHAR_0        ; draw inital snake
    dec hl
    ld (hl), SHAPE_CHAR_0        ; draw inital snake

    ; default two more food so is always 4 on screen at any one time
    call setRandomFood
    call setRandomFood 
    call setRandomFood
    call setRandomFood
    call setRandomFood
    ;ld hl, Display 
    ;ld de, 550
    ;add hl, de
    ;ld (hl), 136    ; this is the first food for snake, and as a test before random

    ;ld hl, Display 
    ;ld de, 207
    ;add hl, de
    ;ld (hl), 136    ; this is the first food for snake, and as a test before random
    
main
    ; read the keyboard input and adust the offset     
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a                            ; Z
    jp z, drawLeft								
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a						    ; M
    jp z, drawRight							    ; jump to move shape right	

    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a					        ; X
    jp z, drawUp	
    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a					        ; N
    jp z, drawDown

    ;; we also have to keep track of the movement direction of the tail, which can clearly
    ;; be different to the head

    ;;; keep moving in same direction even if no key pressed
    ld a, (movedFlag)
    cp SNAKE_MOVEMENT_LEFT
    jp z, drawLeft
    cp SNAKE_MOVEMENT_RIGHT
    jp z, drawRight
    cp SNAKE_MOVEMENT_UP
    jp z, drawUp
    cp SNAKE_MOVEMENT_DOWN
    jp z, drawDown
 
    ret
    jp drawBlock
drawLeft   
    ld a, SNAKE_MOVEMENT_LEFT
    ld (movedFlag), a      
    ld a, (snakeCoordsCol)    
    dec a    
    cp 0
    jp z, drawBlock    
    
    call shuffleSnakeInCol
    
    ld hl, (absoluteScreenMemoryPosition)
    dec hl    
    ld (absoluteScreenMemoryPosition), hl        
    jp drawBlock
    
drawRight
    ld a, SNAKE_MOVEMENT_RIGHT
    ld (movedFlag), a    
    ld a, (snakeCoordsCol)  
    inc a
    cp 31    
    jp z, drawBlock

    call shuffleSnakeInCol    
   
    ld hl, (absoluteScreenMemoryPosition)
    inc hl    
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock  
    
drawUp
    ld a, SNAKE_MOVEMENT_UP
    ld (movedFlag), a
    ld a, (snakeCoordsRow)    
    dec a    
    cp 0    
    jp z, drawBlock
    
    call shuffleSnakeInRow
        
    ld hl, (absoluteScreenMemoryPosition)
    xor a
    push hl
    pop bc
    ld de, 33    
    sbc hl,de
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock
drawDown
    ld a, SNAKE_MOVEMENT_DOWN
    ld (movedFlag), a 
    ld a, (snakeCoordsRow)
    inc a
    cp 22
    jp z, drawBlock    

    call shuffleSnakeInRow

    ld hl, (absoluteScreenMemoryPosition)
    ld bc, 33
    add hl, bc
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock
       
drawBlock
    ; we need to check cursor position we've moved to for and existing block 
    ; not on first time throught though when not moved   
    ld a, (movedFlag)
    cp 0
    jp z, noCheck

    ld hl, (absoluteScreenMemoryPosition)
   
    xor a           ; zero a and clear flags
    ld a, (hl)
    sub SHAPE_CHAR_0    
    jp z, gameOver
    
;    di              ; disable interrupts
;    LD A,0          ; disable NMI for DEBUG only
;    OUT ($FD),A     ; disable NMI for DEBUG only
;here__    
;    jp here__
    
    xor a           ; zero a and clear flags
    ld a, (hl)
    sub SHAPE_CHAR_FOOD
    jp nz, noCheck
    
    call setRandomFood ; generate more food
    call setRandomFood ; generate more food
    
    ld hl, (waitSpeed)  ; speed up by one now we have got some more food
    dec hl              
    ld (waitSpeed), hl
    
    ld a, (score_mem_tens)
    add a, 1
    daa
    ld (score_mem_tens), a
    
    ld bc, 10
    ld de, score_mem_tens
    call printNumber
    
    ; we got the food, so increase length of the snake...    
    ; ...but we also need to set the new coordinates for the tail based on the direction
    ; if moving down new tail is same col, last tail row-1
    ; if moving up new tail is same col, last tail row+1
    ; if moving left new tail is same row last tail col+1
    ; if moving right new tail is same row, last tail col-1    

    ;; the new coordinate  doesn't depend on the current movement, but the 
    ;; direction the previous tail position was moving in
    ;;ld a, (movedFlag)
    ld hl, snakeMovementFlags                   
    ld d, 0
    ld a, (snakeTailIndex)     
    ld e, a
    add hl, de
    ld a, (hl)    
    cp SNAKE_MOVEMENT_LEFT
    jp z, newcorrdForLeft
    cp SNAKE_MOVEMENT_RIGHT
    jp z, newcorrdForRight
    cp SNAKE_MOVEMENT_UP
    jp z, newcorrdForUp
    cp SNAKE_MOVEMENT_DOWN
    jp z, newcorrdForDown
    
    ret
    jp noCheck  ; should never get here as always moving
    
newcorrdForLeft
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCoordsRow                       
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    inc hl  ; to push coord on one
    ld (hl), a      ; make new coord smae as previous

    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCoordsCol
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    inc a           ; going left new coord is current tail plus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous
            
    jp afterCoordAdd

newcorrdForRight
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??
    ld hl, snakeCoordsRow                       
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    inc hl  ; to push coord on one
    ld (hl), a      ; make new coord smae as previous

    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCoordsCol
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    dec a           ; going left new coord is current tail minus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous
    jp afterCoordAdd

newcorrdForUp ; if moving up new tail is same col, last tail row+1
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??
    ld hl, snakeCoordsCol                       
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    inc hl  ; to push coord on one
    ld (hl), a      ; make new coord smae as previous

    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCoordsRow
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    inc a           ; going up new row coord is current tail plus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous
    inc a           ; we have to set the coord after the snakeTailIndex to valid tro prevent extra block dropping 
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous    
    
    jp afterCoordAdd

newcorrdForDown    ; if moving down new tail is same col, last tail row-1
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??
    ld hl, snakeCoordsCol                       
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    inc hl  ; to push coord on one
    ld (hl), a      ; make new coord smae as previous

    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCoordsRow
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    dec a           ; going down new row coord is current tail minus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous    
    dec a           ; we have to set the coord after the snakeTailIndex to valid tro prevent extra block dropping 
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous    
    ; don't need jp afterCoordAdd as next is the same
    
afterCoordAdd    
    ld a, (snakeTailIndex)
    inc a
    ld (snakeTailIndex), a

    
    
    ; drop through

noCheck  
    ld a, (movedFlag)
    cp 0
    jp z, NOwipeLastTailPreviousPos    
    
    ld hl, snakeCoordsRow
    ld b, 0
    ld a, (snakeTailIndex) 
    ld c, a
    add hl, bc
    inc hl          ; set "index" to one past the end
    ld a, (hl)
   
    ;ld a, (snakeCoordsRow+5)   
    ld h, a				    ; row set for PRINTAT
    
    push hl
    ld hl, snakeCoordsCol
    ld b, 0
    ld a, (snakeTailIndex) 
    ld c, a
    add hl, bc
    inc hl          ; set "index" to one past the end
    ld a, (hl)
    pop hl
    ;ld a, (snakeCoordsCol+5)
    ld l, a				    ; column set for PRINTAT
    
    push hl  ; push hl to get into bc via the pop, why is ld bc, hl not an instruction? who am I to question :)
    pop bc
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e  
    ld a, SHAPE_CHAR_NO_PRINT
    call PRINT     
    
NOwipeLastTailPreviousPos        
    ld a, (snakeCoordsRow)   
	ld h, a				    ; row set for PRINTAT
    ld a, (snakeCoordsCol)
    ld l, a				    ; column set for PRINTAT
    
    push hl  ; push hl to get into bc via the pop, why is ld bc, hl not an instruction? who am I to question :)
    pop bc
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e  
    ld a, (shapeSet)
    call PRINT 
  
    ;ld hl, $0fff
    ;ld hl, $ffff
    ld hl, (waitSpeed)
    ;ld hl, $0fff
    push hl
    pop bc
  
waitloop
    dec bc
    ld a,b
    or c
    jr nz, waitloop    
    jp main
  
gameOver
    ld bc,342
    ld de,game_over_txt   
    call printstring	

    ld hl, $ffff
    push hl
    pop bc
  
waitloop_endGame
    dec bc
    ld a,b
    or c
    jr nz, waitloop_endGame      
    jp intro_title
; this prints at top any offset (stored in bc) from the top of the screen D_FILE
printstring
    ld hl,Display
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

printNumber
    ld hl,Display
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
    
drawInitialScreen    
    ;; draw boarder for the play area consisting of inverse x character decinaml 189
    ;; is a lot faster (ie perceptively instant) not using the PRINTAT and PRINT calls
    call CLS

    ld de, 0
    ld (absoluteScreenMemoryPosition), de    
drawLineZeroAndLast         ; draw the boarder at top and bottom
    ld hl, Display+1    
    add hl, de    
    ld (hl), 189
    push de
    ld de, 726
    add hl, de    
    ld (hl), 189   
    pop de
    inc de
    ld a, 32
    cp e
    jp nz, drawLineZeroAndLast

    ld b, 22            ;; best way to just draw column down each side of screen
    ld de, 31
    ld hl, Display+1
drawColZero      
    ld (hl), 189          
    add hl, de  
    ld (hl), 189    
    inc hl    
    inc hl
    djnz drawColZero

    ret

;;;;;;;;;;;;;;;;;;;;; SHUFFLE SNAKE IN COL    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

shuffleSnakeInCol
    ld (snakeCoordsColTemp), a          ; save the new position of the head of the snake    
    ld a, (snakeTailIndex)    
    inc a
    ld b, a 
    ;; loop for b
    
CdrawLeftSnakeShuffleLoopCol    
    ld hl, snakeCoordsCol                        
    ld d, 0
    ld e, b    
    dec e       ; we want e to be one less than the loop counter
    add hl, de
    ld a, (hl)
    inc hl    
    ld (hl), a                

    ld hl, snakeMovementFlags                   
    ld d, 0
    ld e, b    
    dec e       ; we want e to be one less than the loop counter
    add hl, de
    ld a, (hl)
    inc hl    
    ld (hl), a                
    
    djnz CdrawLeftSnakeShuffleLoopCol     
       
    ld a, (snakeCoordsColTemp)      ; store the head of the snake new position in (snakeCoordsCol)
    ld (snakeCoordsCol), a
    
    ld a, (movedFlag)
    ld (snakeMovementFlags), a    

    ld a, (snakeTailIndex)    
    inc a
    ld b, a 
    ;; loop for b
    
CdrawLeftSnakeShuffleLoopRow   
    ld hl, snakeCoordsRow                       
    ld d, 0
    ld e, b    
    dec e       ; we want e to be one less than the loop counter
    add hl, de
    ld a, (hl)
    inc hl    
    ld (hl), a                
    djnz CdrawLeftSnakeShuffleLoopRow
    ret
    
;;;;;;;;;;;;;;;;;;;;; SHUFFLE SNAKE IN ROW    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
shuffleSnakeInRow
    ld (snakeCoordsRowTemp), a          ; save the new position of the head of the snake        
    ld a, (snakeTailIndex)        
    inc a
    ld b, a 
    ;; loop for b
RdrawLeftSnakeShuffleLoopRow
    ld hl, snakeCoordsRow                        
    ld d, 0
    ld e, b    
    dec e       ; we want e to be one less than the loop counter
    add hl, de
    ld a, (hl)
    inc hl    
    ld (hl), a                

    ld hl, snakeMovementFlags                      
    ld d, 0
    ld e, b    
    dec e       ; we want e to be one less than the loop counter
    add hl, de
    ld a, (hl)
    inc hl    
    ld (hl), a  
    
    djnz RdrawLeftSnakeShuffleLoopRow     
        
    ld a, (snakeCoordsRowTemp)      ; store the head of the snake new position in (snakeCoordsCol)
    ld (snakeCoordsRow), a

    ld a, (movedFlag)
    ld (snakeMovementFlags), a    

    ld a, (snakeTailIndex)    
    inc a
    ld b, a 
    ;; loop for b
    
RdrawLeftSnakeShuffleLoopCol   
    ld hl, snakeCoordsCol                       
    ld d, 0
    ld e, b    
    dec e       ; we want e to be one less than the loop counter
    add hl, de
    ld a, (hl)
    inc hl    
    ld (hl), a                
    djnz RdrawLeftSnakeShuffleLoopCol
    ret

setRandomFood
    ; get a random column, then row and set as the food block (136)
    ; TODO probably need to check if the generated row and column isn't one of the snakes coords!!!
    
tryAnotherRCol                             ; generate random number to index shape memory
    ld a, r                             ; we want a number 0 to 4 inclusive 
    and %00011111
    cp 29    
    jp nc, tryAnotherRCol                  ; loop when nc flag set ie not less than 5 try again    
    inc a
    ld (setRandomFoodCOL), a
    
tryAnotherRRow                             ; generate random number to index shape memory
    ld a, r                             ; we want a number 0 to 4 inclusive 
    and %00011111
    cp 20    
    jp nc, tryAnotherRRow                  ; loop when nc flag set ie not less than 5 try again    
    inc a
    ld (setRandomFoodROW), a
    
    ld a, (setRandomFoodROW)   
	ld h, a				    ; row set for PRINTAT
    ld a, (setRandomFoodCOL)
    ld l, a				    ; column set for PRINTAT
    
    push hl  ; push hl to get into bc via the pop, why is ld bc, hl not an instruction? who am I to question :)
    pop bc
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e  
    ld a, SHAPE_CHAR_FOOD
    call PRINT 
    
    ret
    
   
#include "line2.asm"
#include "screenFull.asm"      			; definition of the screen memory, in colapsed version for 1K        
game_over_txt
	DEFB	_G,_A,_M,_E,__,_O,_V,_E,_R,$ff 
scoreText    
    DEFB	_S,_C,_O,_R,_E,__,__,$ff 
;first_line_a
;    DEFB _L,_E,_F,_T,__,_Z,__,_R,_I,_G,_H,_T,$ff 
;first_line_b    
;    DEFB __,_M,__,_U,_P,__,_X,__,_D,_O,_W,_N,__,_N,$ff 
shapeSet
    DEFB SHAPE_CHAR_0
shapeOnFlag
    DEFB 0
movedFlag
    DEFB 0      ; default to stationary = 0, 1 = left, 2 = right, 3 = up, 4 = down
absoluteScreenMemoryPosition
    DEFB 0,0    
firstTimeFlag
    DEFB 1   
snakeTailIndex      ; this is the index of the last coordinate of the snake, 3 initially (meaning there are 4 snake blocks)
    DEFB 0
snakeTailPlusOne      ; this is the index of the last coordinate of the snake, 3 initially (meaning there are 4 snake blocks)
    DEFB 0    
setRandomFoodCOL
    DEFB 0
setRandomFoodROW
    DEFB 0
waitSpeed
    DEFB 0,0
score_mem_tens
    DEFB 0,0
; the snake can grow to a maximum length of 16 so store 16 row and column positions
; to enable it to be undrawn as it moves around. will increase once code works
; when we use these we will optimise by index offset of 16 as contiguous in memory

; the intention is to delete (on screen) the tail of the snake which is at whatever the length is set to
; position and then draw that.

;; eventually we'll add code to make the snake longer when items are collected (eaten), and in that case
;; we'll have to store a tail index, initially 4 lnog
snakeMovementFlags           ; this keeps track of the direction in force at each snake body position
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0    
snakeCoordsCol            
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0    
snakeCoordsRow    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0    

title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_S,_N,_A,_K,_E,$ff
keys_screen_txt_1
	DEFB	_S,__,_T,_O,__,_S,_T,_A,_R,_T,26,__,_Z,__,_L,_E,_F,_T,26,__,_M,__,_R,_I,_G,_H,_T,$ff
keys_screen_txt_2
	DEFB	__,_X,__,_U,_P,26,__,__,__,_N,__,_D,_O,_W,_N,$ff    
game_objective_txt
	DEFB	_G,_R,_O,_W,__,_S,_N,_A,_K,_E,__,_A,_N,_D,__,_A,_V,_O,_I,_D,$ff
	
last_Score_txt
	DEFB	21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff	
high_Score_txt
	DEFB	21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff		
titleBanner		
	DEFB	4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,$ff		
credits_and_version_1
	DEFB _B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,$ff
credits_and_version_2
	DEFB __,__,__,__,__,__,_2,_0,_2,_2,__,__,__,__,__,$ff
    
    
snakeCoordsColTemp
    DEFB 0
snakeCoordsRowTemp
    DEFB 0
   
#include "endbasic.asm"
