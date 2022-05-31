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

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

#define SHAPE_CHAR_0   128        ; black square
#define SHAPE_CHAR_NO_PRINT   0        ; black square
#define SNAKE_LEN_MINUS_ONE 15

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp initVariables		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

initVariables    
    call drawInitialScreen

    ld a, 10                    ; bit wasteful of memory, should put in loop?!
    ld (snakeCoordsRow), a       
    ld (snakeCoordsRow+1), a    
    ld (snakeCoordsRow+2), a    
    ld (snakeCoordsRow+3), a    
    
    ld a, 15			  
    ld (snakeCoordsCol), a
    dec a
    ld (snakeCoordsCol+1), a
    dec a
    ld (snakeCoordsCol+2), a
    dec a
    ld (snakeCoordsCol+3), a

    ld a, 3
    ld (snakeTailIndex), a
    
    ld a, 0
    ld (movedFlag),a
    
    ld hl, Display
    ld de, 346
    add hl, de
    ld (absoluteScreenMemoryPosition), hl    
    ld hl, (absoluteScreenMemoryPosition)
    ld (hl), SHAPE_CHAR_0        ; draw inital snake
    dec hl    
    ld (hl), SHAPE_CHAR_0        ; draw inital snak
    dec hl
    ld (hl), SHAPE_CHAR_0        ; draw inital snak
    dec hl
    ld (hl), SHAPE_CHAR_0        ; draw inital snak
    
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

    ;;; keep moving in same direction even if no key pressed
    ld a, (movedFlag)
    cp 1
    jp z, drawLeft
    cp 2
    jp z, drawRight
    cp 3
    jp z, drawUp
    cp 4
    jp z, drawDown
 
    jp drawBlock
drawLeft   
    ld a, 1
    ld (movedFlag), a      
    ld a, (snakeCoordsCol)    
    dec a    
    cp 0
    jp z, drawBlock    
    
    ; mv all the coordinates leaving the zero index to the new coord
    
; if this were C, but only works for 4 long snake, needs to be loop
;snakeCoordsCol[3] = snakeCoordsCol[2]
;snakeCoordsCol[2] = snakeCoordsCol[1]
;snakeCoordsCol[1] = snakeCoordsCol[0]
;snakeCoordsRow[3] = snakeCoordsRow[2]
;snakeCoordsRow[2] = snakeCoordsRow[1]
;snakeCoordsRow[1] = snakeCoordsRow[0] and leave snakeCoordsRow[0] as is
    push af     ; store current a which is new col
    ld a, (snakeCoordsCol+3)
    ld (snakeCoordsCol+4), a    
    ld a, (snakeCoordsCol+2)
    ld (snakeCoordsCol+3), a
    ld a, (snakeCoordsCol+1)
    ld (snakeCoordsCol+2), a
    ld a, (snakeCoordsCol+0)
    ld (snakeCoordsCol+1), a
    pop af
    ld (snakeCoordsCol), a    

    ld a, (snakeCoordsRow+3)
    ld (snakeCoordsRow+4), a    
    ld a, (snakeCoordsRow+2)
    ld (snakeCoordsRow+3), a
    ld a, (snakeCoordsRow+1)
    ld (snakeCoordsRow+2), a
    ld a, (snakeCoordsRow+0)
    ld (snakeCoordsRow+1), a
    ;;ld a, (snakeCoordsRow+0) snake row + zero is same
    
    ld hl, (absoluteScreenMemoryPosition)
    dec hl    
    ld (absoluteScreenMemoryPosition), hl        
    jp drawBlock
    
drawRight
    ld a, 2
    ld (movedFlag), a    
    ld a, (snakeCoordsCol)  
    inc a
    cp 31    
    jp z, drawBlock

    ld (snakeCoordsColTemp), a      ; save til the shuffle complete
    ld hl, snakeCoordsCol            
    ld b, 0
    ld a, (snakeTailIndex) 
    ld c, a
    add hl, bc
    ld bc, (snakeTailIndex)          
    ;; loop for b
drawLeftSnakeShuffleLoopCol
    ld a, (hl)    
    inc hl
    ld (hl), a    
    dec hl
    dec hl
    djnz drawLeftSnakeShuffleLoopCol     
    
    ld a, (snakeCoordsColTemp); set the new head position to the temp stored one right at the end    
    ld (snakeCoordsCol), a    

    ld a, (snakeCoordsRow+3)
    ld (snakeCoordsRow+4), a      ;; we're setting the index one after tail to the last tail before moving
    ld a, (snakeCoordsRow+2)
    ld (snakeCoordsRow+3), a
    ld a, (snakeCoordsRow+1)
    ld (snakeCoordsRow+2), a
    ld a, (snakeCoordsRow+0)
    ld (snakeCoordsRow+1), a       
    
    ld hl, (absoluteScreenMemoryPosition)
    inc hl    
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock  
    
drawUp
    ld a, 3
    ld (movedFlag), a
    ld a, (snakeCoordsRow)    
    dec a    
    cp 0    
    jp z, drawBlock
    
    push af     ; store current a which is new col
    ld a, (snakeCoordsCol+3)
    ld (snakeCoordsCol+4), a  
    ld a, (snakeCoordsCol+2)
    ld (snakeCoordsCol+3), a
    ld a, (snakeCoordsCol+1)
    ld (snakeCoordsCol+2), a
    ld a, (snakeCoordsCol+0)
    ld (snakeCoordsCol+1), a

    ld a, (snakeCoordsRow+3)
    ld (snakeCoordsRow+4), a      
    ld a, (snakeCoordsRow+2)
    ld (snakeCoordsRow+3), a
    ld a, (snakeCoordsRow+1)
    ld (snakeCoordsRow+2), a
    ld a, (snakeCoordsRow+0)
    ld (snakeCoordsRow+1), a        
    pop af    
    ld (snakeCoordsRow), a    
    
    ld hl, (absoluteScreenMemoryPosition)
    xor a
    push hl
    pop bc
    ld de, 33    
    sbc hl,de
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock
drawDown
    ld a, 4
    ld (movedFlag), a 
    ld a, (snakeCoordsRow)
    inc a
    cp 22
    jp z, drawBlock    
    
    push af     ; store current a which is new col
    ld a, (snakeCoordsCol+3)
    ld (snakeCoordsCol+4), a  
    ld a, (snakeCoordsCol+2)
    ld (snakeCoordsCol+3), a
    ld a, (snakeCoordsCol+1)
    ld (snakeCoordsCol+2), a
    ld a, (snakeCoordsCol+0)
    ld (snakeCoordsCol+1), a

    ld a, (snakeCoordsRow+3)
    ld (snakeCoordsRow+4), a      
    ld a, (snakeCoordsRow+2)
    ld (snakeCoordsRow+3), a
    ld a, (snakeCoordsRow+1)
    ld (snakeCoordsRow+2), a
    ld a, (snakeCoordsRow+0)
    ld (snakeCoordsRow+1), a        
    pop af    
    ld (snakeCoordsRow), a    


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
   
    ld a, (hl)
    and SHAPE_CHAR_0    
    jp nz, gameOver

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
    ;ld a, (snakeCoordsRow+4)   
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
    ;ld a, (snakeCoordsCol+4)
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
  
    ld hl, $0fff
    ;ld hl, $ffff
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
    jp initVariables
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
   
#include "line2.asm"
#include "screenFull.asm"      			; definition of the screen memory, in colapsed version for 1K        
game_over_txt
	DEFB	_G,_A,_M,_E,__,_O,_V,_E,_R,$ff 
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
; the snake can grow to a maximum length of 16 so store 16 row and column positions
; to enable it to be undrawn as it moves around. will increase once code works
; when we use these we will optimise by index offset of 16 as contiguous in memory

; the intention is to delete (on screen) the tail of the snake which is at whatever the length is set to
; position and then draw that.

;; eventually we'll add code to make the snake longer when items are collected (eaten), and in that case
;; we'll have to store a tail index, initially 4 lnog
snakeCoordsRow    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
snakeCoordsCol            
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
snakeCoordsColTemp
    DEFB 0
#include "endbasic.asm"
