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
#define SHAPE_CHAR_NO_PRINT   136        ; black square

shapeRow 
    DEFB 0
shapeCol     
    DEFB 0
shapeSet
    DEFB SHAPE_CHAR_0
shapeOnFlag
    DEFB 0
movedFlag
    DEFB 5      ; default to stationary , 0 = left, 1 = right, 2 = up, 3 = down
absoluteScreenMemoryPosition
    DEFB 0,0    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp initVariables		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

initVariables
    
    ld a, 10
    ld (shapeRow),a
    ld h, a				; row set for PRINTAT       
    ld a, 15
    ld (shapeCol),a
    ld l, a				; row set for PRINTAT    
    push hl
    pop bc
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e    
    
    ld a, 5
    ld (movedFlag),a

main
    ; read the keyboard input and adust the offset     
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a
    ; check bit set for key press left  (Z)
    jp z, drawLeft								
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a						    ; Z
    jr z, drawRight							    ; jump to move shape right	

    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a					        ; X
    jr z, drawUp	
    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a					; N
    jr z, drawDown	    

    ;;; keep moving in same direction even if no key pressed
    ld a, (movedFlag)
    cp 0
    jp z, drawLeft
    cp 1
    jp z, drawRight
    cp 2
    jp z, drawUp
    cp 3
    jp z, drawDown
 
    jp drawBlock
drawLeft   
    ld a, 0
    ld (movedFlag), a
    ld a, (shapeCol)         
    dec a    
    cp 0
    jp z, drawBlock    
    ld (shapeCol),a
    ld hl, (absoluteScreenMemoryPosition)
    dec hl    
    ld (absoluteScreenMemoryPosition), hl        
    jp drawBlock
drawRight
    ld a, 1
    ld (movedFlag), a
    ld a, (shapeCol)         
    inc a
    cp 31
    jp z, drawBlock
    ld (shapeCol),a    
    ld hl, (absoluteScreenMemoryPosition)
    inc hl
    ld (absoluteScreenMemoryPosition), hl
    jp drawBlock
drawUp
    ld a, 2
    ld (movedFlag), a
    ld a, (shapeRow)         
    dec a    
    cp 0
    jp z, drawBlock    
    ld (shapeRow),a
    ld hl, (absoluteScreenMemoryPosition)
    ld a, l
    sub a,33
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock
drawDown
    ld a, 3
    ld (movedFlag), a
    ld a, (shapeRow)    
    inc a
    cp 22
    jp z, drawBlock
    ld (shapeRow),a
    ld hl, (absoluteScreenMemoryPosition)
    add hl, 33
    ld (absoluteScreenMemoryPosition), hl
    
    jp drawBlock
    
drawBlock
    ld a,(shapeRow)
	ld h, a				; row set for PRINTAT
    ld a, (shapeCol)
    ld l, a				; row set for PRINTAT
    
    ; we need to check current cursor position for existing block
        
    
    push hl
    pop bc
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
drawIt    
    ld a, (shapeSet)
    call PRINT 
  
    ld hl, $0fff
    push hl
    pop bc
  
waitloop
    dec bc
    ld a,b
    or c
    jr nz, waitloop    
    jp main
   
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

   
#include "line2.asm"
#include "screenFull.asm"      			; definition of the screen memory, in colapsed version for 1K        
#include "endbasic.asm"
