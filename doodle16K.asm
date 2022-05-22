;;;;;;;;;;;;;;;;;;;;;
;;; zx81 16K code (gave up for now with getting tetris working in 1K!!)
;;; It's a clone of tetris (in case that wasn't clear from filename;)
;;; The code heavily!! dependant on the definition of the screen memory in screenTetris.asm
;;;;;;;;;;;;;;;;;;;;;
;;; as of 16/5/2022 size of assembled p file is 934bytes (has to be < 950 ish)

; TODO  
;   scoring
;   side to side collision detect
;   speed up game each time line removed
;   the delete old shape needs to use the proper shape definition, not overwrite 8 blocks
;   straight shape doesn't go all way to left, due to wayit's defined and drawn
;   when shape next to edge no logic to prevent rotation, so sticks to wall or worse goes through

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
    DEFB 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp initVariables		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

initVariables
    
    ld a, 10
    ld (shapeRow),a
    ld a, 10
    ld (shapeCol),a
    
    ld a,(shapeRow)
	ld h, a				; row set for PRINTAT
    ld a, (shapeCol)
    ld l, a				; row set for PRINTAT
    push hl
    pop bc
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e    
main
    xor a
    ld (movedFlag), a    
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

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a					; SPACE
    jr z, switchShapeOff	    

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 1, a			; .
    jr z, switchShapeOn	

    xor a
    ld (movedFlag), a
    
    jp drawBlock
drawLeft   
    ld a, 1
    ld (movedFlag), a
    ld a, (shapeCol)         
    dec a    
    cp 0
    jp z, drawBlock    
    ld (shapeCol),a
    jp drawBlock
drawRight
    ld a, 1
    ld (movedFlag), a
    ld a, (shapeCol)         
    inc a
    cp 31
    jp z, drawBlock
    ld (shapeCol),a    
    jp drawBlock
drawUp
    ld a, 1
    ld (movedFlag), a
    ld a, (shapeRow)         
    dec a    
    cp 0
    jp z, drawBlock    
    ld (shapeRow),a
    jp drawBlock
drawDown
    ld a, 1
    ld (movedFlag), a
    ld a, (shapeRow)    
    inc a
    cp 22
    jp z, drawBlock
    ld (shapeRow),a
    jp drawBlock

switchShapeOff
    ld a, SHAPE_CHAR_NO_PRINT 
    ld (shapeSet), a 
    ld a, 0
    ld (shapeOnFlag), a 
    jp drawBlock
    
switchShapeOn
    ld a, SHAPE_CHAR_0 
    ld (shapeSet), a 
    ld a, 0
    ld (shapeOnFlag), a     
    jp drawBlock
    
drawBlock
    ld hl, (PR_CC)      ; else check if there was a shape there before
    ld a, (hl)
    and SHAPE_CHAR_0
    jp z, restoreCurrentPositionBlock
    
    jp putBlankThere 
    
restoreCurrentPositionBlock
    ld hl, (PR_CC)
    ld (hl), SHAPE_CHAR_0
    jp moveCursor
putBlankThere
    ld hl, (PR_CC)
    ld (hl), 0 
moveCursor   
    ld a,(shapeRow)
	ld h, a				; row set for PRINTAT
    ld a, (shapeCol)
    ld l, a				; row set for PRINTAT
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
