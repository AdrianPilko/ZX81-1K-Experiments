;;;;;;;;;;;;;;;;;;;;;
;;; Spirals game from the book Mastering Machine code on your zx81 1981 - by Toni Baker
;;; typed in here from the assembly, rather than machine code directly on ZX81 would have used HEXLD3.
;;; 
;;; Typed in and ammended by AdrianPilko(ByteForever) October 2023
;;; The code heavily!! dependant on the definition of the screen memory in screen.asm
;;;;;;;;;;;;;;;;;;;;;

;; keys are ...

;; my own changes from the book type-in version:
;;      - 
;;
;; known bugs
;;      - 
;; todo
;;      - 


;#define DEBUG_PRINT
;#define DEBUG_SLOW
;#define LIVES_1

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"
#include "line1.asm"
    jp start
sprint:    ; string print
    push hl    ; the code in book could not have worked it had the miusbalanced push pop hl called from start 
oneAfterStartSprint:
    pop hl
    ld a, (hl)
    inc hl
    push hl
    cp $ff
    ret z
    rst 10
    jr oneAfterStartSprint    ; the code in book could not have worked it had the miusbalanced push pop hl called from start 
start:    
    ld hl, boardDef
    call sprint
    ret
    
boardDef:
    DEFB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$76
    DEFB $80,$15,$80,$00,$00,$00,$00,$00,$00,$00,$80,$76
    DEFB $80,$00,$80,$00,$80,$80,$80,$80,$80,$00,$80,$76
    DEFB $80,$00,$80,$00,$80,$00,$00,$00,$80,$00,$80,$76
    DEFB $80,$00,$80,$00,$80,$00,$80,$00,$80,$00,$80,$76
    DEFB $80,$00,$80,$00,$80,$80,$80,$00,$80,$00,$80,$76
    DEFB $80,$00,$80,$00,$00,$00,$00,$00,$80,$00,$80,$76
    DEFB $80,$00,$80,$80,$80,$80,$80,$80,$80,$00,$80,$76
    DEFB $80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$76
    DEFB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$76
    DEFB $76
    DEFB $3e,$34,$3a,$37,$00,$38,$28,$34,$37,$2a,$00,$33
    DEFB $34,$3c,$00,$25,$25,$25,$1c,$1c,$ff
	
    ret  ;;; never gets here

    
debugPrintRegisters
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ; position the cursor
    ;set b to row, c to first col, which is the last row    
    ld b, 0     ; have seen strange thing when debug comes out and bug happens it drops one line
    ld c, 1
    ld b, 21        
    call PRINTAT
    pop bc
    pop af
    pop de
    pop hl    
    
    push hl
    push de
    push af    
    push bc
    
    ld a, a
    call hprint    
    ld a, 14
    call PRINT  

    ld a, h
    call hprint    
    ld a, l    
    call hprint
    ld a, 14
    call PRINT
       
    ld a, d
    call hprint
    ld a, e
    call hprint
    ld a, 14
    call PRINT

    ld a, b
    call hprint
    ld a, c
    call hprint    
    ld a, 14
    call PRINT      
  
    ;;print a couple of memory locations
;    ld hl, (?????) 
;    ld a, h
;    call hprint    
;    ld a, l
;    call hprint      
;    ld a, 14
;    call PRINT

 ;   ld hl, (?????) 
 ;   ld a, h
 ;   call hprint    
 ;   ld a, l
 ;   call hprint    
 ;   ld a, 14
 ;   call PRINT

    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret
    
hprint 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	push af ;store the original value of a for later
	and $f0 ; isolate the first digit
	rra
	rra
	rra
	rra
	add a,$1c ; add 28 to the character code
	call PRINT ;
	pop af ; retrieve original value of a
	and $0f ; isolate the second digit
	add a,$1c ; add 28 to the character code
	call PRINT
	ret
    
hprintInverse 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	push af ;store the original value of a for later
	and $f0 ; isolate the first digit
	rra
	rra
	rra
	rra
	add a,$9c ; add 156 to the character code make it inverse (28to get to character from number +128inverse
	call PRINT ;
	pop af ; retrieve original value of a
	and $0f ; isolate the second digit
	add a,$9c ; add 28 to the character code
	call PRINT
	ret
    
    
; this prints at top any offset (stored in bc) from the top of the screen D_FILE
printstring
	ld hl,(D_FILE)
    inc hl
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


kscan:
    ld hl, $ffff
    ld bc, $fefe
    in a, (c)
    or $01
kscanloop:     
    or $e0
    ld d, a
    cpl
    cp $01
    sbc a, a
    or b
    and l
    ld l, a
    ld a, h
    and d
    ld h, a
    rlc b
    in a, (c)
    jr c, kscanloop
    rra
    rl h
    ret

    
    
#include "line2.asm"
#include "screenFull.asm" 
;;; ball "directions", used to add or subract ball position to move diagonally down left or right (tablestartlow) then up left right - these are offsets which with the code to moveball causes the ball to move in screen memory
#include "endbasic.asm"
