;;;;;;;;;;;;;;;;;;;;;
;;; Breakout game from the book Mastering Machine code on your zx81 1981 - Toni Baker
;;; typed in here from the assembly, rather than machine code directly on ZX81 would have used HEXLD3.
;;;
;;; The code heavily!! dependant on the definition of the screen memory in screen.asm
;;;;;;;;;;;;;;;;;;;;;

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"
#include "line1.asm"
    jp breakout

;;; ball "directions", used to add or subract ball position to move diagonally down left or right (tablestartlow) then up left right - these are offsets which with the code to moveball causes the ball to move in screen memory
tablestartlow:
    DEFB $00
tablestarthigh:        
    DEFB $20         ;; 31 to move ball down and to left
    DEFB $00
    DEFB $22         ;; 33 to move ball down and to right
    DEFB $ff
    DEFB $e0         ;; -31 when taken as twos compliment up and right
    DEFB $ff
    DEFB $de         ;; -33 when taken as twos compliment up and left
ballinit:
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
speed:     
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
ballpos:
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
direction:
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
batpos:    
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
seed:    
    DEFW $ffe0         ;; these were just addresses in the machine code, here define a label
   
breakout:
    ld hl,(D_FILE)
    ld de,$0085 
    add hl,de
    ld bc,$8080   ; b is number of bricks, c is 128 (0x80) which is black square for sides and top boarder
nxbrk:
    inc hl
    ld a,(hl)
    cp $76
    jr z, nxbrk
    ld (hl), $08
    djnz nxbrk
    ld hl, (D_FILE)  ; initialise the top part of boarder
    ld b,$1e
nxbl:
    inc hl
    ld (hl), c
    djnz nxbl
    inc hl
    ld (hl), $9c      ; current score inverse video "0" character
    inc hl
    ld (hl), c
    inc hl
    inc hl
    ld de,$001f
    ld b, $17
sides:
    ld (hl), c
    add hl, de
    ld (hl), c
    inc hl
    inc hl
    djnz sides
    ld b, $20
    inc hl
base:
    ld (hl), $1b
    inc hl
    djnz base
    
    ld de, $fefc
    add hl, de
    ld (ballinit), hl
    ld hl, $0900
    ld (speed), hl
restart:
    ld hl, (ballinit)
    inc hl
    ld (ballinit), hl
    ld (ballpos), hl
    ld (hl), $34        ; print the ball
    ld hl, $ffe0        ; set initial direction
    ld (direction), hl
    ld a, (speed)
    dec a
    ret z
    ld (speed), a
    ld hl, (D_FILE)
    ld de,  $02b7
    add hl, de
    ld (hl),$00
    ld a, $03
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    ld (batpos), hl
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    ld b, $18
erase:    
    inc hl
    ld (hl), $00
    djnz erase
    ld hl, $0000
    jr delay

loop:                    ; this is the main game loop
    ld hl, (speed)
delay:
    dec hl
    ld a, h
    or l
    jr nz, delay
    inc b
    bit $00, b            ; only move ball every other time
    jr nz, movebat
moveball:
    ld hl, (ballpos)
    ld (hl), $00
    ld de, (direction)    ; initital direction is stored as twos compliment so add is subtract 0xffe0 = 0xffff - 0xffe0 = 31 hence this will move ball diagonally to left up to right
    add hl, de
    ld a, (hl)            ; check contents of next ball position
    cp $1b
    jr z, restart
    ld c, a
    and $f7
    jr nz, dontmove
    ld (hl), $34
    ld (ballpos), hl    
dontmove:
    or c
    jr z, movebat
    push hl    
    ld hl, (seed)
    ld d, h
    ld e, l
    add hl, hl
    add hl, hl
    add hl, de
    add hl, hl
    add hl, hl
    add hl, hl    
    add hl, de    
    ld (seed), hl      
    ld a, h
    and $06
    add a, (tablestartlow)
    ld l, a
    ld h, (tablestarthigh)    
    ld e, (hl)
    inc hl
    ld d,(hl)
    ld (direction), de
    pop hl
    
    ld a, c
    cp $08
    jr nz, moveball

    ld hl, (D_FILE)    ; increase score       
    ld de, $001f    ; position of score in boarder?
    add hl, de
carry:
    ld a, (hl)
    cp $80
    jr nz, digit
    ld a, $9c
digit:
    inc a
    cp $a6
    jr nz, increased
    ld (hl), $9c
    dec hl
    jr carry
increased:
    ld (hl), a           

    
movebat:    
    push bc
    call kscan    ;;; was on page 95 of book :)
    pop bc
    ld a, l
    cpl
    push af
    and $0f
    jr z, notleft
    ld hl, (batpos)
    dec hl
    dec hl
    dec hl    
    ld a, (hl)
    cp $80
    jr z, cycle1
    ld (hl), $03
    inc hl
    inc hl
    ld (batpos), hl
    inc hl
    inc hl
    inc hl
    ld (hl), 00        
notleft:    
    pop af
    and $f0
    jr z, cycle2
    ld hl, (batpos)
    inc hl
    inc hl
    inc hl
    ld a, (hl)
    cp $80
    jr z, cycle2
    ld (hl), $03
    dec hl
    dec hl
    ld (batpos), hl
    dec hl
    dec hl
    dec hl
    ld (hl), 00
    push hl
cycle1:
    pop hl
cycle2:
    jp loop  ;; just until we've typed in the rest of the code

	ret  ;;; never gets here


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
#include "screenFull.asm"      			; definition of the screen memory, in colapsed version for 1K        
#include "endbasic.asm"
