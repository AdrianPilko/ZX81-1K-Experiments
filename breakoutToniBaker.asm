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
tablestart:
dirTabDownLeft:
    DEFB $00   
    DEFB $20         ;; 31 to move ball down and to left
dirTabDownRight:    
    DEFB $00
    DEFB $22         ;; 33 to move ball down and to right
dirTabUpRight:
    DEFB $ff
    DEFB $e0         ;; -31 when taken as twos compliment up and right
dirTabUpLeft:    
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
upFlag:                 ;; if the ball is moving up == 1 else 0
    DEFB $01            ; default is ball moving up and to right
rightFlag:                 ;; if the ball is moving right == 1 else 0
    DEFB $01            ; default is ball moving up and to right
wallFlag    
    DEFB $00
    
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
    
    xor a
    ld (wallFlag), a
    ld a, $01               ; default ball move up and to right
    ld (upFlag), a
    ld a, $01
    ld (rightFlag), a    
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
    jp nz, movebat
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
    jp z, movebat
    push hl    

    ld a, c
    cp $80                  ; check if the next position is a the wall
    jp nz, checkIfNextIsBat
    ld a, $01
    ld (wallFlag), a
    jp checkDirectionChanges
    
checkIfNextIsBat:    
    ld a, c
    cp $03                  ; check if the next position is a the bat
    jp nz, checkIfNextIsBrick
    ;;pop hl  ;; debug
    ;;ret     ;; debug    
    jp checkDirectionChanges
    
checkIfNextIsBrick:
    ld a, c
    cp $08                  ; check if the next position is a the bat    
    jp nz, skipChangeDirection    
    
    ;;pop hl  ;; debug
    ;;ret     ;; debug
    
    jp checkDirectionChanges
    
;;; code to reverse directions (warning is a bit verbose!)
checkDirectionChanges:
    ; if upFlag==1 & rightFlag==1 then switchDownRight
    ld a, (upFlag)  
    cp $01
    jp nz, checkif_switchUpRight
    ld a, (rightFlag)    
    cp $01
    jp nz, checkif_switchUpRight
    
    ld a, (wallFlag)
    cp $01
    jp z, switchUpLeft
    
    jp switchDownRight
    
    ; if upFlag==0 & rightFlag==1 then switchUpRight
checkif_switchUpRight:    
    ld a, (upFlag)  
    cp $01
    jp z, checkif_switchDownLeft
    ld a, (rightFlag)    
    cp $01
    jp nz, checkif_switchDownLeft   
    jp switchUpRight
    
    ; if upFlag==1 & rightFlag==0 then switchDownLeft
checkif_switchDownLeft:
    ld a, (upFlag)  
    cp $01
    jp nz, checkif_switchUpLeft
    ld a, (rightFlag)    
    cp $01
    jp z, checkif_switchUpLeft
    
    ld a, (wallFlag)
    cp $01
    jp z, switchUpRight

    jp switchDownLeft

    ; if upFlag==0 & rightFlag==0 then switchUpLeft        
checkif_switchUpLeft:        
;; no check as is last option
    jp switchUpLeft
    
    
switchUpLeft:
    ld a, (dirTabUpLeft)
    ld h, a
    ld a, (dirTabUpLeft+1)
    ld l, a  
    
    ld a, $01
    ld (upFlag), a
    xor a
    ld (rightFlag), a    
    ld (wallFlag), a
    jp skipChangeDirection
    
    
switchDownLeft:
    ld a, (dirTabDownLeft)
    ld h, a
    ld a, (dirTabDownLeft+1)
    ld l, a

    xor a
    ld (upFlag), a
    ld (rightFlag), a            
    ld (wallFlag), a
    
    jp skipChangeDirection
switchUpRight:
    ld a, (dirTabUpRight)
    ld h, a
    ld a, (dirTabUpRight+1)
    ld l, a    

    ld a, $01
    ld (upFlag), a
    ld (rightFlag), a            
    xor a
    ld (wallFlag), a
    jp skipChangeDirection
switchDownRight:
    ld a, (dirTabDownRight)
    ld h, a
    ld a, (dirTabDownRight+1)
    ld l, a
    
    xor a
    ld (upFlag), a    
    ld a, $01    
    ld (rightFlag), a            
    xor a
    ld (wallFlag), a
    jp skipChangeDirection
    
skipChangeDirection:

    ld (direction), hl; 
    pop hl
    
    ld a, c
    cp $08                  ; if the contents of the square is not a brick
                            ; then move again
    jp nz, moveball

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
