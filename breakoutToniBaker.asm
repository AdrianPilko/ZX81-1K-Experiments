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
    DEFW $0020, $0022
    DEFW $ffe0, $ffde
ballinit:
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
speed:     
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
ballpos:
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
direction:
    DEFW $0000         ;; these were just addresses in the machine code, here define a label
    
    
breakout:
    ld hl,(D_FILE)
    ld de,$0084   ;; increased by one from book listing (bug?)
    add hl,de
    ld bc,$8080   
nxbrk:
    inc hl
    ld a,(hl)
    cp $76
    jr z, nxbrk
    ld (hl), $08
    djnz nxbrk
    ld hl, (D_FILE)
    ld b,$1e
nxbl:
    inc hl
    ld (hl), c
    djnz nxbl
    inc hl
    ld (hl), $9c
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
	ret

#include "line2.asm"
#include "screenFull.asm"      			; definition of the screen memory, in colapsed version for 1K        
#include "endbasic.asm"
