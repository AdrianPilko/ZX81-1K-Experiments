;;;;;;;;;;;;;;;;;;;;;
;;; Breakout game from the book Mastering Machine code on your zx81 1981 - by Toni Baker
;;; typed in here from the assembly, rather than machine code directly on ZX81 would have used HEXLD3.
;;; 
;;; Typed in and ammended by AdrianPilko(ByteForever) October 2023
;;; The code heavily!! dependant on the definition of the screen memory in screen.asm
;;;;;;;;;;;;;;;;;;;;;

;; my own changes from the book type-in version:
;;      - corrected the errors in the next "ballpos" logic, more verbose now but works 
;;      - added "lives" count down
;;      - added more blocks to remove
;;      - added game over message and full restart when lives reach zero 
;;
;; known bugs
;;      - sometimes, especially when hitting the upper right wall the ball disappera off into oblivion
;;        and the game craches
;;      - sometime when the ball is lost in bottom left of screen no restart happens
;; todo
;;      - would be better for the bat to impart "spin" on ball to prevent "checkerboard" effect
;;        The book code handles this by addin one to ballinit after each life lost
;;      - would be nice to have a high score


;#define DEBUG_PRINT
;#define DEBUG_START_BALL_TOP
;#define DEBUG_SLOW
;#define LIVES_1

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"
#include "line1.asm"
    jp breakout
    
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
    ld bc,$8080   ; b is number of bricks, c is 128 (0x80) which is black square for sides and top boarder    
nxbrk2:
    inc hl
    ld a,(hl)
    cp $76
    jr z, nxbrk2
    ld (hl), $08
    djnz nxbrk2   
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
    ld hl, (D_FILE)  ; initialise the top part of boarder    
    ld de, $22
    add hl, de    
    ld b,$1f
nxbl2:
    ld (hl), c
    inc hl
    djnz nxbl2
    ld (hl), c   ; last wall block on second row
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
      
    ld de, $fefc   ;; this only works because of the last value of hl from previous loop
#ifdef DEBUG_START_BALL_TOP        
    ld de, $4a   ; for debug put it above top to test bounce3 of top wall
    ld hl, (D_FILE) ; for debug put it above top to test bounce3 of top wall
#endif    
    add hl, de
    ld (ballinit), hl
#ifdef DEBUG_SLOW
    ld hl, $0e00       ;; the delay loops for debug slower
#else    
    ;ld hl, $03f0       ;; the delay loops
    ld hl, $04f0
#endif
    ld (speed), hl
    
    ld hl, (D_FILE)
    ld de, $1e
    
    add hl, de
    ld (topRow), hl
    
#ifdef LIVES_1   ; to test for end of game
    ld a, $01
#else    
    ld a, $03    ; 3 lines bit mean but hey it's not meant to be easy, right ?? :)
#endif    
    daa
    ld (lives), a   
    ld c, 7
    ld b, 0        
    call PRINTAT
    ld a, (lives)
    call hprintInverse
   
    ld bc,0
	ld de,top_row_text_lives
	call printstring	    
    ld bc,22
	ld de,top_row_text_score
	call printstring	   
    
    jp first_time ;; don't dec lives first time through
    
restart:
    
    ld a, (lives)   
    dec a
    cp 0
    jp z, gameover   ; do full restart (ie game over
    daa
    ld (lives), a
    ld c, 7
    ld b, 0        
    call PRINTAT
    ld a, (lives)
    call hprint       
    
first_time:    

    ld hl, (ballinit)
    ;inc hl           ;; instead of this sneaky inc to stop repeating checkerboard could  bat  
                     ;; use direction of later to kick ball left/right by 2
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
#ifdef DEBUG_PRINT    
    ld de,  $0296
#else    
    ld de,  $02b7   ;; to allow for debug print raise by one line
#endif
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
    jp z, restart
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
#ifdef DEBUG_PRINT        
    call debugPrintRegisters
#endif        
    cp $80                  ; check if the next position is a the wall
    jp nz, checkIfNextIsBat
    
    ;; check if this is the "top wall"
    ld hl,(ballpos)  ; Load the first 16-bit value into HL
    ld de,(topRow)  ; Load the second 16-bit value into DE
#ifdef DEBUG_PRINT    
    call debugPrintRegisters
#endif

    ld a, h    ; Load the high byte of HL into the accumulator
#ifdef DEBUG_PRINT    
    call debugPrintRegisters
#endif    
    sub d      ; Subtract the high byte of DE from the accumulator
    jr nz, notTopWall  ; Jump if no carry (HL >= DE)

    ; If there was a carry, the high byte of HL is less than DE
    ; Now, compare the low bytes (least significant bytes)
    ld a, l    ; load the low byte of hl into the accumulator
#ifdef DEBUG_PRINT    
    call debugPrintRegisters
#endif        
    sub e      ; subtract the low byte of de from the accumulator
    jr z, notTopWall  ; jump if no carry (hl >= de)

less_than:
    ; Your code to handle the case where HL is less than DE        
    ld a, $01    
    ld (topWallFlag), a
    jp checkDirectionChanges  
notTopWall:    
    ld a, $01
    ld (wallFlag), a
    jp checkDirectionChanges

    
checkIfNextIsBat:    
    ld a, c
    cp $03                  ; check if the next position is a the bat
    jp nz, checkIfNextIsBrick

    ld a, 1
    ld (batHitFlag), a
    jp checkDirectionChanges
    
checkIfNextIsBrick:
    ld a, c
#ifdef DEBUG_PRINT    
    call debugPrintRegisters
#endif
    
    cp $08                  ; check if the next position is a the bat    
    jp nz, skipChangeDirection    
    
#ifdef DEBUG_PRINT    
    call debugPrintRegisters
#endif    
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
    
    
    ;; need to check topWallFlag    
    ld a, (topWallFlag)
    cp $01    
    jp z, switchDownRight
    
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
    
    ld a, (wallFlag)
    cp $01
    jp z, switchDownLeft
    
    jp switchUpRight
    
    ; if upFlag==1 & rightFlag==0 then switchDownLeft
checkif_switchDownLeft:
    ld a, (upFlag)  
    cp $01
    jp nz, checkif_switchUpLeft
    ld a, (rightFlag)    
    cp $01
    jp z, checkif_switchUpLeft

    ;; need to check topWallFlag    
    ld a, (topWallFlag)
    cp $01    
    jp z, switchDownRight
        
    ld a, (wallFlag)
    cp $01
    jp z, switchUpRight

    jp switchDownLeft

    ; if upFlag==0 & rightFlag==0 then switchUpLeft        
checkif_switchUpLeft:        
;; now only check for wall
    ld a, (wallFlag)
    cp $01
    jp z, switchDownRight

    jp switchUpLeft
    
    
switchUpLeft:
    ld a,  (batHitFlag)
    cp 1
    jp nz, afterSpinLeft
    ld hl,(ballpos)
    dec hl
    ld (ballpos), hl
afterSpinLeft:      

    ld a, (dirTabUpLeft)
    ld h, a
    ld a, (dirTabUpLeft+1)
    ld l, a  

    ld a, $01
    ld (upFlag), a
    xor a
    ld (rightFlag), a    
    ld (wallFlag), a
    ld (topWallFlag), a
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
    ld (topWallFlag), a
    jp skipChangeDirection
switchUpRight:

    ld a,  (batHitFlag)
    cp 1
    jp nz, afterSpinRight
    ld hl,(ballpos)
    inc hl
    ld (ballpos), hl
afterSpinRight:    
    ld a, (dirTabUpRight)
    ld h, a
    ld a, (dirTabUpRight+1)
    ld l, a    

    ld a, $01
    ld (upFlag), a
    ld (rightFlag), a            
    xor a
    ld (wallFlag), a
    ld (topWallFlag), a
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
    ld (topWallFlag), a
    jp skipChangeDirection
    
    
skipChangeDirection:
    xor a   ; clear bat hit flag 
    ld (batHitFlag), a
    
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
        
    push bc
    ld c, 7
    ld b, 0        
    call PRINTAT
    ld a, (lives)
    call hprintInverse
    pop bc

    
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
    
gameover:
    ld b,$ff
gameOverDelay:    
    push bc
    ld bc,540
	ld de,game_over_text
	call printstring	        
    ld b, $ff
gameOverInnerDelay:
    xor a
    djnz gameOverInnerDelay
    ld bc,540
	ld de, game_over_blank_text    
	call printstring	        
    
    pop bc    
    djnz gameOverDelay
    
    ld bc,338
	ld de, game_over_blank_text    
	call printstring	        
    
    jp breakout
        
    
    
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
    ld hl, (topRow) 
    ld a, h
    call hprint    
    ld a, l
    call hprint      
    ld a, 14
    call PRINT

    ld hl, (ballpos) 
    ld a, h
    call hprint    
    ld a, l
    call hprint    
    ld a, 14
    call PRINT

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
topWallFlag
    DEFW $0000
batHitFlag
    DEFB $00
topRow    
    DEFW $0000
lives   
    DEFB $00
top_row_text_lives
	DEFB	_L+128,_I+128,_V+128,_E+128,_S+128,$ff   ; the +128 makes it inverse video
top_row_text_score
	DEFB	_S+128,_C+128,_O+128,_R+128,_E+128,$ff  ; the +128 makes it inverse video
game_over_text
    DEFB	_G+128,_A+128,_M+128,_E+128,128,_O+128, _V+128,_E+128,_R+128,$ff  ; the +128 makes it inverse video
game_over_blank_text
    DEFB	0,0,0,0,0,0,0,0,0,$ff  ; black blocks 
#include "screenFull.asm" 
;;; ball "directions", used to add or subract ball position to move diagonally down left or right (tablestartlow) then up left right - these are offsets which with the code to moveball causes the ball to move in screen memory
#include "endbasic.asm"
