;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;;; Breakout game from the book Mastering Machine code on your zx81 1981 - by Toni Baker
;;; typed in here from the assembly, rather than machine code directly on ZX81 would have used HEXLD3.
;;; 
;;; Typed in and ammended by AdrianPilko(ByteForever) December 2023
;;; The code heavily!! dependant on the definition of the screen memory in screen.asm
;;;;;;;;;;;;;;;;;;;;;

;; keys are any left side of keyboard moves left and any right side moves right

;; my own changes from the book type-in version:
;;      - corrected the errors in the next "ballpos" logic, more verbose now but works 
;;      - added "lives" count down
;;      - added more blocks to remove
;;      - added game over message and full restart when lives reach zero 
;;      - added "spin" to ball if bat was moving - this enables a certain amount of random
;;        which prevents the comon problem with breakout where it gets into a stable state and
;;        no more bricks can be removed
;; known bugs
;;      - sometime when the ball is lost in bottom left of screen no restart happens
;;      - seen a few times - ball went through bat when bat was moving continuously before hit!
;; todo
;;      - would be nice to have a high score

;#define DEBUG_MOVEBALL_EVERYTIME
;#define DEBUG_PRINT
;#define DEBUG_PRINT_DEBUG_MESSAGE
;#define DEBUG_PRINT_SPIN_MESSAGE
;#define DEBUG_START_BALL_TOP
;#define DEBUG_SLOW
;#define LIVES_1
CLS EQU $0A2A
PRINTAT EQU $08f5
PRINT EQU $10

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt
_SD:			EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F


;;;; this is the whole ZX81 runtime system and gets assembled and
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address

VERSN
    DB 0
E_PPC:
    DW 2
D_FILE:
    DW Display
DF_CC:
    DW Display+1                  ; First character of display
VARS:
    DW Variables
DEST:           DW 0
E_LINE:         DW BasicEnd
CH_ADD:         DW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DW 0
STKBOT:         DW BasicEnd+5
STKEND:         DW BasicEnd+5                 ; Empty stack
BREG:           DB 0
MEM:            DW MEMBOT
UNUSED1:        DB 0
DF_SZ:          DB 2
S_TOP:          DW $0002                      ; Top program line number
LAST_K:         DW $fdbf
DEBOUN:         DB 15
MARGIN:         DB 55
NXTLIN:         DW Line2                      ; Next line address
OLDPPC:         DW 0
FLAGX:          DB 0
STRLEN:         DW 0
T_ADDR:         DW $0c8d
SEED:           DW 0
FRAMES:         DW $f5a3
COORDS:         DW 0
PR_CC:          DB $bc
S_POSN:         DW $1821
CDFLAG:         DB $40
MEMBOT:         DB 0,0 ;  zeros
UNUNSED2:       DW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DB $00,$0a                    ; Line 10
                DW Line1End-Line1Text         ; Line 10 length
Line1Text:      DB $ea                        ; REM




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
;#ifdef DEBUG_START_BALL_TOP        
;    ;ld de, $4a   ; for debug put it above top to test bounce3 of top wall
;    ld de, $66   ; for debug put it above top to test bounce3 of top wall
;    ld hl, (D_FILE) ; for debug put it above top to test bounce3 of top wall
;#endif    
    add hl, de
    ld (ballinit), hl
;#ifdef DEBUG_SLOW
;    ld hl, $ff00       ;; the delay loops for debug slower
;#else    
    ;ld hl, $03f0       ;; the delay loops
    ld hl, $04f0
;#endif
    ld (speed), hl
    
    ld hl, (D_FILE)
    inc hl
    ld de, 98
    
    add hl, de
    ld (topRow), hl
    
;#ifdef LIVES_1   ; to test for end of game
;    ld a, $01
;#else    
    ld a, $03    ; 3 lines bit mean but hey it's not meant to be easy, right ?? :)
;#endif    
    daa
    ld (lives), a   
    ld c, 6
    ld b, 0        
    call PRINTAT
    ld a, (lives)
    call hprintInverse
   
    ld bc,0
	ld de,top_row_text_lives
	call printstring	    
    
    ld bc,9
	ld de,top_row_text_high_score    
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
    ld c, 6
    ld b, 0        
    call PRINTAT
    ld a, (lives)
    call hprintInverse
    
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
;#ifdef DEBUG_PRINT    
;    ld de,  $0296
;#else    
    ld de,  $02b7   ;; to allow for debug print raise by one line
;#endif
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
;#ifdef DEBUG_MOVEBALL_EVERYTIME
    ;; move ball every time!
;#else
    inc b    
    bit $00, b            ; only move ball every other time
    jp nz, movebat
;#endif    
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
    ;push hl    

    ld a, c
;#ifdef DEBUG_PRINT        
    ;call debugPrintRegisters
;#endif        
    cp $80                  ; check if the next position is a the wall
    jp nz, checkIfNextIsBat
    
    ;; check if this is the "top wall"
    ld hl,(ballpos)  ; Load the first 16-bit value into HL
    ld de,(topRow)  ; Load the second 16-bit value into DE
;#ifdef DEBUG_PRINT    
 ;   call debugPrintRegisters
;#endif

    sbc hl, de
    jr c, isTopWall ; jump to 'action_x' if carry flag is set (hl < de)
   
    jp isSideWall
    
isSideWall:
    ld a, 1
    ld (wallFlag), a
    xor a
    ld (topWallFlag), a  
;#ifdef DEBUG_PRINT_DEBUG_MESSAGE    
;    call debugPrintWasSide
;#endif    
    jp checkDirectionChanges
isTopWall:    
    xor a
    ld (wallFlag), a
    ld a, 1 
    ld (topWallFlag), a  
;#ifdef DEBUG_PRINT_DEBUG_MESSAGE
;    call debugPrintWasTop
;#endif    

    jp checkDirectionChanges  


    
checkIfNextIsBat:    
    ld a, c
    cp $03                  ; check if the next position is a the bat
    jp nz, checkIfNextIsBrick

    ld a, 1
    ld (batHitFlag), a

;#ifdef DEBUG_PRINT_DEBUG_MESSAGE    
;    call debugPrintWasBat
;#endif    

    ; check if bat moved last time, if so add spin
    ld a, (batMoved)
    cp 2
    jp z, spinLeft
    cp 1
    jp z, spinRight
;#ifdef DEBUG_PRINT_SPIN_MESSAGE    
;    call debugPrintNoSpin    
;#endif DEBUG_PRINT_SPIN_MESSAGE        
    jp checkDirectionChanges    
    
spinLeft:    
    ld hl, (ballpos)
    dec hl
    ld (ballpos), hl
    
;#ifdef DEBUG_PRINT_SPIN_MESSAGE    
;    call debugPrintWasSpinLeft
;#endif DEBUG_PRINT_SPIN_MESSAGE            
    jp checkDirectionChanges

spinRight:
    ld hl, (ballpos)
    inc hl
    ld (ballpos), hl        
;#ifdef DEBUG_PRINT_SPIN_MESSAGE    
;    call debugPrintWasSpinRight    
;#endif DEBUG_PRINT_SPIN_MESSAGE                
    jp checkDirectionChanges
    
checkIfNextIsBrick:
    ld a, c
    cp $08                  ; check if the next position is brick
    jp nz, skipChangeDirection    

;#ifdef DEBUG_PRINT_DEBUG_MESSAGE    
;    call debugPrintWasBrick
;#endif        
    ld a, 0
    ld (batMoved), a

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
    ;pop hl
    
    ld a, c
    cp $08                  ; if the contents of the square is not a brick
                            ; then move again
                            
    jp nz, moveball

;; the score is stored and calculated in a clever way using the actual screen memory as the "variable"/digits
;; however this makes adding a high score more difficult
    ld hl, (D_FILE)    ; increase score       
    ld de, $001f       ; position 0 to 9 part of score in boarder
    add hl, de
carry:
    ld a, (hl)
    cp $80
    jr nz, digit
    ld a, $9c
digit:
    push af                 
    ld a, (copyOfScore) ;; added to get a score we can manipulate in normal numerical terms and create a high score from later
    inc a
    ld (copyOfScore), a
    pop af
    
    inc a
    cp $a6
    jr nz, increased
    ld (hl), $9c
    dec hl
    jr carry
increased:
    ld (hl), a           
        
    push bc
    ld c, 6
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
    ld a, 2 
    ld (batMoved), a
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
    ld a, 1
    ld (batMoved), a    
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
        
    ;; prints a:hl:de:bc:topRow::ballpos
    
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

debugPrintWasSide
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ld bc,760
	ld de,debug_side_text
	call printstring	    
    
    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret
    
debugPrintWasTop
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ld bc,760
	ld de,debug_top_text
	call printstring	    
    
    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret
    
debugPrintWasBat    
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ld bc,760
	ld de,debug_bat_text
	call printstring	    
    
    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret    

debugPrintWasBrick
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ld bc,760
	ld de,debug_brick_text
	call printstring	    
    
    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret    
    
debugPrintWasSpinLeft
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ld bc,760
	ld de,debug_ball_spin_lefttext
	call printstring	    
    
    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret    
    
debugPrintWasSpinRight
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ld bc,760
	ld de,debug_ball_spin_righttext
	call printstring	    
    
    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret    
    
debugPrintNoSpin    
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ld bc,760
	ld de,debug_no_spin_text
	call printstring	    
    
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
    
;INCLUDE commonUtils.asm

                DB $76                        ; Newline
Line1End
Line2			DB $00,$14
                DW Line2End-Line2Text
Line2Text     	DB $F9,$D4                    ; RAND USR
				DB $1D,$22,$21,$1D,$20        ; 16514
                DB $7E                        ; Number
                DB $8F,$01,$04,$00,$00        ; Numeric encoding
                DB $76                        ; Newline
Line2End
endBasic

Display        	DB $76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b,$76
                DB  0, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b,$76

Variables

mazeVisitedLocations
    DS 128, $1b
    DB $76
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
copyOfScore
    DEFW $0000   
lives   
    DEFB $00
top_row_text_lives
	DEFB	_L+128,_I+128,_V+128,_E+128,_S+128,$ff   ; the +128 makes it inverse video
top_row_text_score
	DEFB	_S+128,_C+128,_O+128,_R+128,_E+128,$ff  ; the +128 makes it inverse video
top_row_text_high_score
	DEFB	_H+128,_I+128,_G+128,_H+128,128,_S+128,_C+128,$ff   ; the +128 makes it inverse video

debug_side_text
    DEFB   _W,_A,_S,0,_S,_I,_D,_E,0,_W,_A,_L,_L,$ff
debug_top_text
    DEFB   _W,_A,_S,0,_T,_O,_P,0,_W,_A,_L,_L,0,0,$ff
debug_bat_text
    DEFB   _W,_A,_S,0,_B,_A,_T,0,0,0,0,0,0,0,$ff
debug_brick_text
    DEFB   _W,_A,_S,0,_B,_R,_I,_C,_K,0,0,0,0,0,$ff    
debug_ball_spin_lefttext
    DEFB   _S,_P,_I,_N,0,_L,_E,_F,_T,0,0,$ff        
debug_ball_spin_righttext
    DEFB   _S,_P,_I,_N,0,_R,_I,_G,_H,_T,$ff            
debug_no_spin_text
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,$ff            
game_over_text
    DEFB	_G+128,_A+128,_M+128,_E+128,128,_O+128, _V+128,_E+128,_R+128,$ff  ; the +128 makes it inverse video
game_over_blank_text
    DEFB	0,0,0,0,0,0,0,0,0,$ff  ; black blocks 
oneBytePaddingForAlignment    
    DEFB $00    
batMoved
    DEFB $00


VariablesEnd:   DB $80
BasicEnd:

