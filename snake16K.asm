;;;;;;;;;;;;;;;;;;;;;
;;; zx81 16K code by Adrian Pilkington (orginally 2022) updated later as well!
;;; It's a clone of snake (in case that wasn't clear from filename;)
;;;;;;;;;;;;;;;;;;;;;
;; see demo video at https://youtu.be/k7EiBF07xOs?si=e3_hom0M1qUv2a9q
;; download free from itch.io https://adrianpilko.itch.io/zx81-snake-game

; KNOWN BUGS /  FEATURES;       
;       1) when the snake gets longer than about 70 (unsure exact number) after game over the
;          title screen is corrupted and then pressing s to start causes screen corruption
;          and the game never starts - must be a memory overrite somewhere :-///
;       2) no check of the new food appearing in any of the snake body coordinates
;          which is why have to add two new food each time. To correct this would have to check
;          entire length of snake for conflicts, is doable but would slow code down for long snakes
;       3) no joystick support - would improve the game running on real ZX81, is ok in emulator
;       4) no key redefine


; to enable "halt" for debug uncomment next 4 rows
;    di              ; disable interrupts
;    LD A,0          ; disable NMI for DEBUG only
;    OUT ($FD),A     ; disable NMI for DEBUG only
;here__    
;    jp here__

;  all the includes for base zx81 "memory image" came from  
;  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm" 
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"
#include "line1.asm"

;;; #define FULL_DEBUG  ;;; comment out if no debug then rebuild


#define KEYBOARD_READ_PORT_P_TO_Y	$DF
; for start key 
#define KEYBOARD_READ_PORT_A_TO_G	$FD
; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; keyboard q to t
#define KEYBOARD_READ_PORT_Q_TO_T $FB
; keyboard read port 
#define KEYBOARD_READ_PORT $FE 

;#define SHAPE_CHAR_SNAKE   151        ; black square

;#define SHAPE_CHAR_SNAKE_UP   3        
;#define SHAPE_CHAR_SNAKE_DOWN   131    
;#define SHAPE_CHAR_SNAKE_LEFT   5      
;##define SHAPE_CHAR_SNAKE_RIGHT   133   

#ifdef FULL_DEBUG
#define SHAPE_CHAR_SNAKE_UP   156
#define SHAPE_CHAR_SNAKE_DOWN   157    
#define SHAPE_CHAR_SNAKE_LEFT   158      
#define SHAPE_CHAR_SNAKE_RIGHT   159   
#else
#define SHAPE_CHAR_SNAKE_UP   151
#define SHAPE_CHAR_SNAKE_DOWN   151    
#define SHAPE_CHAR_SNAKE_LEFT   151      
#define SHAPE_CHAR_SNAKE_RIGHT   151   
#endif

#define SHAPE_CHAR_FOOD 136
#define SHAPE_CHAR_WALL 189
#define SHAPE_CHAR_NO_PRINT   0        ; black square
#define SNAKE_MOVEMENT_LEFT 1
#define SNAKE_MOVEMENT_RIGHT 2
#define SNAKE_MOVEMENT_UP 3
#define SNAKE_MOVEMENT_DOWN 4

#define SNAKE_MAX_LENGTH 250  

VSYNCLOOP     .equ      3

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
	call drawInitialScreen  ; clears screen and sets the boarder
    
	ld bc,111
	ld de,title_screen_txt
	call printstring
	ld bc,202    
	ld de,keys_screen_txt_1
	call printstring		
    
    

	ld bc,246
	ld de,keys_screen_txt_2
	call printstring		
    ld bc,280
	ld de,keys_screen_txt_3
	call printstring
    
	ld bc,337
	ld de,game_objective_txt
	call printstring	
	ld bc,436
	ld de,last_Score_txt
	call printstring	
	
    ld bc, 476
    ld de, last_score_mem_hund ; load address of hundreds
	call printNumber    
	ld bc, 478			; bc is offset from start of display
	ld de, last_score_mem_tens ; load address of  tens		
	call printNumber	
	ld bc,537	
	ld de,credits_and_version_1
	call printstring		
	ld bc,569	
	ld de,credits_and_version_2
	call printstring	
    
    ld bc,632
	ld de,TextForGameJam1
    call printstring	
    ld bc,665
	ld de,TextForGameJam2
    call printstring	
    ld bc,698
	ld de,TextForGameJam3
    call printstring	
	
read_start_key
	ld a, KEYBOARD_READ_PORT_A_TO_G	
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop
    ;; else drop into initVariables

initVariables    
    call drawInitialScreen
    call drawAdditionalWalls
    
    ld bc,3
    ld de,scoreText   
    call printstring	
    
    xor a           ; xor a is quickest way to zero a
    ld (score_mem_tens),a
	ld (score_mem_hund),a       
    
    ld bc, 12
    ld de, score_mem_tens
    call printNumber
    ld bc, 10
    ld de, score_mem_hund
    call printNumber    
    
    
    ld a, 2
    ld (snakeTailIndex), a    
    ld (foodCount), a
    daa
    ;;ld (snakeLength), a
    
    ld hl, $04ff
    ld (waitSpeed), hl
    

    ld a, 10                    ; bit wasteful of memory, should put in loop?!
    ld (snakeCoordsRow), a      
    inc a
    ld (snakeCoordsRow+1), a    
    xor a        
    ld (snakeCoordsRow+2), a    
    ld (snakeCoordsRow+3), a    
    ld (snakeCoordsRow+4), a    
    ld (snakeCoordsRow+5), a 
    ld (snakeCoordsRow+6), a 
    ld (snakeCoordsRow+7), a 
    ld (snakeCoordsRow+8), a 
    
    ld a, 15			  
    ld (snakeCoordsCol), a
    ld (snakeCoordsCol+1), a    
    xor a        
    ld (snakeCoordsCol+2), a
    ld (snakeCoordsCol+3), a
    ld (snakeCoordsCol+4), a    
    ld (snakeCoordsCol+5), a
    ld (snakeCoordsCol+6), a
    ld (snakeCoordsCol+7), a
    ld (snakeCoordsCol+8), a
    
    ; default movement is right
    ld a, SNAKE_MOVEMENT_RIGHT
    ld (snakeMovementFlags), a
    ld (snakeMovementFlags+1), a
    xor a    
    ld (snakeMovementFlags+2), a
    ld (snakeMovementFlags+3), a
    ld (snakeMovementFlags+4), a    
    ld (snakeMovementFlags+5), a
    ld (snakeMovementFlags+6), a
    ld (snakeMovementFlags+7), a
    ld (snakeMovementFlags+8), a
    
    ld a,SHAPE_CHAR_SNAKE_RIGHT
    ld (snakeCharsToPrint), a    
    ld (snakeCharsToPrint+1), a    
    xor a    
    ld (snakeCharsToPrint+2), a    
    ld (snakeCharsToPrint+3), a
    ld (snakeCharsToPrint+4), a    
    ld (snakeCharsToPrint+5), a
    ld (snakeCharsToPrint+6), a
    ld (snakeCharsToPrint+7), a
  
    
    ld a, SNAKE_MOVEMENT_RIGHT
    ld (movedFlag),a
    
    ld hl, Display
    ld de, 346
    add hl, de
    ld (absoluteScreenMemoryPosition), hl    
    ld hl, (absoluteScreenMemoryPosition)
    ld (hl), SHAPE_CHAR_SNAKE_RIGHT        ; draw inital snake
    ;dec hl    
    ;ld (hl), SHAPE_CHAR_SNAKE_RIGHT        ; draw inital snake
    ;dec hl
    ;ld (hl), SHAPE_CHAR_SNAKE_RIGHT        ; draw inital snake

    ; default two more food so is always 4 on screen at any one time
    call setRandomFood
    call setRandomFood 
    call setRandomFood
    call setRandomFood
    call setRandomFood
    
main

	ld b,VSYNCLOOP
waitForTVSync	
	call vsync
	djnz waitForTVSync


    ld bc, 20
    ; copy snake length
    ld a, (snakeTailIndex)

; keyboard layout for reading keys on ZX81
; BIT   left block      right block  BIT
; off                                off in <port>, when ld a, <port>
;       0  1 2 3 4     4 3 2 1 0                 <<< bit to check for each column after in a, $fe 
; 3   ( 1  2 3 4 5 ) ( 6 7 8 9 0 )     4
; 2   ( Q  W E R T ) ( Y U I O P )     5
; 1   ( A  S D F G ) ( H I K L n/l)    6
; 0   (sft Z X C V ) ( B N M . spc)    7
;
; to read keys 1 2 3 4 5
; set all bits except bit 3 of register A = 1 1 1 1 0 1 1 1= f7, then execute in a, $fe  (fe is the "keyboard read port")
; now register a will contain a bit pattern to check for which key in that block was set, eg Key "1" = bit 0 of a
; ld a, $f7    
; in a, $fe    
; similarly for the rest, to read from block A S D F G, set a to 1 1 1 1 1 1 1 0 1 = $fd


    ; read the keyboard input and adust the offset     
    
    ; check for pause key pressed "space"    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 0, a                            ; "SPACE"
    jp z, goIntoPause           ; only returns from pause when p is pressed again
afterPause    
    
    ld a, (movedFlag)   ;  check that we don't double back on ourselves!
    cp SNAKE_MOVEMENT_RIGHT
    jp z, skipCheckKeyLeft
    ld a, KEYBOARD_READ_PORT_P_TO_Y			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a                            ; O
    jp z, drawLeft								
skipCheckKeyLeft  
    ld a, (movedFlag)   ;  check that we don't double back on ourselves!
    cp SNAKE_MOVEMENT_LEFT
    jp z, skipCheckKeyRight
    ld a, KEYBOARD_READ_PORT_P_TO_Y			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; P
    jp z, drawRight							    ; jump to move shape right	    

skipCheckKeyRight    
    ld a, (movedFlag)   ;  check that we don't double back on ourselves!
    cp SNAKE_MOVEMENT_DOWN
    jp z, skipCheckKeyUp    
    ld a, KEYBOARD_READ_PORT_Q_TO_T			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a					        ; Q
    jp z, drawUp    
skipCheckKeyUp  
    ld a, (movedFlag)   ;  check that we don't double back on ourselves!
    cp SNAKE_MOVEMENT_UP
    jp z, skipCheckKeyDown  
    ld a, KEYBOARD_READ_PORT_A_TO_G   ; $fd
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a					        ; A
    jp z, drawDown
skipCheckKeyDown

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
     
    jp drawBlock
drawLeft   

    ;; set the character to draw at this position
    ld  a, SHAPE_CHAR_SNAKE_LEFT
    ld (shapeSet),a
    

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

    ;; set the character to draw at this position
    ld  a, SHAPE_CHAR_SNAKE_RIGHT
    ld (shapeSet),a
    
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

    ;; set the character to draw at this position
    ld  a, SHAPE_CHAR_SNAKE_UP
    ld (shapeSet),a
    
    ld a, SNAKE_MOVEMENT_UP
    ld (movedFlag), a
    ld a, (snakeCoordsRow)    
    dec a    
    cp 0    
    jp z, drawBlock
    call shuffleSnakeInRow
    ld hl, (absoluteScreenMemoryPosition)    
    ld de, 33    
    sbc hl,de
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock
drawDown
    ;; set the character to draw at this position
    ld  a, SHAPE_CHAR_SNAKE_DOWN
    ld (shapeSet),a
    ld a, SNAKE_MOVEMENT_DOWN
    ld (movedFlag), a 
    ld a, (snakeCoordsRow)
    inc a
    cp 22
    jp z, drawBlock    
    call shuffleSnakeInRow
    ld hl, (absoluteScreenMemoryPosition)
    ld de, 33
    add hl, de
    ld (absoluteScreenMemoryPosition), hl    
    jp drawBlock
       
drawBlock
    ; we need to check cursor position we've moved to for and existing block 
    ; not on first time throught though when not moved   
    ld a, (movedFlag)
    cp 0
    jp z, noCheck
      
    ld a, (shapeSet)    
    ld b, a
    ld hl, (absoluteScreenMemoryPosition)
    ld a, (hl)        
    sub b   
    
#ifndef FULL_DEBUG    
    ; while debugging disable collision with snake and walls
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jp z, gameOver    
   
    xor a           ; zero a and clear flags
    ld a, (hl)    
    sub SHAPE_CHAR_WALL
    jp z, gameOver
#endif 
   
    xor a           ; zero a and clear flags
    ld a, (hl)
    sub SHAPE_CHAR_FOOD
    jp nz, noCheck
    
    ;;;; we've hit food, great!
    
    ;check snake length not at maximum, if so limit
    ld a, (snakeTailIndex)
    cp SNAKE_MAX_LENGTH
    jp z, noCheck 
       
    ld a, (foodCount)    
    dec a    
    ld (foodCount), a
    cp 2
    jp z, skipAddingFood
    
    inc a
    inc a
    ld (foodCount), a   ; save the "food" on screen
    call setRandomFood ; generate more food
    call setRandomFood ; generate more food
    call setRandomFood ; generate more food
    
skipAddingFood
    
    ld hl, (waitSpeed)  ; speed up by one now we have got some more food
    dec hl              
    ld (waitSpeed), hl
    
    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
	add a,1	
	daa									; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_tens),a	
	cp 153
	jr z, addOneToHund
	jr skipAddHund
addOneToHund
	ld a, 0
	ld (score_mem_tens), a
    ld a, (score_mem_hund)
	add a, 1
	daa
	ld (score_mem_hund), a
skipAddHund	

    ld bc, 12
    ld de, score_mem_tens
    call printNumber
    ld bc, 10
    ld de, score_mem_hund
    call printNumber    
    
    ;;call debugPrintRegisters
    
    ; we got the food, so increase length of the snake...    
    ; ...but we also need to set the new coordinates for the tail based on the direction
    ; if moving down new tail is same col, last tail row-1
    ; if moving up new tail is same col, last tail row+1
    ; if moving left new tail is same row last tail col+1
    ; if moving right new tail is same row, last tail col-1    

    ;; the new coordinate  doesn't depend on the current movement, but the 
    ;; direction the previous tail position was moving in    
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeMovementFlags                       
    ld d, 0
    ld e, b
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
    
    ret   ; should never get here as always moving
    ;jp noCheck  
    
newcorrdForLeft
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCoordsRow                       
    ld d, 0
    ld e, b
    add hl, de    
    ld a, (hl)
    inc hl  ; to push coord on one
    ld (hl), a      ; make new coord same as previous

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
    inc a           ; going left new coord is current tail plus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous    
    
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCharsToPrint
    ld d, 0
    ld e, b
    add hl, de        
    ld a,  SHAPE_CHAR_SNAKE_LEFT
    ld (hl), a      
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
    dec a           ; going right new coord is current tail minus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous
    dec a           ; going right new coord is current tail minus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous    

    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCharsToPrint
    ld d, 0
    ld e, b
    add hl, de        
    ld a,  SHAPE_CHAR_SNAKE_RIGHT
    ld (hl), a      
    
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
    inc a           ; going up new row coord is current tail plus one
    inc hl          ; to push coord on one
    ld (hl), a      ; make new coord smae as previous    

    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCharsToPrint
    ld d, 0
    ld e, b
    add hl, de        
    ld a,  SHAPE_CHAR_SNAKE_UP
    ld (hl), a          
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

    dec a           ; going down new row coord is current tail minus one
    inc hl          ; to push coord on one    
    ld (hl), a      ; make new coord smae as previous    
    
    ld a, (snakeTailIndex)        
    ld b, a     ; for some reason get a MS byte not used unless you go via a ??      
    ld hl, snakeCharsToPrint
    ld d, 0
    ld e, b
    add hl, de        
    ld a,  SHAPE_CHAR_SNAKE_DOWN
    ld (hl), a          
    
    
afterCoordAdd    
    ld a, (snakeTailIndex)
    inc a
    ld (snakeTailIndex), a
  
    ; drop through

noCheck  

    ;; here we're wiping the tail
    ld hl, snakeCoordsRow
    ld b, 0
    ld a, (snakeTailIndex) 
    ld c, a
    add hl, bc    
    
    ld a, (hl)
    ld h, a				    ; row set for PRINTAT
    cp 0
    jp nz, noErrorRow
    ret         ; the column should never be zero
noErrorRow    
    
    push hl
    ld hl, snakeCoordsCol
    ld b, 0
    ld a, (snakeTailIndex) 
    ld c, a
    add hl, bc
    ld a, (hl)
    pop hl
    ld l, a				    ; column set for PRINTAT
    cp 0
    jp nz, noErrorCol
    ret         ; the column should never be zero
noErrorCol    
    push hl  ; push hl to get into bc via the pop, why is ld bc, hl not an instruction? who am I to question :)
    pop bc
    
    ;call debugPrintRegisters
    
    push hl   ; save registers thinking is it's the bug that drops print at down one
    push de
    push af    
    push bc   
    call PRINTAT		; ROM routine to set current cursor position, from row b and column c    
    pop bc
    pop af
    pop de
    pop hl        
    
    ld a, SHAPE_CHAR_NO_PRINT
    call PRINT     
    
NOwipeLastTailPreviousPos        
    ld a, (snakeCoordsRow)   
	ld h, a				    ; row set for PRINTAT
    ld a, (snakeCoordsCol)
    ld l, a				    ; column set for PRINTAT
    
    push hl  ; push hl to get into bc via the pop, why is ld bc, hl not an instruction? who am I to question :)
    pop bc


    push hl   ; save registers thinking is it's the bug that drops print at down one
    push de
    push af    
    push bc   
    call PRINTAT		; ROM routine to set current cursor position, from row b and column c     
    pop bc
    pop af
    pop de
    pop hl  

    
    
    ;;;;; DRAW THE NEW HEAD OF THE SNAKE
    ld a, (shapeSet)
    call PRINT 
  
    ;ld hl, $4fff
    ;ld hl, $ffff
;;    ld hl, (waitSpeed)
    ;;ld hl, $ffff
;;    push hl
;;    pop bc
  
;;waitloop
  ;;  dec bc
;;    ld a,b
    ;;or c
    ;;jr nz, waitloop    
    jp main
  
gameOver
    ld bc,342
    ld de,game_over_txt   
    call printstring	

	ld a, (score_mem_tens) ; load tens into last score		
	ld (last_score_mem_tens),a 
	ld a, (score_mem_hund) ; load hundreds into last score	
	ld (last_score_mem_hund),a	
    
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
    ld (hl), SHAPE_CHAR_WALL
    push de
    ld de, 726
    add hl, de    
    ld (hl), SHAPE_CHAR_WALL   
    pop de
    inc de
    ld a, 32
    cp e
    jp nz, drawLineZeroAndLast

    ld b, 22            ;; best way to just draw column down each side of screen
    ld de, 31
    ld hl, Display+1
drawColZero      
    ld (hl), SHAPE_CHAR_WALL          
    add hl, de  
    ld (hl), SHAPE_CHAR_WALL    
    inc hl    
    inc hl
    djnz drawColZero

    ret


drawAdditionalWalls
    ld b, 10            ;; best way to just draw column down each side of screen
    ld de, 31
    ld hl, Display+412
drawAdditionalWalls_loop      
    ld (hl), SHAPE_CHAR_WALL          
    add hl, de      
    inc hl    
    inc hl
    djnz drawAdditionalWalls_loop

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
;;;;; chances are the bug where extra shake bits drop one line and off the tail is here!!! 
;;;;; chances are the bug where extra shake bits drop one line and off the tail is here!!! 
    ld hl, snakeCoordsCol                        
    ld d, 0
    ld e, b    
    dec e       ; we want e to be one less than the loop counter
    add hl, de
    ld a, (hl)
    inc hl    
    ld (hl), a                    
    ld hl, snakeMovementFlags

#ifdef FULL_DEBUG    
    push bc
    push hl
    push af    
    call debugPrintRegisters
    pop af    
    pop hl    
    pop bc    
#endif 

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

#ifdef FULL_DEBUG    
    push bc
    push hl
    push af    
    call debugPrintRegisters   
    pop af    
    pop hl    
    pop bc    
#endif    


    
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
    inc a
    ld b, a 
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
    
tryAnotherRCol                          ; generate random number to index shape memory
    ld a, r                             
    and %00011111
    cp 29    
    jp nc, tryAnotherRCol               ; loop when nc flag set ie not less than 5 try again    
    inc a                               ; inc guarntees range 1 to 30 for col
    ld (setRandomFoodCOL), a
    
tryAnotherRRow                          ; generate random number to index shape memory
    ld a, r                             
    and %00011111
    cp 20    
    jp nc, tryAnotherRRow               ; loop when nc flag set ie not less than 5 try again    
    inc a                               ; inc guarntees range 1 to 21 for row 
    ld (setRandomFoodROW), a
    
    ld a, (setRandomFoodROW)   
	ld h, a				    ; row set for PRINTAT
    ld a, (setRandomFoodCOL)
    ld l, a				    ; column set for PRINTAT
    
    push hl  ; push hl to get into bc via the pop, why is ld bc, hl not an instruction? who am I to question :)
    pop bc

    push hl   ; save registers thinking is it's the bug that drops print at down one
    push de
    push af    
    push bc   
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e     
    pop bc
    pop af
    pop de
    pop hl  


    ld a, SHAPE_CHAR_FOOD
    call PRINT 
    
    ret

goIntoPause
	ld bc,$0fff ;max waiting time
pauseWaitLoop       ; needs a wait otherwise the key press is still down reads to quick
	dec bc
	ld a,b
	or c
	jr nz, pauseWaitLoop

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 0, a                            ; "P"
    jp z, returnFromPause           ; only returns from pause when p is pressed again
    jr goIntoPause
returnFromPause    
    jp afterPause

;check if TV synchro (FRAMES) happend
vsync	
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
	ret


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
    ld a, (snakeCoordsRow)
    call hprint    
    ld a, 14
    call PRINT      
    ld a, (snakeCoordsCol)
    call hprint    
    ld a, 14
    call PRINT          
    ld a, (snakeTailIndex)   
    call hprint    
    ld a, 14
    call PRINT      

    ld hl, (absoluteScreenMemoryPosition) 
    ld a, h
    call hprint    
    ld a, l
    call hprint    

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




;include our variables
;#include "vars.asm"
game_over_txt
	DEFB	_G,_A,_M,_E,__,_O,_V,_E,_R,$ff 
scoreText    
    DEFB	_S,_C,_O,_R,_E,__,__,$ff 
shapeSet
    DEFB SHAPE_CHAR_SNAKE_RIGHT
shapeOnFlag
    DEFB 0
movedFlag
    DEFB 0      ; default to stationary = 0, 1 = left, 2 = right, 3 = up, 4 = down
absoluteScreenMemoryPosition
    DEFB 0,0    
firstTimeFlag
    DEFB 1   
snakeTailIndex      ; this is the index of the last coordinate of the snake, 
    DEFB 0
snakeTailPlusOne      ; this is the index of the last coordinate of the snake, 
    DEFB 0    
setRandomFoodCOL
    DEFB 0
setRandomFoodROW
    DEFB 0
waitSpeed
    DEFB 0,0
score_mem_tens
	DEFB 0
score_mem_hund
	DEFB 0
high_score_mem_tens
	DEFB 0
high_score_mem_hund
	DEFB 0		
last_score_mem_tens
	DEFB 0
last_score_mem_hund
	DEFB 0	
; the snake can grow to a maximum length of 255 so store  255 row and column positions
; and movement directions to enable it to be undrawn properly as it moves around.

snakeMovementFlags       ; this keeps track of the direction in force at each snake body position
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0     
snakeCoordsCol            
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   
snakeCoordsRow    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
snakeCharsToPrint
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_S,_N,_A,_K,_E,$ff
keys_screen_txt_1
	DEFB	_S,__,_T,_O,__,_S,_T,_A,_R,_T,26,__,_O,__,_L,_E,_F,_T,26,__,_P,__,_R,_I,_G,_H,_T,$ff
keys_screen_txt_2
	DEFB	__,_Q,__,_U,_P,26,__,__,__,_A,__,_D,_O,_W,_N,$ff    
keys_screen_txt_3
	DEFB	_S,_P,_A,_C,_E,__,_P,_A,_U,_S,_E,$ff    
game_objective_txt
	DEFB	_G,_R,_O,_W,__,_S,_N,_A,_K,_E,__,_A,_N,_D,__,_A,_V,_O,_I,_D,$ff
	
last_Score_txt
	DEFB	21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff	
high_Score_txt
	DEFB	21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff		
credits_and_version_1
	DEFB _B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,$ff
credits_and_version_2
	DEFB __,__,__,__,__,__,_2,_0,_2,_2,24,_3,__,__,__,$ff    
TextForGameJam1    
    DEFB _P,_A,_R,_T,_I,_C,_I,_P,_A,_T,_E,_D,0,_I,_N,0,_T,_H,_E,0,_R,_E,_T,_R,_O,$ff
TextForGameJam2    
    DEFB _P,_R,_O,_G,_R,_A,_M,_M,_E,_R,_S,0,_I,_N,_S,_I,_D,_E,16,_R,_P,_I,17,$ff
TextForGameJam3    
    DEFB _A,_N,_D,0, _P,_H,_A,_Z,_E,_1,_0,_1,0,_G,_A,_M,_E,0,_J,_A,_M,$ff
snakeCoordsColTemp
    DEFB 0
snakeCoordsRowTemp
    DEFB 0
foodCount
    DEFB 0    
   
#include "line2.asm"
#include "screenFull.asm"      			; definition of the screen memory       
#include "endbasic.asm"
