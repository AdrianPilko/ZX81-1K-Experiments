;ZX81 screens
; Display file (ZX81 screen) - low res screen 
Display        	; empty (collapsed) DFILE on zx81 1K consists of the 25 chr$118 (hex 118)
	DEFB 118,7,  3,  3,  3,  3,  3,  3,  3,132  ; 0, 136 first chr$118 marks the start of DFILE     
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 1, play area offset from DF_CC 12 to 18 
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 2, "" 22 to 28  
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 3  "" 32 to 38
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 4     42 to 48  etc
	DEFB 118,5,136, _O, _U, _T,136, _O, _F,133  ; 5  
	DEFB 118,5, _M, _E, _M, _O, _R, _Y,136,133  ; 6  
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 7    ;; all the 136 in here get overriten with the 
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 8    ;; blank space "0" character, but this was to test 
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 9    ;; that the screen has blanked fully
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 10   ;; 
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 11   ;; if the memory is full then thre game will crash
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 12   ;; beforte it gets to clear play area  hence the 
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 13   ;; message no-one should see "OUT OF MEMEORY"
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 14   ;; if the game fits in 1K
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 15
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 16
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 17
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 18
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 19
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 20
	DEFB 118,5,136,136,136,136,136,136,136,133  ; 21
	DEFB 118,5,128,128,128,128,128,128,128,133  ; 22,   222 to last block on play area 228
    DEFB 118,130,131,131,131,131,131,131,131,129; 23
	DEFB 118

                

