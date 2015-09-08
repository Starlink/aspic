	.TITLE	RGO_TBC
;-
;
;  - - - - - - - - - - -
;  :   R G O _ T B C   :
;  - - - - - - - - - - -
;
;
;  ARGS graphic store program to drive the cursor with the
;  trackerball, returning control to the host when any
;  combination of buttons is pressed.
;
;
;
;  DJK  Starlink  RGO  JUN  81
;
;-

	.LIBRARY	/ARGSMAC/

	ARGSDEF	TBCX,<^X400>



	ARGSINPUT

CRNO:	.WAD	CR1		; cursor number

	ARGSINEND


	ARGSOUTPUT

DATA:
DATA1:	.WORD	33		; cursor x
DATA2:	.WORD	33		; cursor y
DATA3:	.WORD	33		; buttons

	ARGSOUTEND



;  ARGS program: preliminaries

	ARGSENTRY

	SETVV	CRNO,CR		; set up input cursor number
	SETVV	CRNO,CS		; into program data areas
	DEV.	TBR
	CUR.	CR		; read cursor address
	MOV	CR2,CR3,DATA	; copy into data area
	SETLV	0,DATA3		; set buttons pressed to none
	SETLV	50,COUNTB	; set timeout period for multi button presses
	SETLV	0,CONSTB

;  main loop

LOOP:
	DEV.	TBR		; read TB
	ADDVV	TBR2,DATA1	; update x
	LDX	DATA1		; restrict range
	JSR	CLAMP
	ADDVV	TBR3,DATA2	; update y
	LDX	DATA2		; restrict range
	JSR	CLAMP
	SETVV	DATA1,CS2	; set cursor x
	XCL.	CS
	SETVV	DATA2,CS2	; set cursor y
	YCL.	CS
	ANDLV	<^X0078>,TBR4	; mask button bits
	BRAZE	NBUT		; if a button has been pressed
	SETLV	1,CONSTB	; then set button timeout constant to 1
NBUT:
	ORVV	TBR4,DATA3	; set all buttons pressed before timeout
	SUBVV	CONSTB,COUNTB	; subtract constant from button timeout counter
	BRAGT	LOOP		; counter=0 means timeout

;  wrap up

EXIT:
	STP			; stop


;  subroutine to keep x & y in range
;  enter with address of coordinate in index register

CLAMP:
	CMPLI	-1,0		; <-1?
	BRALT	10$		; yes
	SETLI	-1,0		; no: set to -1
	JMA	20$		; finished
10$:
	CMPLI	512,0		; >512?
	BRAGE	20$		; no: finished
	SETLI	512,0		; yes: set to 512
20$:
	RET	0		; return

;  data areas

COUNTB:	.WORD	0		; button timeout count
CONSTB:	.WORD	0		; button timeout constant

TBR:				; parameters: read TB
TBR1:	.WORD	<^X0044>	; device 4, mode 4
TBR2:	.WORD	0		; dx
TBR3:	.WORD	0		; dy
TBR4:	.WORD	0		; xxxx xxxx x432 1xxx

CR:				; parameters: read cursor address
CR1:	.WORD	0		; cursor number
CR2:	.WORD	0		; x
CR3:	.WORD	0		; y

CS:				; parameters: set cursor x or y
CS1:	.WORD	0		; cursor number
CS2:	.WORD	0		; x or y

	ARGSEND	TBCX

	.END
