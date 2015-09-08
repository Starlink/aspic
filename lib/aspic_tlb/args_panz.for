	.TITLE	ARGS_PANZ
;-
;
;  - - - - - - - - - - - -
;  :  A R G S _ P A N Z  :
;  - - - - - - - - - - - -
;
;
;  ARGS graphic store program to drive the cursor with the
;  trackerball and to change the ZOOM factors depending on
;  the buttons that have been pressed. The use of the buttons
;  is as follows:-
;
;	1	Reset ZOOM and image position
;	3	Increment ZOOM factor
;	2	Decrement ZOOM factor
;	4	Return control to host
;
;
;  The layout of this program is as follows:
;
;	1	base address
;	2	length of program to be loaded
;	3	entry point
;	4	head of chain: cursor x
;	5	cursor y
;	6	button status:  0000 0000 0432 1000
;	7	remainder
;
;
;  D.J.King	R.G.O.	DEC 1981
;
;-

	.LIBRARY	/ARGSMAC/

	ARGSDEF	PANZ,<^X400>

	ARGSINPUT

INZM:	.WORD	0		; current zoom factors
IND:	.WORD	0		; best attempt at zoom table offset

	ARGSOUTPUT

DATA1:	.WORD	0		; cursor x
DATA2:	.WORD	0		; cursor y

	ARGSINEND

DATA3:	.WORD	0		; zoom factors

	ARGSOUTEND

;  ARGS program: preliminaries
	ARGSENTRY
	DEV.	TBR		; read TB to reset everything
	SETVV	IND,ZIND	; set zoom table offset
	SETVV	INZM,Z+2	; set current zoom factors

;  main loop
LOOP:
	DEV.	TBR		; read TB
	SUBVV	TBR2,DATA1	; update x
	LDX	DATA1		; restrict range
	JSE	CLAMP
	SUBVV	TBR3,DATA2	; update y
	LDX	DATA2		; restrict range
	JSE	CLAMP
	SETVV	DATA1,CS2	; set cursor x
	XCL.	CS
	SETVV	DATA2,CS2	; set cursor y
	YCL.	CS
Z:	ZOM	0,8,0,0
	SETVV	TBR4,WORK
	ANDLV	<^X0020>,WORK	; has button 3 been pressed?
	BRAZE	TB2
	JSE	ZFACT		; go and increment table offset
	JRE	LOOP
TB2:
	SETVV	TBR4,WORK
	ANDLV	<^X0010>,WORK	; has button 2 been pressed?
	BRAZE	TB4
	JSE	ZFACT		; go and decrement table offset
	JRE	LOOP
TB4:
	SETVV	TBR4,WORK
	ANDLV	<^X0008>,WORK	; has button 1 been pressed?
	BRAZE	TB3
	SETLV	0,ZIND		; reset table offset to top of table
	LDX	ZOOM0
	SETIV	0,Z+2
	SETLV	256,DATA1
	SETLV	256,DATA2	; set image to centre of screen
	JRE	LOOP
TB3:
	ANDLV	<^X0040>,TBR4	; has button 4 been pressed?
	BRAZE	LOOP		; if not go back and read TB
;wrap up
	SETVV	Z+2,DATA3	; return zoom factors
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

;   subroutine to increment or decrement
;   the offset to a table of zoom factors
ZFACT:
	SETVV	TBR4,WORK
	ANDLV	<^X0020>,WORK	; increment or decrement zoom?
	BRAZE	DOWN
UP:
	CMPLV	4,ZIND		; reached upper limit of table?
	BRALE	END
	ADDLV	1,ZIND		; add 1 to zoom table offset
	JMR	END
DOWN:
	CMPLV	0,ZIND		; reached lower limit of table
	BRAGE	END
	SUBLV	1,ZIND		; subtract 1 from table offset
END:
	LDX	ZOOM0		; load zoom table address
	ADDVX	ZIND		; add table offset
	SETIV	0,Z+2		; patch into ZOM command
	RET	0

;  data areas
TBR:				; parameters: read TB
TBR1:	.WORD	<^X0044>	; device 4, mode 4
TBR2:	.WORD	0		; dx
TBR3:	.WORD	0		; dy
TBR4:	.WORD	0		; xxxx xxxx x432 1xxx

CS:				; parameters: set cursor x or y
CS1:	.WORD	0		; cursor number
CS2:	.WORD	0		; x or y

WORK:	.WORD	0

ZIND:	.WORD	0
ZOOMS:				; table of allowed zooms
ZOOM0:	.WORD	<^X0000>
ZOOM1:	.WORD	<^X0101>
ZOOM3:	.WORD	<^X0303>
ZOOM7:	.WORD	<^X0707>
ZOOM15:	.WORD	<^X0F0F>
	ARGSEND PANZ

	.END
