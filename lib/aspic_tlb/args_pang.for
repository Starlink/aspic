	.TITLE	ARGS_PANG
;-
;
;  - - - - - - - - - - - -
;  :  A R G S _ P A N G  :
;  - - - - - - - - - - - -
;
;
;  ARGS graphic store program to drive the cursor with the
;  trackerball and to change the ZOOM factors depending on
;  the buttons that have been pressed. The use of the buttons
;  is as follows:-
;
;	1	Reset ZOOM and image position
;	2	Increment X ZOOM factor
;	3	Increment Y ZOOM factor
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
;  D.J.King	R.G.O.	Dec  1981
;
;-

	.LIBRARY	/ARGSMAC/

	ARGSDEF	PANZG,<^X400>

	ARGSINPUT

	ARGSOUTPUT

ZMX:	.WORD	0		; x zoom
ZMY:	.WORD	0		; y zoom

DATA1:	.WORD	0		; cursor x
DATA2:	.WORD	0		; cursor y

	ARGSOUTEND

	ARGSINEND


;  ARGS program: preliminaries

	ARGSENTRY

	DEV.	TBR		; read TB to reset everything

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
	SETVV	ZMX,WORK	; get x zoom
	ADDVV	ZMY,WORK	; add y zoom
	SETVV	WORK,Z+2	; set x and y zooms into zoom instruction
Z:	ZOM	0,8,0,0
	SETVV	TBR4,WORK
	ANDLV	<^X0010>,WORK	; has button 2 been pressed?
	BRAZE	TB2
	SETVV	ZMXD,WORK
	BRAZE	ZMXAD		; incrementing or decrementing x zoom
	SUBLV	1,ZMX		; decrement x zoom by 1
	CMPLV	0,ZMX
	BRALT	ZXOK		; if zoom=0 set zoom to increment
	SETLV	0,ZMXD
	JRE	LOOP
ZMXAD:
	ADDLV	1,ZMX		; increment zoom by 1
	CMPLV	15,ZMX
	BRAGT	ZXOK		; if zoom=15 set zoom to decrement
	SETLV	1,ZMXD
ZXOK:
	JRE	LOOP
TB2:
	SETVV	TBR4,WORK
	ANDLV	<^X0020>,WORK	; has button 3 been pressed?
	BRAZE	TB4
;	
;		Because of the interlaced display the y zooms are
;		in multiples of 2. As the y zoom factor is the most
;		signifigant byte in the zoom instruction all y zoom
;		factors appearing in the code are *256 .
;
	SETVV	ZMYD,WORK	; incrementing or decrementing y zoom?
	BRAZE	ZMYAD
	CMPLV	256,ZMY		; y zoom=1?
	BRALT	ZMYGT
	SUBLV	256,ZMY		; if yes decrement zoom by 1
	SETLV	0,ZMYD		; set zoom to increment
	JRE	LOOP
ZMYGT:
	SUBLV	512,ZMY		; decrement zoom by 2
	JRE	LOOP
ZMYAD:
	SETVV	ZMY,WORK
	BRAGT	ZMYNZ		; y zoom=0
	ADDLV	256,ZMY		; if yes increment zoom by 1
	JRE	LOOP
ZMYNZ:
	ADDLV	512,ZMY		; increment zoom by 2
	CMPLV	3840,ZMY	; y zoom=15?
	BRAGT	ZYOK
	SETLV	1,ZMYD		; if yes set zoom to decrement
ZYOK:
	JRE	LOOP
TB4:
	SETVV	TBR4,WORK
	ANDLV	<^X0008>,WORK	; has button 1 been pressed?
	BRAZE	TB3
	SETLV	0,ZMX		; set x zoom to 0
	SETLV	0,ZMY		; set y zoom to 0
	SETLV	256,DATA1
	SETLV	256,DATA2	; set image to centre of screen
	JRE	LOOP
TB3:
	ANDLV	<^X0040>,TBR4	; has button 4 been pressed?
	BRAZE	LOOP		; if not go back and read TB
;wrap up
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
TBR:				; parameters: read TB
TBR1:	.WORD	<^X0044>	; device 4, mode 4
TBR2:	.WORD	0		; dx
TBR3:	.WORD	0		; dy
TBR4:	.WORD	0		; xxxx xxxx x432 1xxx


CS:				; parameters: set cursor x or y
CS1:	.WORD	0		; cursor number
CS2:	.WORD	0		; x or y


ZMXD:	.WORD	0		; indicator for x zoom increment/decrement
ZMYD:	.WORD	0		; indicator for y zoom increment/decrement

WORK:	.WORD	0

	ARGSEND	PANZG

	.END
