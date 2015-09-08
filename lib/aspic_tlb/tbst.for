	.TITLE	TBST
;+
;   ARGS code program to read status of system cursor position 
;      and buttons then return to calling routine.
;
;   The following information is placed in the output data area:
;
;   Word 1	Final X coordinate of cursor
;    "   2        "   Y      "          "
;    "   3      Set to 1 if left hand button pressed, otherwise set to 0
;    "   4       "   " 1 "  centre left  "     "          "      "     0
;    "   5       "   " 1 "  centre right "     "          "      "     0
;    "   6       "   " 1 "  right hand   "     "          "      "     0
;
; This data area is reinitialised each time the program is run so that it
; can be re-run without being reloaded.
;
; Only one button can be pressed, ie if two are pressed simultaniously
; only the right hand one only is indicated to the VAX.
;
; The VAX program is responsible for enabling/disabling the cursor and 
; control of the trackerball lights.
;
; Original program was called TBC, which came from:
;  DLT Starlink MAN  OCT 1981
;  JRG Starlink RAL.  Modified March 1982.
;        (1) Insert the PTW clamp to keep cursor in range.
;        (2) Alter to use modified CMP macros.
;        (3) Alter use of TB_INIT because of new ARGS.
;
;  W Pence, modified it in Dec 1982 to return immediately, regardless 
;     of whether any of the buttons have been pressed.
;  BMC Starlink ROE JULY 1984
;        (1) Insert .LIBRARY statement for ARGSMAC
;-

	.LIBRARY	/ARGSMAC/

	ARGSDEF	TBST,<^X3C1>
	TB_INIT				; Expanded (for TB_REG) but
					; not obeyed.
	ARGSENTRY
;	TB_READ				; Dummy read to clear rubbish
;                                         NOT NEEDED IN THIS VERSION!

	SETLV	0,OUT1			; Set buttons data area to zero
	SETLV	0,OUT2
	SETLV	0,OUT3
	SETLV	0,OUT4


	CUR.	CREAD			; Read current cursor position
	SETVV	X,LOADX+2		; Move to working area
	SETVV	Y,LOADY+2

LOOP:	TB_READ	P1,P2,P3,P4

	ADDVV	TB_DELTAX,LOADX+2	; Add T/B offset to cursor position
	ADDVV	TB_DELTAY,LOADY+2
	LDX	LOADX+2			; CLAMP X POS
	JSR	CLAMP
	LDX	LOADY+2			; CLAMP Y POS
	JSR	CLAMP

LOADX:	XCL	0,0			; Load cursor position
LOADY:	YCL	0,0

	JMR	EXIT                    ; EXIT IMMEDIATELY

P1:	SETLV	1,OUT1			; Set word indicating button press
					; to 1
	JMR	EXIT

P2:	SETLV	1,OUT2
	JMR	EXIT

P3:	SETLV	1,OUT3
	JMR	EXIT

P4:	SETLV	1,OUT4

EXIT:	SETVV	LOADX+2,X		; Move final cursor position to
	SETVV	LOADY+2,Y		; output area

	STP

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

;   Data areas

CREAD:	.WORD	0			; use system cursor

	ARGSOUTPUT
X:	.BLKW	1
Y:	.BLKW	1
OUT4:	.BLKW	1
OUT3:	.BLKW	1
OUT2:	.BLKW	1
OUT1:	.BLKW	1

	ARGSOUTEND

	ARGSEND	TBST
	.END
