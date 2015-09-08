	.TITLE	ARGS_RCUR

;+  ARGS_RCUR
;
;   Display a rectangular 'cursor' on the ARGS, allow the user to
;   move it around using the trackerball and return its position
;   when he presses any button.
;
;   See documentation for the driving subroutine args_reccur for
;   further details.
;
;   W F Lupton RGO (at ROE) 17th November 1981
;   modified J.A.Cooke/UOE/20Nov81+14Dec81   to return button code
;   modified JAC/UOE/28Jun82 for new CMP and library
;-
	.LIBRARY	/ARGSMAC/

	ARGSDEF	RCUR,<^X400>

	ARGSINPUT		; INPUT data
COL:	.BLKW	1		; ZDI value to be used for cursor
XIN:	.BLKW	1		; starting X posn of bot lh corner
YIN:	.BLKW	1		;     "    Y  "   "   "  "    "
XSIZ:	.BLKW	1		; size in X dirn (XIN to XIN+XSIZ)
YSIZ:	.BLKW	1		;  "   "  Y  "   (YIN to YIN+YSIZ)
	ARGSINEND

	ARGSENTRY

	TB_INIT			; initialise trackerball
	DEV	4,5
	.WORD	<^X78>		; light all lamps

	SDM	3		; Solid Line Box drawing mode
	JSR	DRAW		; draw user cursor

LOOP:
	TB_READ BT4,BT3,BT2,BT1	; exit on any button
	SETVV	TB_DELTAX,WORK
	ADDVV	TB_DELTAY,WORK
	CMPLV	0,WORK
	BRAZE	LOOP
	JSR	ERASE		; if trackerball has moved, erase...
	ADDVV	TB_DELTAX,XIN	; ...increment X...
	ADDVV	TB_DELTAY,YIN	; ...and Y...
	JSR	DRAW		; ...and draw
	JMR	LOOP

BT1:
	SETLV	1,TBB		; button 1
	JMR	END
BT2:
	SETLV	2,TBB		; button 2
	JMR	END
BT3:
	SETLV	3,TBB		; button 3
	JMR	END
BT4:
	SETLV	4,TBB		; button 4
END:
	JSR	ERASE		; erase user cursor
	SETVV	XIN	XOUT	; set up output data
	SETVV	YIN	YOUT

	DEV	4,5
	.WORD	0		; switch off trackerball lamps

	SDM	0		; restore Solid Vector drawing mode

	STP

ERASE:
	ZDI	0		; ZDI 0 because erasing
	JMR	DRAW1
DRAW:
	ZDI.	COL		; ZDI COL because drawing
DRAW1:
	SETVV	XIN,XC
	ANDVV	MASK,XC		; XIN masked to 12 bits
	SETVV	YIN,YC
	ANDVV	MASK,YC		; YIN masked to 12 bits
	ADDVV	XMAC,XC		; add in OP codes
	ADDVV	YMAC,YC
	SETVV	XC,XMA		; and move to bottom lh corner
	SETVV	YC,YMA
XMA:	.BLKW	1
YMA:	.BLKW	1

	ADDVV	XSIZ,XC		; increment X posn
	SETVV	XC,TCX
TCX:	.BLKW	1
	SETVV	YIN,YC
	ADDVV	YSIZ,YC
	ANDVV	MASK,YC
	ADDVV	YDAC,YC
	SETVV	YC,YDA		; and draw to top rh corner
YDA:	.BLKW	1
	RET	0

WORK:	.BLKW	1		; local variables
XC:	.BLKW	1
YC:	.BLKW	1

MASK:	.WORD	<^X0FFF>	; constants
XMAC:	.WORD	<^XC000>
YMAC:	.WORD	<^XA000>
YDAC:	.WORD	<^XE000>

	ARGSOUTPUT		; OUTPUT data
XOUT:	.BLKW	1		; final X posn
YOUT:	.BLKW	1		;   "   Y  "
TBB:	.BLKW	1		; button code (1 to 4)
	ARGSOUTEND

	ARGSEND	RCUR

	.END
