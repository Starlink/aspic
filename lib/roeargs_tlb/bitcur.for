	.TITLE	ARGS_BITCUR

;+  ARGS_BITCUR
;
;   Displays 64*64 bit 'user cursor' on the ARGS, allows the user
;   to move it around using the trackerball and return its position
;   when he presses any button.  The button code is returned.
;
;   See documentation for the driving subroutine args_usrcur for further
;   details.
;
;   Modified 28Jun82 for new CMP and macro library
;
;   J.A.Cooke/UOE/1Dec81;14Dec81;28Jun82
;-

	.LIBRARY	/ARGSMAC/

	ARGSDEF	BITCUR,<^X400>

	ARGSINPUT		; INPUT data
CRS:	.BLKW	256		; cursor in 64*64 bit array
OVP:	.BLKW	1		; overlay plane (8-15)
XIN:	.BLKW	1		; starting X posn of bot lh corner
YIN:	.BLKW	1		;     "    Y  "   "   "  "    "
	ARGSINEND

	ARGSENTRY

	TB_INIT			; initialise trackerball
	DEV	4,5
	.WORD	<^X78>		; light all lamps

	SETAV	CRS,ADDR	; CRS address
	SETVV	ADDR,CSTA
	ADDLV	255,ADDR	; CRS address + 255
	SETVV	ADDR,CEND
	SETAV	BITS,ADDR
	SETVV	ADDR,BSTA

	.WORD	<^X4000>	; copy cursor into executable code (MOV)
				; MOV explicitly now needs arguments
CSTA:	.BLKW	1
CEND:	.BLKW	1
BSTA:	.BLKW	1

	SETVV	OVP,WORK	; mask overlay plane to range 0-15
	ANDLV	15,WORK
	SETVV	WORK,ZSR
	.WORD	<^X0700>	; set zsr for correct plane
ZSR:	.BLKW	1

	JSR	DRAW		; draw user cursor

LOOP:
	TB_READ BT4,BT3,BT2,BT1	; exit on any button
	SETVV	TB_DELTAX,WORK
	ADDVV	TB_DELTAY,WORK
	CMPLV	0,WORK
	BRAZE	LOOP
	JSE	ERASE		; if trackerball has moved, erase...
	ADDVV	TB_DELTAX,XIN	; ...increment X...
	ADDVV	TB_DELTAY,YIN	; ...and Y...
	JSR	DRAW		; ...and draw
	JMR	LOOP

BT1:
	SETLV	1,TBB
	JMR	END
BT2:
	SETLV	2,TBB
	JMR	END
BT3:
	SETLV	3,TBB
	JMR	END
BT4:
	SETLV	4,TBB
END:
	JSE	ERASE		; erase user cursor
	SETVV	XIN	XOUT	; set up output data
	SETVV	YIN	YOUT

	DEV	4,5
	.WORD	0		; switch off trackerball lamps

	.WORD	<^X0700>	; reset ZSR
	.WORD	<^X1F00>

	STP

DRAW:
	SETVV	XIN,XC
	ANDVV	MASK,XC		; XIN masked to 12 bits
	SETVV	YIN,YC
	ANDVV	MASK,YC		; YIN masked to 12 bits
	SETVV	XC,XSTA
	SETVV	YC,YSTA

	.WORD	<^X3E00>	; write pixel data +x +y, mode 0 (bits)
XSTA:	.BLKW	1		; x start address
YSTA:	.BLKW	1		; y start address
	.WORD	<^X0040>	; no of points in x
	.WORD	<^X0040>	; no of points in y
	.WORD	<^X0100>	; no of data words
BITS:	.BLKW	256		; bit pixel data
	RET	0

ERASE:
	SETVV	XSTA,XEST	; set current position for erasing
	SETVV	YSTA,YEST

	.WORD	<^X3E00>	; write pixel data as for DRAW
XEST:	.BLKW	1
YEST:	.BLKW	1
	.WORD	<^X0040>
	.WORD	<^X0040>
	.WORD	<^X0100>
	.WORD	0[256]		; bit pixel data - zeros
	RET	0

WORK:	.BLKW	1		; local variables
XC:	.BLKW	1
YC:	.BLKW	1
ADDR:	.BLKW	1

MASK:	.WORD	<^X0FFF>	; constants

	ARGSOUTPUT		; OUTPUT data
XOUT:	.BLKW	1		; final X posn
YOUT:	.BLKW	1		;   "   Y  "
TBB:	.BLKW	1		; button code
	ARGSOUTEND

	ARGSEND	BITCUR

	.END
