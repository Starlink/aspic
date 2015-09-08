	.TITLE	ARGS_BPOSCL

;+  ARGS_BPOSCL
;
;   Oscillate the ARGS display between frames PMIN and PMAX.
;
;   See documentation for the driving subroutine args_poscl for further
;   details.
;
;   Modified 28Jun82 for new CMP 
;   J.A.Cooke/UOE/22Mar82;28Jun82
;-

	.LIBRARY	/ARGSMAC/

	ARGSDEF	BPOSCL,<^X400>

	ARGSINPUT		; INPUT data
PMIN:	.BLKW	1		; start pixel plane 0-14
PMAX:	.BLKW	1		; end pixel plane 1-15 (> PMIN)
DEL:	.BLKW	1		; delay between frames
	ARGSINEND

	ARGSENTRY

	TB_INIT			; initialise trackerball
	DEV	4,5
	.WORD	<^X78>		; light all lamps

	SETLV	1,DIR		; count UP
	SETVV	PMIN,PCNT	; start at PMIN

LOOP:
	TB_READ BT4,BT3,BT2,BT1	; button controls for movie
	JSR	NEXT
	JMR	LOOP

BT1:
	JMR	STEP
BT2:
	SUBLV	2,DEL		; decrease delay
	CMPLV	0,DEL
	BRALT	LOOP
	SETLV	0,DEL
	JMR	LOOP
BT3:
	ADDLV	2,DEL		; increase delay
	CMPLV	31,DEL
	BRAGT	LOOP
	SETLV	31,DEL
	JMR	LOOP
BT4:
END:
	DEV	4,5
	.WORD	0		; switch off trackerball lamps

	STP

STEP:
	TB_READ	BT4,BT3A,BT2A,BT1A	; buttons for step
	JMR	STEP
BT1A:
	JRE	LOOP		; return to movie
				; JRE needed as CMP generates lots of code
BT2A:
	JSR	NEXT		; step
	JMR	STEP
BT3A:
	CMPLV	0,DIR		; change direction
	BRAGE	NEG
	SETLV	-1,DIR
	JMR	STEP
NEG:	SETLV	1,DIR
	JMR	STEP

NEXT:
	CMPLV	0,DIR		; see if counting up or down
	BRAGE	DOWN
UP:
	CMPVV	PMAX,PCNT	; reached max ?
	BRAGT	SETP
	SETLV	-1,DIR		; set DOWN
	JMR	SETP
DOWN:
	CMPVV	PMIN,PCNT	; reached min ?
	BRALT	SETP
	SETLV	1,DIR		; set UP
	JMR	SETP
SETP:
	SETVV	DEL,WCNT	; delay loop
WAIT:	SUBLV	1,WCNT

	SETLV	50,WORK
WT1:	SUBLV	1,WORK
	CMPLV	0,WORK
	BRALE	WT1

	CMPLV	0,WCNT
	BRALE	WAIT

	ADDVV	DIR,PCNT	; add/subtract counter
	SETVV	PCNT,WORK
	SETVV	PCNT,PTR
	SETAV	LIST,WORK
	ADDVV	WORK,PTR	; pointer to bit plane mask
	SETVV	PTR,MASK
	.WORD	<^X6A00>	; pixel plane enable ZDO1 (indirect)
MASK:	.BLKW	1
	RET	0

PCNT:	.BLKW	1		; local variables
DIR:	.BLKW	1
WCNT:	.BLKW	1
WORK:	.BLKW	1
PTR:	.BLKW	1

LIST:	.WORD	<^X0001>	; bit plane masks
	.WORD	<^X0002>
	.WORD	<^X0004>
	.WORD	<^X0008>
	.WORD	<^X0010>
	.WORD	<^X0020>
	.WORD	<^X0040>
	.WORD	<^X0080>
	.WORD	<^X0100>
	.WORD	<^X0200>
	.WORD	<^X0400>
	.WORD	<^X0800>
	.WORD	<^X1000>
	.WORD	<^X2000>
	.WORD	<^X4000>
	.WORD	<^X8000>

	ARGSEND	BPOSCL

	.END
