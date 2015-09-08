	.TITLE	ARGS_ROT
;+
;  ---------------
;  : R O T A T E :
;  _______________
;
;  Rotates the ARGS colour table under trackerball control
;
;-
	.LIBRARY	/ARGSMAC/

	ARGSDEF	ROT,<^X0400>

	ARGSINPUT
	.BYTE	0,0,0,0
TABLE:
	I=0
	.REPEAT 255
	.BYTE	I,I,I,0
	I=I+1
	.ENDR
	ARGSINEND
	I=0
TABLE2:	.BLKW	510			; space for second copy
START:
	TB_INIT
	ARGSENTRY
	MOV	TABLE,TABLE+1018,TABLE2	; duplicate VLUT


	TB_READ				; dummy read to reset tracker ball
	DEV 4,5				; switch on light used for
	.WORD	^X48			; exit.
	JMA	DISP

LOOP:	TB_READ END,,,BEGIN
	CMPLV	0,TB_DELTAX
	BRAZE	LOOP

DISP:
	SUBVV	TB_DELTAX,OFFSET	; accumulate rotation
	ANDLV	<^X00FE>,OFFSET		; MOD (256)

RDISP:
	SETAV	TABLE,MOV+2
	ADDVV	OFFSET,MOV+2
	ADDVV	OFFSET,MOV+2

	SETVV	MOV+2,MOV+4
	ADDLV	509,MOV+4

MOV:	MOV	0,0,LOAD


	LVT	254,1
LOAD:
	.BLKW	510

	JMA	LOOP

BEGIN:
	SETLV	0,OFFSET
	JMA	RDISP

END:	DEV 4,5			; switch off lights
	.WORD	0
	STP

	ARGSOUTPUT
OFFSET:	.WORD	0
	ARGSOUTEND

	ARGSEND	ROT
	.END
