	.TITLE	WFL_SETSYM
;++
;	SETSYM - Set Symbol (in global symbol table)
;
;	This routine allows a program to set the value of a symbol
;	in the user's DCL global symbol table. Refer to the DCL User's
;	Guide for a full description of symbols.
;
;	CALL WFL_SETSYM(NAME,VALUE)
;
;	Input arguments:
;	---------------
;	NAME:	CHARACTER expression:	Symbol name.
;	VALUE:	CHARACTER expression:	Value to be assigned to symbol
;
;	(NOTE: Users are warned that any preceding or following spaces
;	in both the name and value will be treated as significant by 
;	the CLI)
;
;
;	D.PEARCE  27-JAN-81
;--

NAME=4
VALUE=8

	$CLISERVDEF

REQBLK:	.LONG	CLI$K_DEFGLOBAL@8!CLI$K_CLISERV
	.QUAD
	.QUAD


WFL_SETSYM::
	.WORD	^M<>
	MOVL	NAME(AP),R0		;addr of NAME descr
	CVTBL	0(R0),REQBLK+04		;size of NAME string
	MOVL	4(R0),REQBLK+08		;addr of NAME string
	MOVL	VALUE(AP),R0		;addr of VALUE descr
	CVTBL	0(R0),REQBLK+12		;size of VALUE string
	MOVL	4(R0),REQBLK+16		;addr of VALUE string
	PUSHAB	REQBLK			;stack addr of request block
	CALLS	#1,G^SYS$CLI		;set symbol
	RET
	.END
