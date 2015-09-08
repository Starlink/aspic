	.TITLE UDISC
	.IDENT /01/
;
;+++
;
; UDISC: FORTRAN callable routines for efficient direct access I/O to a
; single file, for use by MEM program. A temporary file is created, which
; is automatically deleted at the end of the program.
;
; There are 4 entry points:
;
;       CALL UOPEN(NAME,ISIZE,ISTAT)
; opens/creates file
; given
;       NAME file name CHARACTER*(*)
;       ISIZE file size in blocks (512 bytes)
; returned
;       ISTAT .EQ.0 implies file opened/created OK
;             .NE.0 implies something went wrong
;
;       CALL UCLOSE(ISTAT)
; closes file opened by UOPEN
; returned
;       ISTAT .EQ.0 implies file closed OK
;             .NE.0 implies file not closed
;
;       CALL UGET(IVBN,X,M)
; and   CALL UPUT(IVBN,X,M)
; read/write from/to file opened by UOPEN
; given
;       IVBN virtual block number of file where read/write is to start
;       M number of long words to read/write
; returned (UGET) or given (UPUT)
;       X array to read from/ write to
;
; written by John Fielden, Cambridge 04-APR-82
; last modified                      24-JUL-83
;
; Copyright (C) MEDC Ltd.
;
;---
;

	$IODEF
	$RMSDEF

	.PSECT	$LOCAL,RD,WRT,NOEXE,LCL,NOSHR,PIC,REL,CON,LONG,NOVEC

FFAB:	$FAB	DNM = <.DAT>,-
		FAC = <BIO,GET,PUT,DEL>,-
		FOP = <CBT,UFO,TMD>,-
		MRS = 512,-
		ORG = <SEQ>,-
		RFM = <FIX>

QIOARG:	$QIO	IOSB = IOSB

IOSB:	.BLKW	4

	.PSECT	$CODE,RD,NOWRT,EXE,LCL,SHR,PIC,REL,CON,LONG,NOVEC

; UOPEN

	.ENTRY	UOPEN,^M<R11>

;       SUBROUTINE UOPEN(NAME,INEW,ISIZE,ISTAT)

 NAME = 4
 ISIZE = 8
 ISTAT = 12

;

	MOVAL	FFAB,R11

; get file name

	MOVQ	@NAME(AP),R0
	MOVB	R0,FAB$B_FNS(R11)
	MOVL	R1,FAB$L_FNA(R11)

; get size and create file

CREATE:

	MOVL	@ISIZE(AP),FAB$L_ALQ(R11)
	$CREATE	FAB = FFAB
 CHAN = QIO$_CHAN+QIOARG-FFAB
	BLBC	R0,1$
	MOVL	FAB$L_STV(R11),CHAN(R11)
	CLRL	R0
1$:	MOVL	R0,@ISTAT(AP)
	RET

; UCLOSE

	.ENTRY	UCLOSE,^M<>

;       SUBROUTINE UCLOSE(ISTAT)

ISTAT = 4

;

	$DASSGN_S CHAN = QIO$_CHAN+QIOARG
	BLBC	R0,10$
	CLRL	R0
10$:	MOVL	R0,@ISTAT(AP)
	RET

; UGET and UPUT

 IVBN = 4
 X = 8
 M = 12

;

	.ENTRY	UGET,^M<R11>

;       SUBROUTINE UGET(IVBN,X,M)

	MOVAL	QIOARG,R11
	MOVL	#IO$_READVBLK,QIO$_FUNC(R11)
	BRW	DOIO

	.ENTRY	UPUT,^M<R11>

;       SUBROUTINE UPUT(IVBN,X,M)

	MOVAL	QIOARG,R11
	MOVL	#IO$_WRITEVBLK,QIO$_FUNC(R11)

; this bit does the I/O for both UGET and UPUT

DOIO:
	MOVL	@IVBN(AP),QIO$_P3(R11)
	MOVL	X(AP),QIO$_P1(R11)
	ASHL	#2,@M(AP),QIO$_P2(R11)

	$QIOW_G	QIOARG

	BLBC	R0,ERROR
 CSTAT = IOSB-QIOARG
	MOVZWL	CSTAT(R11),R0
	BLBC	R0,ERROR

; successful completion of QIO

	RET

; QIO error, so stop and send appropriate error message

ERROR:
	$EXIT_S	CODE = R0

;

	.END
