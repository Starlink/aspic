	.TITLE	ASP_FMTCON
;++
;	ASP_FMTCON - Format Conversion
;
;	This routine is used to convert data from one format to another.
;	There are seven possible data formats, each assigned a unique
;	numeric code:
;
;	1:  Signed Byte
;	2:  Signed Word
;	3:  Signed Longword
;	4:  Real
;	5:  Double Precision
;	6:  Unsigned Byte
;	7:  Unsigned Word
;
;	(No check is made that the codes passed to this routine are
;	within the above range; it is assumed that the calling program
;	has performed this task).
;
;	A source buffer containing data in one of these formats is pro-
;	-cessed to produce an output buffer containing data in the re-
;	-quested format. If conversion to the same type is requested,
;	then the operation is not performed and the status return value,
;	NBAD, is set to -1; otherwise, NBAD is used to accumulate an
;	error count for any 'bad' conversions which are detected. If a
;	destination value is produced that is out of range for the par-
;	-ticular format then an associated 'high' value will be loaded.
;
;	CALL ASP_FMTCON(SRCFMT,DSTFMT,SRCBUF,DSTBUF,NVAL,NBAD)
;
;	Input parameters:
;	----------------
;	SRCFMT:  INTEGER expression:     Format code of source data
;	DSTFMT:  INTEGER expression:     Format code of destination data
;	SRCBUF:  Non-CHARACTER array:    Source buffer
;	NVAL:    INTEGER expression:     Number of values to convert
;
;	Output parameters:
;	-----------------
;	DSTBUF:  Non-CHARACTER array:    Destination buffer
;	NBAD:    INTEGER variable:       Number of 'bad' conversions
;
;
;	D.PEARCE  29/JUN/80  VERSION #2
;
;	W.F.LUPTON 3/FEB/81 RENAMED STL_FMTCON TO ASP_FMTCON
;	(FAILURE NOW CAUSES LOADING OF A HIGH VALUE)
;--
;

ROUTINE_TABLE:
	.ADDRESS -
		$NOCON		;NO CONVERSION       (SAME TYPE)
	.ADDRESS -
		$SB_SW		;SIGNED BYTE      TO SIGNED WORD
	.ADDRESS -
		$SB_SL		;SIGNED BYTE      TO SIGNED LONGWORD
	.ADDRESS -
		$SB_R		;SIGNED BYTE      TO REAL
	.ADDRESS -
		$SB_DP		;SIGNED BYTE      TO DOUBLE PRECISION
	.ADDRESS -
		$SB_UB		;SIGNED BYTE      TO UNSIGNED BYTE
	.ADDRESS -
		$SB_UW		;SIGNED BYTE      TO UNSIGNED WORD
	.ADDRESS -
		$SW_SB		;SIGNED WORD      TO SIGNED BYTE
	.ADDRESS -
		$NOCON		;NO CONVERSION       (SAME TYPE)
	.ADDRESS -
		$SW_SL		;SIGNED WORD      TO SIGNED LONG
	.ADDRESS -
		$SW_R		;SIGNED WORD      TO REAL
	.ADDRESS -
		$SW_DP		;SIGNED WORD      TO DOUBLE PRECISION
	.ADDRESS -
		$SW_UB		;SIGNED WORD      TO UNSIGNED BYTE
	.ADDRESS -
		$SW_UW		;SIGNED WORD      TO UNSIGNED WORD
	.ADDRESS -
		$SL_SB		;SIGNED LONGWORD  TO SIGNED BYTE
	.ADDRESS -
		$SL_SW		;SIGNED LONGWORD  TO SIGNED WORD
	.ADDRESS -
		$NOCON		;NO CONVERSION       (SAME TYPE)
	.ADDRESS -
		$SL_R		;SIGNED LONGWORD  TO REAL
	.ADDRESS -
		$SL_DP		;SIGNED LONGWORD  TO DOUBLE PRECISION
	.ADDRESS -
		$SL_UB		;SIGNED LONGWORD  TO UNSIGNED BYTE
	.ADDRESS -
		$SL_UW		;SIGNED LONGWORD  TO UNSIGNED WORD
	.ADDRESS -
		$R_SB		;REAL             TO SIGNED BYTE
	.ADDRESS -
		$R_SW		;REAL             TO SIGNED WORD
	.ADDRESS -
		$R_SL		;REAL             TO SIGNED LONGWORD
	.ADDRESS -
		$NOCON		;NO CONVERSION       (SAME TYPE)
	.ADDRESS -
		$R_DP		;REAL             TO DOUBLE PRECISION
	.ADDRESS -
		$R_UB		;REAL             TO UNSIGNED BYTE
	.ADDRESS -
		$R_UW		;REAL             TO UNSIGNED WORD
	.ADDRESS -
		$DP_SB		;DOUBLE PRECISION TO SIGNED BYTE
	.ADDRESS -
		$DP_SW		;DOUBLE PRECISION TO SIGNED WORD
	.ADDRESS -
		$DP_SL		;DOUBLE PRECISION TO SIGNED LONGWORD
	.ADDRESS -
		$DP_R		;DOUBLE PRECISION TO REAL
	.ADDRESS -
		$NOCON		;NO CONVERSION       (SAME TYPE)
	.ADDRESS -
		$DP_UB		;DOUBLE PRECISION TO UNSIGNED BYTE
	.ADDRESS -
		$DP_UW		;DOUBLE PRECISION TO UNSIGNED WORD
	.ADDRESS -
		$UB_SB		;UNSIGNED BYTE    TO SIGNED BYTE
	.ADDRESS -
		$UB_SW		;UNSIGNED BYTE    TO SIGNED WORD
	.ADDRESS -
		$UB_SL		;UNSIGNED BYTE    TO SIGNED LONGWORD
	.ADDRESS -
		$UB_R		;UNSIGNED BYTE    TO REAL
	.ADDRESS -
		$UB_DP		;UNSIGNED BYTE    TO DOUBLE PRECISION
	.ADDRESS -
		$NOCON		;NO CONVERSION       (SAME TYPE)
	.ADDRESS -
		$UB_UW		;UNSIGNED BYTE    TO UNSIGNED WORD
	.ADDRESS -
		$UW_SB		;UNSIGNED WORD    TO SIGNED BYTE
	.ADDRESS -
		$UW_SW		;UNSIGNED WORD    TO SIGNED WORD
	.ADDRESS -
		$UW_SL		;UNSIGNED WORD    TO SIGNED LONGWORD
	.ADDRESS -
		$UW_R		;UNSIGNED WORD    TO REAL
	.ADDRESS -
		$UW_DP		;UNSIGNED WORD    TO DOUBLE PRECISION
	.ADDRESS -
		$UW_UB		;UNSIGNED WORD    TO UNSIGNED BYTE
	.ADDRESS -
		$NOCON		;NO CONVERSION       (SAME TYPE)

HIGH_SB:.BYTE	^X7F
HIGH_SW:.WORD	^X7FFF
HIGH_SL:.LONG	^X7FFFFFFF
HIGH_UB:.BYTE	^X7F
HIGH_UW:.WORD	^X7FFF

ASP_FMTCON::
	
	.WORD	^M<R2,R3,R4,R11>

	MOVAL	ROUTINE_TABLE,R0
	SUBL3	#1,@04(AP),R1		;decrement source code
	MULL2	#28,R1			;R1 * 28 = displ #1
	ADDL2	R1,R0			;add to base addr of table
	SUBL3	#1,@08(AP),R2		;dest code - 1 = displ #2
	MOVL	(R0)[R2],R11		;R11 = addr of routine

	MOVL	@20(AP),R1		;number of data values
	MOVL	12(AP),R2		;addr of source buffer
	MOVL	16(AP),R3		;addr of destination buffer
	MOVL	24(AP),R4		;addr of NBAD
	CLRL	(R4)			;NBAD = 0

	JSB	(R11)			;perform conversion routine

	RET

;------ SIGNED BYTE TO SIGNED WORD -------------------------------------
$SB_SW:	CVTBW	(R2)+,(R3)+		;convert byte/word
	SOBGTR	R1,$SB_SW
	RSB

;------ SIGNED BYTE TO SIGNED LONGWORD ---------------------------------
$SB_SL:	CVTBL	(R2)+,(R3)+		;convert byte/longword
	SOBGTR	R1,$SB_SL
	RSB

;------ SIGNED BYTE TO REAL --------------------------------------------
$SB_R:	CVTBF	(R2)+,(R3)+		;convert byte/floating
	SOBGTR	R1,$SB_R
	RSB

;------ SIGNED BYTE TO DOUBLE PRECISION --------------------------------
$SB_DP:	CVTBD	(R2)+,(R3)+		;convert byte/double
	SOBGTR	R1,$SB_DP
	RSB

;------ SIGNED BYTE TO UNSIGNED BYTE -----------------------------------
$SB_UB:	MOVB	(R2)+,(R3)+		;move byte
	BGEQ	30$			;destination byte 0 or +ve ?
	MOVB	HIGH_UB,-1(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SB_UB
	RSB

;------ SIGNED BYTE TO UNSIGNED WORD -----------------------------------
$SB_UW:	CVTBW	(R2)+,(R3)+		;convert byte to word
	BGEQ	30$			;destination word 0 or +ve ?
	MOVW	HIGH_UW,-2(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SB_UW
	RSB

;------ SIGNED WORD TO SIGNED BYTE -------------------------------------
$SW_SB:	CVTWB	(R2)+,(R3)+		;convert word/byte
	BVC	30$			;check if overflow clear
	MOVB	HIGH_SB,-1(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SW_SB
	RSB

;------ SIGNED WORD TO SIGNED LONGWORD ---------------------------------
$SW_SL:	CVTWL	(R2)+,(R3)+		;convert word/long
	SOBGTR	R1,$SW_SL
	RSB

;------ SIGNED WORD TO REAL --------------------------------------------
$SW_R:	CVTWF	(R2)+,(R3)+		;convert word/floating
	SOBGTR	R1,$SW_R
	RSB

;------ SIGNED WORD TO DOUBLE PRECISION --------------------------------
$SW_DP:	CVTWD	(R2)+,(R3)+		;convert word/double
	SOBGTR	R1,$SW_DP
	RSB

;------ SIGNED WORD TO UNSIGNED BYTE -----------------------------------
$SW_UB:	CVTWB	(R2)+,(R3)+		;convert word/byte
    	TSTB	-1(R2)			;top byte of word = 0 ?
	BEQL	30$			;if so, then no error
       	MOVB	HIGH_UB,-1(R3)		;...otherwise, load HIGH value
    	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SW_UB
	RSB

;------ SIGNED WORD TO UNSIGNED WORD -----------------------------------
$SW_UW:	MOVW	(R2)+,(R3)+		;move word
	BGEQ	30$			;destination word 0 or +ve ?
	MOVW	HIGH_UW,-2(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SW_UW
	RSB

;------ SIGNED LONGWORD TO SIGNED BYTE ---------------------------------
$SL_SB:	CVTLB	(R2)+,(R3)+		;convert longword/byte
	BVC	30$			;check if overflow clear
	MOVB	HIGH_SB,-1(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SL_SB
	RSB

;------ SIGNED LONGWORD TO SIGNED WORD ---------------------------------
$SL_SW:	CVTLW	(R2)+,(R3)+		;convert longword/word
	BVC	30$			;check if overflow clear
	MOVW	HIGH_SW,-2(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SL_SW
	RSB

;------ SIGNED LONGWORD TO REAL ----------------------------------------
$SL_R:	CVTLF	(R2)+,(R3)+		;convert longword/floating
	SOBGTR	R1,$SL_R
	RSB

;------ SIGNED LONGWORD TO DOUBLE PRECISION ----------------------------
$SL_DP:	CVTLD	(R2)+,(R3)+		;convert longword/double
	SOBGTR	R1,$SL_DP
	RSB

;------ SIGNED LONGWORD TO UNSIGNED BYTE -------------------------------
$SL_UB:	CVTLB	(R2)+,(R3)+		;convert longword/byte
	CMPZV	#8,#24,-4(R2),#0	;top 3 bytes of longword = 0 ?
	BEQL	30$			;if so, then no error
    	MOVB	HIGH_UB,-1(R3)		;...otherwise, load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SL_UB
	RSB

;------ SIGNED LONGWORD TO UNSIGNED WORD -------------------------------
$SL_UW:	CVTLW	(R2)+,(R3)+		;convert longword/word
	TSTW	-2(R2)			;top 2 bytes of longword = 0 ?
	BEQL	30$			;if so, then no error
    	MOVW	HIGH_UW,-2(R3)		;...otherwise, load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$SL_UW
	RSB

;------ REAL TO SIGNED BYTE --------------------------------------------
$R_SB:	CVTRFL	(R2)+,R0		;convert rounded floating/long
	BVC	10$			;check if overflow clear
	MOVB	HIGH_SB,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLB	R0,(R3)+		;convert longword/byte
	BVC	30$			;check if overflow clear
	MOVB	HIGH_SB,-1(R3)		;if not, then load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$R_SB
	RSB

;------ REAL TO SIGNED WORD --------------------------------------------
$R_SW:	CVTRFL	(R2)+,R0		;convert rounded floating/long
	BVC	10$			;check if overflow clear
	MOVW	HIGH_SW,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLW	R0,(R3)+		;convert longword/word
	BVC	30$			;check if overflow clear
	MOVW	HIGH_SW,-2(R3)		;if not, then load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$R_SW
	RSB

;------ REAL TO SIGNED LONGWORD ----------------------------------------
$R_SL:	CVTRFL	(R2)+,(R3)+		;convert rounded floating/long
	BVC	30$			;check if overflow clear
	MOVL	HIGH_SL,-4(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$R_SL
	RSB

;------ REAL TO DOUBLE PRECISION ---------------------------------------
$R_DP:	CVTFD	(R2)+,(R3)+		;convert floating/double
	SOBGTR	R1,$R_DP
	RSB

;------ REAL TO UNSIGNED BYTE ------------------------------------------
$R_UB:	CVTRFL	(R2)+,R0		;convert rounded floating/long
	BVC	10$			;check if overflow clear
	MOVB	HIGH_UB,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLB	R0,(R3)+		;convert longword/byte
	CMPZV	#8,#24,R0,#0		;top 3 bytes of longword = 0 ?
	BEQL	30$			;if so, then no error
    	MOVB	HIGH_UB,-1(R3)		;...otherwise, load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$R_UB
	RSB

;------ REAL TO UNSIGNED WORD ------------------------------------------
$R_UW:	CVTRFL	(R2)+,R0		;convert rounded floating/long
	BVC	10$			;check if overflow clear
	MOVW	HIGH_UW,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLW	R0,(R3)+		;convert longword/word
	CMPZV	#16,#16,R0,#0		;top 2 bytes of longword = 0 ?
	BEQL	30$			;if so, then no error
    	MOVW	HIGH_UW,-2(R3)		;...otherwise load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$R_UW
	RSB

;------ DOUBLE PRECISION TO SIGNED BYTE --------------------------------
$DP_SB:	CVTRDL	(R2)+,R0		;convert rounded double
	BVC	10$			;check if overflow clear
	MOVB	HIGH_SB,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLB	R0,(R3)+		;convert longword/byte
	BVC	30$			;check if overflow clear
	MOVB	HIGH_SB,-1(R3)		;if not, then load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$DP_SB
	RSB

;------ DOUBLE PRECISION TO SIGNED WORD --------------------------------
$DP_SW:	CVTRDL	(R2)+,R0		;convert rounded double/long
	BVC	10$			;check if overflow clear
	MOVW	HIGH_SW,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLW	R0,(R3)+		;convert longword/word
	BVC	30$			;check if overflow clear
	MOVW	HIGH_SW,-2(R3)		;if not, then load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$DP_SW
	RSB

;------ DOUBLE PRECISION TO SIGNED LONGWORD ----------------------------
$DP_SL:	CVTRDL	(R2)+,(R3)+		;convert rounded double/long
	BVC	30$			;check if overflow clear
	MOVL	HIGH_SL,-4(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$DP_SL

	RSB

;------ DOUBLE PRECISION TO REAL ---------------------------------------
$DP_R:	CVTDF	(R2)+,(R3)+		;convert double/floating
	SOBGTR	R1,$DP_R	
	RSB

;------ DOUBLE PRECISION TO UNSIGNED BYTE ------------------------------
$DP_UB:	CVTRDL	(R2)+,R0		;convert rounded double/long
	BVC	10$			;check if overflow clear
	MOVB	HIGH_UB,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLB	R0,(R3)+		;convert longword/byte
	CMPZV	#8,#24,R0,#0		;top 3 bytes of longword = 0 ?
	BEQL	30$			;if so, then no error
    	MOVB	HIGH_UB,-1(R3)		;...otherwise, load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$DP_UB
	RSB

;------ DOUBLE PRECISION TO UNSIGNED WORD ------------------------------
$DP_UW:	CVTRDL	(R2)+,R0		;convert rounded double/long
	BVC	10$			;check if overflow clear
	MOVW	HIGH_UW,(R3)+		;if not, then load HIGH value
	JMP	20$			;...and jump
10$:	CVTLW	R0,(R3)+		;convert longword/word
	CMPZV	#16,#16,R0,#0		;top 2 bytes of longword = 0 ?
	BEQL	30$			;if so, then no error
	MOVW	HIGH_UW,-2(R3)		;...otherwise, load HIGH value
20$:	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$DP_UW
	RSB

;------ UNSIGNED BYTE TO SIGNED BYTE -----------------------------------
$UB_SB:	MOVB	(R2)+,(R3)+		;move byte
	BGEQ	30$			;destination byte 0 or +ve ?
	MOVB	HIGH_SB,-1(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$UB_SB
	RSB

;------ UNSIGNED BYTE TO SIGNED WORD -----------------------------------
$UB_SW:	MOVZBW	(R2)+,(R3)+		;move zero-extended byte/word
	SOBGTR	R1,$UB_SW
	RSB

;------ UNSIGNED BYTE TO SIGNED LONGWORD -------------------------------
$UB_SL:	MOVZBL	(R2)+,(R3)+		;move zero-extended byte/long
	SOBGTR	R1,$UB_SL
	RSB

;------ UNSIGNED BYTE TO REAL ------------------------------------------
$UB_R:	MOVZBL	(R2)+,R0		;move zero-extended byte/long
	CVTLF	R0,(R3)+		;convert longword/floating
	SOBGTR	R1,$UB_R
	RSB

;------ UNSIGNED BYTE TO DOUBLE PRECISION ------------------------------
$UB_DP:	MOVZBL	(R2)+,R0		;move zero-extended byte/long
	CVTLD	R0,(R3)+		;convert longword/double
	SOBGTR	R1,$UB_DP
	RSB

;------ UNSIGNED BYTE TO UNSIGNED WORD ---------------------------------
$UB_UW:	MOVZBW	(R2)+,(R3)+		;move zero-extended byte/word
	SOBGTR	R1,$UB_UW
	RSB

;------ UNSIGNED WORD TO SIGNED BYTE -----------------------------------
$UW_SB:	CVTWB	(R2)+,(R3)+		;convert word/byte
	BVS	10$			;check if overflow set
	BGEQ	30$			;destination byte 0 or +ve ?
10$:	MOVB	HIGH_SB,-1(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$UW_SB
	RSB

;------ UNSIGNED WORD TO SIGNED WORD -----------------------------------
$UW_SW:	MOVW	(R2)+,(R3)+		;move word
	BGEQ	30$			;destination word 0 or +ve ?
	MOVW	HIGH_SW,-2(R3)		;if not, then load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$UW_SW
	RSB

;------ UNSIGNED WORD TO SIGNED LONGWORD -------------------------------
$UW_SL:	MOVZWL	(R2)+,(R3)+		;move zero-extended word/long
	SOBGTR	R1,$UW_SL
	RSB

;------ UNSIGNED WORD TO REAL ------------------------------------------
$UW_R:	MOVZWL	(R2)+,R0		;move zero-extended word/long
	CVTLF	R0,(R3)+		;convert longword/floating
	SOBGTR	R1,$UW_R
	RSB

;------ UNSIGNED WORD TO DOUBLE PRECISION ------------------------------
$UW_DP:	MOVZWL	(R2)+,R0		;move zero-extended word/long
	CVTLD	R0,(R3)+		;convert longword/double
	SOBGTR	R1,$UW_DP
	RSB

;------ UNSIGNED WORD TO UNSIGNED BYTE ---------------------------------
$UW_UB:	CVTWB	(R2)+,(R3)+		;convert word/byte
	TSTB	-1(R2)			;top byte of word = 0 ?
	BEQL	30$			;if so, then no error
	MOVB	HIGH_UB,-1(R3)		;...otherwise, load HIGH value
	INCL	(R4)			;...and increment NBAD
30$:	SOBGTR	R1,$UW_UB
	RSB

;------ NO CONVERSION (SAME TYPE) --------------------------------------
$NOCON:	MOVL	#^XFFFFFFFF,(R4)	;load NBAD with -1
	RSB

	.END
