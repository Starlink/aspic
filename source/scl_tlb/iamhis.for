!+	Procedure IAMHIS
!
!       part of IAM suite of programs for parameterised information
!	computes intensity histogram of image and plots it to given
!	quadrant of ARGS. If this has not been called before this
!	session (HMAX=HMIN=0), then HISTDEF called to compute default
!	min and max bins for histogram, but these values can be
!	overwritten (elsewhere) by user to a better range.
!
!	SCLSTAT tells if program terminated successfully
!
!	D. Tudhope/ROE/Sept 1982
!
!- *
LET IAMHISP_SCLSTAT=1
IAMHISP
LET GLOBAL_SCLSTAT=IAMHISP_SCLSTAT   !   cos of bugs in dscl
IF GLOBAL_SCLSTAT.NE.0 THEN GOTO ERROR
LET GETPAR_HMIN=IAMHISP_HMIN
LET GETPAR_HMAX=IAMHISP_HMAX
LET GETLIMP_HMIN=IAMHISP_HMIN
LET GETLIMP_HMAX=IAMHISP_HMAX
LET SETLIMP_HMIN=IAMHISP_HMIN
LET SETLIMP_HMAX=IAMHISP_HMAX
LET SETLIMP_HMIND=IAMHISP_HMIND
LET SETLIMP_HMAXD=IAMHISP_HMAXD
EXIT
ERROR:
WRITE SYS$OUTPUT "PROGRAM ABORTED - NO ACTION TAKEN"
