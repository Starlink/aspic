!+	Procedure SETLIM
!
!	part of IAM suite
!	change HMIN and HMAX, zero point and value of maximum
!	bin in histogram by typing in new values.
!
!	USES SETLIMP returning results to IAMHISP and GETLIMP
!
!	SCLSTAT tells if program terminated successfully
!
!	D. Tudhope/ROE/1982
!
!- *
LET SETLIMP_SCLSTAT=1
SETLIMP
LET GLOBAL_SCLSTAT=SETLIMP_SCLSTAT   !   cos of bugs in dscl
IF GLOBAL_SCLSTAT.NE.0 THEN GOTO ERROR
LET IAMHISP_HMIN=SETLIMP_HMIN
LET IAMHISP_HMAX=SETLIMP_HMAX
LET GETLIMP_HMIN=SETLIMP_HMIN
LET GETLIMP_HMAX=SETLIMP_HMAX
EXIT
ERROR:
WRITE SYS$OUTPUT "PROGRAM ABORTED - NO ACTION TAKEN"
