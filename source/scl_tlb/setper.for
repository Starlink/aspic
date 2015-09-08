!+	Procedure SETPER
!
!	part of IAM suite
!	set the threshold (THRLD) parameter of histogram as
!	percentage above sky background. Note that the sky
!	background must have been defined first.
!
!	USES SETPERP returning results to IAMANAL and SETALLP
!	and PLOTQTH
!
!	SCLSTAT tells if program terminated successfully
!
!	D. Tudhope/ROE/Sept 1982
!
!- *
LET SETPERP_SCLSTAT=1
SETPERP
LET GLOBAL_SCLSTAT=SETPERP_SCLSTAT   !   cos of bugs in dscl
IF GLOBAL_SCLSTAT.NE.0 THEN GOTO ERROR
LET IAMANAL_THRLD=SETPERP_THRLD
LET SETALLP_THRLD=SETPERP_THRLD
LET PLOTQTH_THRLD=SETPERP_THRLD
EXIT
ERROR:
WRITE SYS$OUTPUT "PROGRAM ABORTED - NO ACTION TAKEN"
