!+	Procedure SETMAG
!
!	part of IAM suite
!	set the IAM sky background magnitude in one pixel
!	USES SETMAGP returning results to IAMANAL and SETALLP
!
!	SCLSTAT tells if program terminated successfully
!
!	D. Tudhope/ROE/Sept 1982
!
!- *
LET SETMAGP_SCLSTAT=1
SETMAGP
LET GLOBAL_SCLSTAT=SETMAGP_SCLSTAT   !   cos of bugs in dscl
IF GLOBAL_SCLSTAT.NE.0 THEN GOTO ERROR
LET IAMANAL_SKYMAG=SETMAGP_SKYMAG
LET SETALLP_SKYMAG=SETMAGP_SKYMAG
EXIT
ERROR:
WRITE SYS$OUTPUT "PROGRAM ABORTED - NO ACTION TAKEN"
