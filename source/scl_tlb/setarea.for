!+	Procedure SETAREA
!
!	part of IAM suite, set the IAM area cut.
!
!	SCLSTAT tells if program terminated successfully
!
!	D. Tudhope/ROE/Sept 1982
!
!- *
LET SETAREAP_SCLSTAT=1
SETAREAP
LET GLOBAL_SCLSTAT=SETAREAP_SCLSTAT   !   cos of bugs in dscl
IF GLOBAL_SCLSTAT.NE.0 THEN GOTO ERROR
LET IAMANAL_AREA=SETAREAP_AREA
LET SETALLP_AREA=SETAREAP_AREA
EXIT
ERROR:
WRITE SYS$OUTPUT "PROGRAM ABORTED - NO ACTION TAKEN"
