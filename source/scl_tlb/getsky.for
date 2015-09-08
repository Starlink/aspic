!+	Procedure GETSKY
!
!	sets IAM parameter SKY in master .CON file IAMANALP.CON
!	(and SETPERP) by calling general IAM parameter setting
!	program GETPAR which uses cursor on a previously plotted
!	histogram.
!
!	SCLSTAT tells if program terminated successfully
!
!	D. Tudhope/ROE/Sept 1982
!
!- *
LET GETPAR_SCLSTAT=1
GETPAR
LET GLOBAL_SCLSTAT=GETPAR_SCLSTAT   !   cos of bugs in dscl
IF GLOBAL_SCLSTAT.NE.0 THEN GOTO ERROR
LET IAMANAL_SKY=GETPAR_PAR
LET SETPERP_SKY=GETPAR_PAR
LET SETALLP_SKY=GETPAR_PAR
EXIT
ERROR:
WRITE SYS$OUTPUT "PROGRAM ABORTED - NO ACTION TAKEN"
