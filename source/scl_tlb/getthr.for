!+	Procedure GETTHR
!
!	sets IAM parameter THRLD in master .CON file IAMANAL.CON
!	by calling general IAM parameter setting program GETPAR
!	which uses cursor on a previously plotted histogram.
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
LET IAMANAL_THRLD=GETPAR_PAR
LET SETALLP_THRLD=GETPAR_PAR
LET PLOTQTH_THRLD=GETPAR_PAR
EXIT
ERROR:
WRITE SYS$OUTPUT "PROGRAM ABORTED - NO ACTION TAKEN"
