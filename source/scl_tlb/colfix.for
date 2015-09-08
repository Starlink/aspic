!+	Procedure COLFIX
!
!	Modifies a cursor indicated vertical line to a specified
!	value. First copies specified input frame to the output
!	frame and then modifies the line.
!
!
!	D. Tudhope/ROE/Dec 1982
!
!- *
ARGSCUR
LET DSCL_I=ARGSCUR_IMAGEN
IF DSCL_I.EQ.0 THEN GOTO ERROR                    ! error in cursor, give up
IF P1.NES."" THEN LET COLFIXP_NEWVAL='P1'         ! LEAVE ALONE IF NO ARGUMENT
LET COLFIXP_INPICT=$
LET COLFIXP_OUTPICT=$
LET COLFIXP_CX=ARGSCUR_CX
COLFIXP
CLEAR COLFIXP_NEWVAL
EXIT
ERROR:
WRITE SYS$OUTPUT "PROCEDURE ABANDONED"
