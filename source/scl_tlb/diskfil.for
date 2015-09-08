!+	Procedure DISKFIL
!
! 	Removes a disk-shaped area from an fft modes image
!	So sets values inside given circle to 'newval' by
!	calling CIRFIL. Circle centred on CX,CY with diameter
!	DIAM (from ARGSCIR ?)
!
!	USES PROGRAM DISKFILP WITH CIRCULAR CURSOR WITH INPUT
!	FROM AND OUTPUT TO STACK
!
!	D. Tudhope/ROE
!
!- *
ARGSCIR
LET DSCL_I=ARGSCIR_IMAGEN
IF DSCL_I.EQ.0 THEN GOTO ERROR                    ! error in cursor, give up
IF P1.NES."" THEN LET DISKFILP_NEWVAL='P1'         ! LEAVE ALONE IF NO ARGUMENT
LET DISKFILP_INPICT=$
LET DISKFILP_OUTPICT=$
LET DISKFILP_CX=ARGSCIR_CX
LET DISKFILP_CY=ARGSCIR_CY
LET DISKFILP_DIAM=ARGSCIR_DIAM
DISKFILP
CLEAR DISKFILP_NEWVAL
EXIT
ERROR:
WRITE SYS$OUTPUT "PROCEDURE ABANDONED"
