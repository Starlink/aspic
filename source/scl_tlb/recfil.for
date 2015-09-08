!+	Procedure RECFIL
!
!    sets pixels inside a specified rectangle to NEWVAL
!    rectangle specified by opposite corners (XO,YO) (XT,YT)
!    first copies INPICT to OUTPICT and then modifies OUTPICT
!
!    USES RECFILP with 2 calls on cursor, input and output
!    to stack
!
!    D. TUDHOPE/ROE
!
!- *
ARGSCUR
LET RECFILP_XO=ARGSCUR_CX
LET RECFILP_YO=ARGSCUR_CY
LET DSCL_I1=ARGSCUR_IMAGEN
ARGSCUR
LET DSCL_I2=ARGSCUR_IMAGEN
IF DSCL_I1.NE.DSCL_I2 THEN GOTO ERROR     ! check 2 cursors on same image
IF DSCL_I1.EQ.0 THEN GOTO ERROR           ! check cursors actually on an image
LET RECFILP_XT=ARGSCUR_CX
LET RECFILP_YT=ARGSCUR_CY
IF P1.NES."" THEN LET RECFILP_NEWVAL='P1'
LET RECFILP_INPICT=$
LET RECFILP_OUTPICT=$
RECFILP
CLEAR RECFILP_NEWVAL
EXIT
ERROR:
WRITE SYS$OUTPUT "BOTH CURSORS MUST BE ON THE SAME IMAGE !"
