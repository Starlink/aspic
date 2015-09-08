!+	Procedure SYMDIS
!
!	Remove 4 symmetrically placed disk-shaped areas from an
!	fft image given centre (CX,CY) and diameter (DIAM) of 1
!	area (from ARGSCIR ?), compute other 3 by reflection in
!	lines x=ixext/2-1 and y=iyext/2. Fft images assumed to
!	have been produced by DFFTASP as here zero frequency is
!	assumed to lie at IXEXT/2-1,IYEXT/2 in args coords 0..511
!	This is for removing noise that appears as 4 symmetric
!	clusters, one in each quadrant of fft image.
!	Call CIRFIL to fill in areas with NEWVAL.
!
!	USES SYMDISP WITH CIRCULAR CURSOR WITH INPUT FROM AND OUTPUT TO STACK
!
!	D. TUDHOPE/ROE
!
!- *
ARGSCIR
LET DSCL_I=ARGSCIR_IMAGEN
IF DSCL_I.EQ.0 THEN GOTO ERROR                    ! error in cursor, give up
IF P1.NES."" THEN LET SYMDISP_NEWVAL='P1'         ! LEAVE ALONE IF NO ARGUMENT
LET SYMDISP_INPICT=$
LET SYMDISP_OUTPICT=$
LET SYMDISP_CX=ARGSCIR_CX
LET SYMDISP_CY=ARGSCIR_CY
LET SYMDISP_DIAM=ARGSCIR_DIAM
SYMDISP
CLEAR SYMDISP_NEWVAL
EXIT
ERROR:
WRITE SYS$OUTPUT "PROCEDURE ABANDONED"
