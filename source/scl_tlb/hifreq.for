!+	Procedure HIFREQ
!
!	Eliminates high frequencies from fft mode image
!	fft images should be produced by DFFTASP as this is
!	geared for its output viz zero frequency assumed to
!	lie at IXEXT/2-1,IYEXT/2 in args coords 0..511
!
!	OUTSETSUB called to put everything outside circle to 0
!	Size of circle derived from ARGSCIR
!
!	USES HIFREQP WITH CIRCULAR CURSOR WITH INPUT FROM AND
!	OUTPUT TO STACK
!
!	D. TUDHOPE/ROE
!
!- *
ARGSCIR
LET DSCL_I=ARGSCIR_IMAGEN
IF DSCL_I.EQ.0 THEN GOTO ERROR                    ! error in cursor, give up
LET HIFREQP_INPICT=$
LET HIFREQP_OUTPICT=$
LET HIFREQP_DIAM=ARGSCIR_DIAM
HIFREQP
EXIT
ERROR:
WRITE SYS$OUTPUT "PROCEDURE ABANDONED"
