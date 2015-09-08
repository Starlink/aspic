!+	Procedure APLOT
!
!	Plots an image to the ARGS scaled up or down to fit the whole
!	image onto the args screen (or the larger dimension if the
!	image is not square). As scaling is involved, the coordinates
!	returned from the cursor will only be approximate and should
!	not be used for accurate work.
!
!	Variables are set to allow APLOTRNG to be called after use of
!	APLOT
!
!	N.B. When scaling down, averaging is done very simply over a
!	     2*2 area (to be quick)
!
!
!	D. Tudhope/ROE/Dec 1982
!   
!- *
IF P1.NES."" THEN LET APLOTP_INPIC1='P1'
APLOTP
LET APLOTRNG_TZEROD=APLOTP_TZEROD_
LET APLOTRNG_TMAXD=APLOTP_TMAXD_
CLEAR APLOTP_INPIC1
