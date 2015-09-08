!+	DSCL procedure BONW
!
!	uses ASPIC program LUTSET to give black axes on white background
!	(for use in association with CONTOUR and any other programs using
!	SIMPLEPLOT on the ARGS)
!
!	WFL August 26 81
!
	LUTSET 0,0 255,255,255
	LUTSET 10,10 0,0,0
