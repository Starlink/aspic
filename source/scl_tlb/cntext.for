!+	Procedure CNTEXT
!   
!
!    Purpose:-
!
!	Contour extraction.
!
!	Generate a set of contours from a predifined region of
!	an image, suitable for subsequent ellipse fitting.
!
!    Usage:-
!
!	Following the normal practice in galaxy photometry the
!	image to be contoured should be prepared in intensity
!	normalised to a sky level of 1.0, the sky not being
!	subtracted. If it is required to define the region to
!	be contoured using the cursor this should be done by
!	plotting it on the Args (eg. by ADISP) and then running
!	the program BOXASP.
!
!	CNTEXT may then be run. The user will fIrst be prompted
!	for the name of the image to be analysed, the number
!	of contours to be extracted and the name of the files to
!	hold the extracted contours and their Log I values.
!	If the region to be contoured has not been predefined it
!	will now be prompted for.
!	Finally the user will be prompted for the Log I value
!	of the faintest contour and the increment, in Log I,
!	between contours. The contours will then be extracted and
!	the procedure terminates.
!
!	A C Davenhall./ROE/                                  5/10/82.
!
!- *
LET CNTEXTP_XBASE = BOXASP_XBASE
LET CNTEXTP_YBASE = BOXASP_YBASE
LET CNTEXTP_XTOP  = BOXASP_XTOP
LET CNTEXTP_YTOP  = BOXASP_YTOP
CNTEXTP
