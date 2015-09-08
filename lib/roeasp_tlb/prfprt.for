      SUBROUTINE PRFPRT
C+
C     PRFPRT.
C
C     Subroutine to read in a profile held as a Starlink image
C     and list it on the lineprinter. The profile may be listed
C     exactly as input, or if it is held as intensity relative
C     to a sky of 1.0 it may be converted to magnitudes/sq.
C     arcsec.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-          KOPY, KOPY2, FILPRT.
C   Interfaces:-   INPICR, READC, YESNO, READR, OUTPUT, CLEARIM.
C
C  Structure:-
C   Attempt to obtain pointer to image.
C   If pointer obtained Ok.
C     Obtain title for listing.
C     Inquire if profile is to be converted to mag/sq.arcsec before
C       printing.
C     If to be simply printed
C       Copy to output arrays.
C     else
C       Obtain sky brightness.
C       Convert and copy to output arrays.
C     end if
C     print output array.
C   else
C     Print message saying unable to get pointer.
C   end if
C   Tidy up input profile.
C
C  A C Davenhall./ROE/                                    1/8/82.
C-
      INTEGER PROPTR,NPTS,IMSTAT,IOSTAT,PRSTAT
      INTEGER AXIS(2)
C
      REAL RADIUS(1024),INTEN(1024)
      INTEGER MAXPTS
      PARAMETER (MAXPTS=1024)
C
      CHARACTER TITLE1*30,TITLE2*30,REPLY*10,HEAD1*15,HEAD2*15
      REAL SKY
C
C
C    Attempt to obtain pointer to Starlink image holding profile.
C
      IMSTAT=0
      CALL INPICR ('PROFILE',
     :  ' Enter filename for profile;',2,AXIS,PROPTR,IMSTAT)
C
C    Proceed if status Ok.
C
      IF (IMSTAT.EQ.0) THEN
C
C    Determine the number of points in the profile.
C
        NPTS=AXIS(1)
        NPTS=MIN(NPTS,MAXPTS)
C
C    Obtain the title.
C
        CALL READC ('TITLE1',
     :   ' Enter title to appear on printout;',
     :   ' ',' ','~',TITLE1,IOSTAT)
C
C    Inquire whether or not the profile is to be converted to
C    mag./sq. arcsec prior to output.
C
        CALL YESNO (
     :   ' Is the profile to be output as mag./sq.arcsec?',
     :   'N',REPLY,IOSTAT)
        IF (REPLY.EQ.'N') THEN
C
C    Simply copy the profile to the output arrays.
C
          TITLE2='Extracted Profile.'
          HEAD1='Radius'
          HEAD2='Value'
          CALL KOPY (%VAL(PROPTR),NPTS,MAXPTS,RADIUS,INTEN)
        ELSE
C
C    Convert to mag./sq.arcsec prior to output.
C
C    Obtain the sky brightness.
C
          CALL READR ('SKY',
     :     ' Enter sky brightness (mag.sq./arcsec);',
     :       20.55,10.0,30.0,SKY,IOSTAT)
          WRITE(TITLE2,2000) SKY
 2000     FORMAT('Sky brightness = ',0PF7.3)
          HEAD1='Radius'
          HEAD2='Mu'
          CALL KOPY2 (%VAL(PROPTR),NPTS,SKY,MAXPTS,RADIUS,INTEN)
        END IF
C
C    Print out the arrays.
C
        PRSTAT=0
        CALL FILPRT (TITLE1,TITLE2,HEAD1,HEAD2,MAXPTS,NPTS,INTEN,
     :               RADIUS,PRSTAT)
        IF (PRSTAT.EQ.0) THEN
          CALL OUTPUT (' Profile printed Ok.',IOSTAT)
        ELSE
          CALL OUTPUT (' ***ERROR Profile not printed successfully.',
     :                   IOSTAT)
        END IF
      ELSE
C
C    Unable to obtain a pointer to the profile.
C
        CALL OUTPUT (' ***ERROR Unable to obtain file successfully.',
     :                 IOSTAT)
      END IF
C
C    Tidy up the image.
C
      CALL CLEARIM ('PROFILE')
      END
