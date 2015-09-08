      SUBROUTINE PRFPLT
C+
C     PRFPLT.
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
C   E2D:-          KOPY.
C   Graphics:-     PGRAPH.
C   Interfaces:-   INPICR, READC, OUTPUT, CLEARIM.
C
C  Structure:-
C   Attempt to obtain pointer to image.
C   If pointer obtained Ok.
C     Obtain title for listing.
C     Copy to output arrays.
C     Plot the Profile.
C   else
C     Print message saying unable to get pointer.
C   end if
C   Tidy up input profile.
C
C  A C Davenhall./ROE/                                    6/8/82.
C-
      INTEGER PROPTR,NPTS,IMSTAT,IOSTAT
      INTEGER AXIS(2)
C
      REAL RADIUS(1024),INTEN(1024)
      INTEGER MAXPTS
      PARAMETER (MAXPTS=1024)
C
      CHARACTER TITLE*30
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
        CALL READC ('TITLE',
     :   ' Enter title to appear on graph;',
     :   ' ',' ','~',TITLE,IOSTAT)
C
C    Copy to the output arrays.
C
        CALL KOPY (%VAL(PROPTR),NPTS,MAXPTS,RADIUS,INTEN)
C
C    Plot the profile.
C
        CALL PGRAPH (MAXPTS,NPTS,RADIUS,INTEN,'RADIUS',
     :               'BRIGHTNESS',TITLE)
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
