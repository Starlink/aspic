      SUBROUTINE FITELLS
C+
C     FITELLS.
C
C     Subroutine to fit ellipses to a series of contours held
C     as a Starlink image and save the parameters of the
C     ellipses fitted as a Starlink image.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- INPICR, OUTPICR, OUTPUT, CLEARIM.
C   E2D:-        ELLFIT.
C
C  Structure:-
C   Attempt to obtain pointer to file of contours.
C   Attempt to obtain pointer to file holding intensity of each contour.
C   Attempt to obtain pointer to output file of fitted parameters.
C   If pointers obtained Ok
C     perform fit
C   else
C     print message
C   end if
C   tidy up images.
C
C  A C Davenhall./ROE/                                   23/9/82.
C-
      INTEGER AXIS1(3),AXIS2(1),AXIS3(2)
      INTEGER CNTPTR,LGIPTR,PARPTR
      INTEGER IMSTAT,IOSTAT
      INTEGER MAXPTS,MAXPOS,MAXCNT,PARMS
      PARAMETER (PARMS=6)
      CHARACTER BUFFER*70
C
C
C    Attempt to obtain pointer to file holding contours.
C
      IOSTAT=0
      IMSTAT=0
      CALL INPICR ('CONTOURS',
     : ' Enter name of file holding contours to be fitted;',
     :   3,AXIS1,CNTPTR,IMSTAT)
C
C    Attempt to obtain pointer to file holding Log I values.
C
      CALL INPICR ('LOGI',
     : ' Enter name of file holding contour intensity levels;',
     :   1,AXIS2,LGIPTR,IMSTAT)
C
C    Attempt to obtain pointer to an output file to hold the 
C    ellipse parameters determined for each contour.
C
      AXIS3(1)=PARMS
      AXIS3(2)=AXIS1(3)
      CALL OUTPICR ('ELLIPSES',
     : ' Enter name of file to hold ellipse parameters;',
     :   2,AXIS3,PARPTR,IMSTAT)
C
C    Proceed if pointers obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
        MAXPAR=AXIS1(1)
        MAXPOS=AXIS1(2)
        MAXCNT=AXIS1(3)
        WRITE(BUFFER,2000) MAXCNT
 2000   FORMAT(1X,'No. of contours to be fitted = ',I3,
     :         '. Please wait.')
        CALL OUTPUT (BUFFER,IOSTAT)
        CALL OUTPUT ('  ',IOSTAT)
C
C    Perform the fit.
C
        CALL ELLFIT (MAXPAR,MAXPOS,MAXCNT,%VAL(CNTPTR),%VAL(LGIPTR),
     :               PARMS,%VAL(PARPTR))
        CALL OUTPUT (' Fitting complete.',ISOTAT)
      ELSE
        CALL OUTPUT (
     : ' ***ERROR Unable to input files successfully.',IOSTAT)
      END IF
C
C    Tidy up images.
C
      CALL CLEARIM ('CONTOURS')
      CALL CLEARIM ('LOGI')
      CALL CLEARIM ('ELLIPSES')
      END
