      SUBROUTINE ELLPRTS
C+
C     ELLPRTS.
C
C     Subroutine to take a Starlink image holding parameters
C     for a set of ellipses, generated using ELLFITP
C     and list them on the lineprinter.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- INPICR, READC, OUTPUT, CLEARIM.
C   E2D:-        ELLPRT.
C
C  Structure:-
C   Attempt to obtain pointer to parameter file.
C   If pointer obtained ok
C     obtain title
C     print file
C   else
C     print error message.
C   end if
C   tidy up image file.
C
C  A C Davenhall./ROE/                                   23/9/82.
C-
      INTEGER AXIS(2),PARPTR
      INTEGER IMSTAT,IOSTAT,PRSTAT
      INTEGER PARMS,MAXCNT
      CHARACTER TITLE*20
C
C
C    Attempt to a obtain a pointer to the file of parameters.
C
      IMSTAT=0
      IOSTAT=0
      CALL INPICR ('ELLIPSES',
     : ' Enter name of file holding ellipse parameters;',
     :   2,AXIS,PARPTR,IMSTAT)
C
C    Proceed if the pointer has been obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
        PARMS=AXIS(1)
        MAXCNT=AXIS(2)
        CALL READC ('TITLE',
     :   ' Enter the title to appear on the listing;',
     :   ' ',' ','~',TITLE,IOSTAT)
C
C    Attempt to print the file.
C
        PRSTAT=0
        CALL ELLPRT (PARMS,MAXCNT,%VAL(PARPTR),TITLE,PRSTAT)
        IF (PRSTAT.EQ.0) THEN
          CALL OUTPUT (
     :     ' File printed successfully.',IOSTAT)
        ELSE
          CALL OUTPUT (
     :     ' ***ERROR Unable to print file succesfully.',IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain file successfully.',IOSTAT)
      END IF
C
C    Tidy up image.
C
      CALL CLEARIM ('ELLIPSES')
      END
