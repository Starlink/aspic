      SUBROUTINE TARGETS
C+
C     TARGETS.
C
C     Subroutine to display a "Star catalogue" held as a Starlink
C     image on an Args. The catalogue must be displayed on top of the
C     image frame containing the object in the catalogue.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  INPICR, OUTPUT, READR, READI, CLEARIM.
C   SIMIM:-       TARGT1.
C
C  Structure:-
C   Attempt to obtain pointer to the catalogue.
C   If pointer obtained Ok
C     indicate the no. of stars in the catalogue to the user.
C     Prompt for plotting threshold (magnitudes)
C     Prompt for Args overlay plane required.
C     Display catalogue
C     If error is made in display
C       print message.
C     end if
C   else
C     print message.
C   end if
C   tidy up image.
C
C  A C Davenhall./ROE/                                     3/1/83.
C-
      INTEGER CATSTT,CATPTR,AXIS(2),CATPAR,STARS
C
      INTEGER IOSTAT,DISTAT
C
      INTEGER PLANE
      REAL THRESH
C
C    Colour for display.
C
      CHARACTER COLOUR*1
      PARAMETER (COLOUR='Y')
C
      CHARACTER BUFFER*60
C
C
      IOSTAT=0
C
C    Attempt to obtain a pointer to the Star catalogue.
C
      CATSTT=0
      CALL INPICR ('CATAL',
     :  ' Enter name of the Star Catalogue file;',
     :    2,AXIS,CATPTR,CATSTT)
      CATPAR=AXIS(1)
      STARS=AXIS(2)
C
C    Proceed if the pointer has been obtained Ok.
C
      IF (CATSTT.EQ.0.AND.CATPAR.GE.3) THEN
C
C    Indicate the number of stars in the catalogue to the user.
C
        WRITE(BUFFER,2000) STARS
 2000   FORMAT(1X,'No. of stars in the catalogue = ',I5)
        CALL OUTPUT (BUFFER,IOSTAT)
C
C    Obtain the threshold from the user.
C
        CALL READR ('THRESH',
     :    ' Enter plotting threshold (magnitudes);',
     :      2.0E1,-5.0E0,4.0E1,THRESH,IOSTAT)
C
C    Obtain the required overlay plane from the user.
C
        CALL READI ('PLANE',
     
     :   ' Enter Args overlay plane required for the display;',
     :     9,8,15,PLANE,IOSTAT)
C
C    Display the catalogue.
C
        DISTAT=0
        CALL TARGT1 (CATPAR,STARS,%VAL(CATPTR),THRESH,PLANE,COLOUR,
     :               DISTAT)
        IF (DISTAT.NE.0) THEN
          CALL OUTPUT (
     :   ' ***ERROR***; Unable to display catalogue successfully.',
     :                  IOSTAT)
          CALL OUTPUT (
     :   '              Check that the correct image frame is ',IOSTAT)
          CALL OUTPUT (
     :   '              displayed on the Args.',IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR***; Unable to access catalogue file successfully.',
     :       IOSTAT)
      END IF
C
C    Tidy up the image frame.
C
      CALL CLEARIM ('CATAL')
      END
