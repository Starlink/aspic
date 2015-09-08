      PROGRAM IMSIZE
C+
C     Program:- IMSIZE.
C
C     Program to determine the dimensionality and size of a Starlink
C     image.
C
C     Note; the maximum permitted image dimensionality is 100.
C
C     Subroutines called;
C      Interfaces:-   OUTPUT, CLEARIM.
C      Starlink:-     RDIMAG, RDDSCR.
C
C     A C Davenhall./ROE/                              21/12/82.
C-
      INTEGER NAXES,AXES,IMPTR,IOSTAT,IMSTAT
      INTEGER STAT1,JDUM
      CHARACTER BUFFER*65, TITLE*30
C
C    Declare the max. permitted image dimensionality.
C
      PARAMETER (NAXES=100)
      INTEGER AXISIZ(NAXES)
C
C    Attempt to obtain a pointer to the image.
C
      IMSTAT=0
      CALL OUTPUT (' Give required image;',IOSTAT)
      CALL RDIMAG ('IMAGE',204,NAXES,AXISIZ,AXES,IMPTR,IMSTAT)
C
C    Proceed to display information on the image if the pointer
C    has been obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
C
C    Display the image title, if any exits.
C
        CALL RDDSCR ('IMAGE','TITLE',1,TITLE,JDUM,STAT1)
        IF (STAT1.EQ.0) THEN
          CALL OUTPUT (TITLE,IOSTAT)
        END IF
        CALL OUTPUT ('  ',IOSTAT)
C
C    Display the image dimensionality and size.
C
        IF (AXES.LE.0) THEN
          WRITE(BUFFER,2000) AXES
 2000     FORMAT(1X,'***ERROR Dimensionality of image = ',I4)
          CALL OUTPUT (BUFFER,IOSTAT)
        END IF
C
        IF (AXES.EQ.1) THEN
          WRITE(BUFFER,2001) AXISIZ(1)
 2001     FORMAT(1X,'One dimensional image, size = ',I6,' elements.')
          CALL OUTPUT (BUFFER,IOSTAT)
        END IF
C
        IF (AXES.EQ.2) THEN
          WRITE(BUFFER,2002) AXISIZ(1),AXISIZ(2)
 2002     FORMAT(1X,'Two dimensional image.  X extent = ',I5,2X,
     :           'Y extent = ',I5)
          CALL OUTPUT (BUFFER,IOSTAT)
        END IF
C
        IF (AXES.GE.3) THEN
          WRITE(BUFFER,2003) AXES
 2003     FORMAT(1X,'Dimensionality of image = ',I3)
          CALL OUTPUT (BUFFER,IOSTAT)
          CALL OUTPUT ('  ',IOSTAT)
          DO I=1,AXES
            WRITE(BUFFER,2004) I,AXISIZ(I)
 2004       FORMAT(1X,'Dimension ',I3,'  size = ',I5,' elements.')
            CALL OUTPUT (BUFFER,IOSTAT)
          END DO
        END IF
      ELSE
        CALL OUTPUT (' ***ERROR Unable to obtain image successfully.',
     :                 IOSTAT)
      END IF
      CALL CLEARIM ('IMAGE')
      END
