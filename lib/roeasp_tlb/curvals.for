      SUBROUTINE CURVALS
C+
C     CURVALS.
C
C     Return the coordinates and value of a cursor defined
C     pixel in a Starlink image.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interface:-  INPICR, READI, WRITER, OUTPUT, CLEARIM.
C   E2D:-        CURVAL1.
C
C  A C Davenhall./ROE/                               28/10/82.
C-
      INTEGER XEXT,YEXT,AXIS(2),ARRPTR
      INTEGER IOSTAT,ARSTAT
      INTEGER XPOS,YPOS
      REAL    VALUE
      CHARACTER BUFFER*60
C
C
      IOSTAT=0
C
C    Attempt to obtain a pointer to the required array.
C   
      ARSTAT=0
      CALL INPICR ('ARRAY',
     :  ' Enter file name of the required image;',
     :    2,AXIS,ARRPTR,ARSTAT)
C
C    Proceed if the pointer has been obtained Ok.
C
      IF (ARSTAT.EQ.0) THEN
        XEXT=AXIS(1)
        YEXT=AXIS(2)
C
C    Display the cursor and return the position and value of the
C    chosen point.
C
        CALL CURVAL1 (XEXT,YEXT,%VAL(ARRPTR),XPOS,YPOS,VALUE)
C
C    Write out the values to the environment.
C
        CALL WRITEI ('XPOS',XPOS,IOSTAT)
        CALL WRITEI ('YPOS',YPOS,IOSTAT)
        CALL WRITER ('VALUE',VALUE,IOSTAT)
C
C    Write the values to the user.
C
        CALL OUTPUT ('  ',IOSTAT)
        WRITE(BUFFER,2000) XPOS,YPOS,VALUE
 2000   FORMAT(1X,'X coord. = ',I4,2X,'Y coord. = ',I4,2X,
     :         'Value = ',1PE12.3)
        CALL OUTPUT (BUFFER,IOSTAT)
        CALL OUTPUT ('  ',IOSTAT)
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain image successfully.',IOSTAT)
      END IF
C
C    Tidy up the image.
C
      CALL CLEARIM ('ARRAY')
      END
