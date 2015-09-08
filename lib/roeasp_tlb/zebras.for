      SUBROUTINE ZEBRAS
C+
C     ZEBRAS.
C
C     Generate a black and white "pseudo contour" colour table
C     and save it as a Starlink image.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  OUTPICI, OUTPUT, CLEARIM, READI.
C   E2D:-         ZEBRA1.
C
C  A C Davenhall./ROE/                                    26/10/82.
C-
      INTEGER AXIS(2),COLPTR
      INTEGER IOSTAT,PTRSTT
C
C    Set up the size of the colour table array;
C    256 increments in each of 3 colours.
C
      INTEGER NCOL,NLEVEL,NUMCON
      PARAMETER (NCOL=3)
      PARAMETER (NLEVEL=256)
C
C
C    Attempt to obtain a pointer to the Starlink image to hold
C    the colour table.
C
      PTRSTT=0
      AXIS(1)=NCOL
      AXIS(2)=NLEVEL
      CALL OUTPICI ('COLTBL',
     : ' Enter name of file to hold colour table;',
     :   2,AXIS,COLPTR,PTRSTT)
C
C    Proceed to generate the colour table if the pointer has been
C    obtained Ok.
C
      IF (PTRSTT.EQ.0) THEN
C
C    Obtain the no. of pseudo contours required from the user.
C
        CALL READI ('NUMCON',' Enter no. of contours required;',
     :               5,1,30,NUMCON,IOSTAT)
C
C    Generate colour table.
C
        CALL ZEBRA1 (NUMCON,NCOL,NLEVEL,%VAL(COLPTR))
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access file successfully.',IOSTAT)
      END IF
C
C    Tidy up image.
C
      CALL CLEARIM ('COLTBL')
      END
