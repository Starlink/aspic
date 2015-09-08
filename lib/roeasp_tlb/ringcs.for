      SUBROUTINE RINGCS
C+
C     RINGCS.
C
C     Generate a continuous purple, blue, green, red, purple
C     colour table with the same colour at either end.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  OUTPICI, OUTPUT, CLEARIM.
C   E2D:-         RINGC1.
C
C  A C Davenhall./ROE/                                    27/10/82.
C-
      INTEGER AXIS(2),COLPTR
      INTEGER IOSTAT,PTRSTT
C
C    Set up the size of the colour table array;
C    256 increments in each of 3 colours.
C
      INTEGER NCOL,NLEVEL
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
        CALL RINGC1 (NCOL,NLEVEL,%VAL(COLPTR))
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access file successfully.',IOSTAT)
      END IF
C
C    Tidy up image.
C
      CALL CLEARIM ('COLTBL')
      END
