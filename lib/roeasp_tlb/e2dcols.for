      SUBROUTINE E2DCOLS
C+
C     E2DCOLS.
C
C     Generate the E2D default colour table and output it as a
C     Starlink image.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  OUTPICI, OUTPUT, CLEARIM.
C   E2D:-         E2DCOL1.
C
C  A C Davenhall./ROE/                                    25/10/82.
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
        CALL E2DCOL1 (NCOL,NLEVEL,%VAL(COLPTR))
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access file successfully.',IOSTAT)
      END IF
C
C    Tidy up image.
C
      CALL CLEARIM ('COLTBL')
      END
