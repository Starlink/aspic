      SUBROUTINE VRGREYS
C+
C     VRGREYS.
C
C     Generate a grey colour table with user defined end points
C     and output it as a Starlink image.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  OUTPICI, OUTPUT, CLEARIM, READI.
C   E2D:-         VRGREY1.
C
C  A C Davenhall./ROE/                                    25/10/82.
C-
      INTEGER AXIS(2),COLPTR
      INTEGER IOSTAT,PTRSTT
      INTEGER BLACK,WHITE
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
C
C    Obtain values for the end points of the colour table.
C
        CALL READI ('BLACK',' Enter black end point (1-256);',
     :               1,1,256,BLACK,IOSTAT)
        CALL READI ('WHITE',' Enter white end point (1-256);',
     :               256,BLACK,256,WHITE,IOSTAT)
C
C    Generate the colour table.
C
        CALL VRGREY1 (BLACK,WHITE,NCOL,NLEVEL,%VAL(COLPTR))
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access file successfully.',IOSTAT)
      END IF
C
C    Tidy up image.
C
      CALL CLEARIM ('COLTBL')
      END
