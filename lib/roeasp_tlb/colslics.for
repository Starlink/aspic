      SUBROUTINE COLSLICS
C+
C     COLSLICS.
C
C     Generate a linear mono-chrome ramp across intensity range of the
C     Args. The minimum intensity is black and the maximum intensity
C     has user specified intensities in each of the three guns.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  OUTPICI, OUTPUT, CLEARIM, READI.
C   E2D:-         COLSLIC1.
C
C  A C Davenhall./ROE/                                    25/2/83.
C-
      INTEGER AXIS(2),COLPTR
      INTEGER IOSTAT,PTRSTT
      INTEGER RED,GREEN,BLUE
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
C    Obtain the colour at max. intensity.
C
C    ...red gun.
C
        CALL READI ('RED',' Enter maximum intensity for red gun;',
     :               256,1,256,RED,IOSTAT)
C
C    ...green gun.
C
        CALL READI ('GREEN',' Enter maximum intensity for green gun;',
     :               1,1,256,GREEN,IOSTAT)
C
C    ...blue gun.
C
        CALL READI ('BLUE',' Enter maximum intensity for blue gun;',
     :               256,1,256,BLUE,IOSTAT)
C
C    Generate colour table.
C
        CALL COLSLIC1 (RED,GREEN,BLUE,%VAL(COLPTR))
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access file successfully.',IOSTAT)
      END IF
C
C    Tidy up image.
C
      CALL CLEARIM ('COLTBL')
      END
