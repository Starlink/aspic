      SUBROUTINE COLCYS
C+
C     COLCYS.
C
C     Subroutine to input a colour table held as a Starlink image
C     and produce from it an output colour consisting of a number
C     of cyclic replicas of the original colour table. The number
C     of cycles is obtained from the user.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  INPICI, OUTPICI, READI, OUTPUT, CLEARIM.
C   E2D:-         COLCY1.
C
C  A C Davenhall./ROE/                                   26/10/82.
C-
      INTEGER INPTR,OUTPTR,AXIS(2),IMSTAT,IOSTAT
      INTEGER NCOL,NLEVEL,CYCLE
C
C
C    Attempt to obtain pointers to the images;
C    ... Input image.
C
      IOSTAT=0
      IMSTAT=0
      CALL INPICI ('INCOL',
     : ' Enter filename for input colour table;',
     :   2,AXIS,INPTR,IMSTAT)
C
C    ... Output image.
C
      CALL OUTPICI ('OUTCOL',
     : ' Enter filename for output colour table;',
     :   2,AXIS,OUTPTR,IMSTAT)
C
C    Proceed if the pointers have been obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
        NCOL=AXIS(1)
        NLEVEL=AXIS(2)
C
C    Obtain the required no. of cycles from the user.
C
        CALL READI ('CYCLE',
     :   ' Enter the no. of cycles required (1-10);',
     :     5,1,10,CYCLE,IOSTAT)
C
C    Replicate the colour table.
C
        CALL COLCY1 (CYCLE,NCOL,NLEVEL,%VAL(INPTR),%VAL(OUTPTR))
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access files successfully.',IOSTAT)
      END IF
C
C    Tidy up the images.
C
      CALL CLEARIM ('INCOL')
      CALL CLEARIM ('OUTCOL')
      END
