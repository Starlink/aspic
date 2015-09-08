       SUBROUTINE BOXS
C+
C     BOXS.
C
C     Subroutine  define a box on an image drawn on the Args
C     using the cross-hair cursor and to write its
C     coordinates to the environment.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- WRITEI, OUTPUT.
C   E2D:-        RECUR.
C
C  A C Davenhall./ROE/                               13/9/82.
C-
      INTEGER XBASE,YBASE,XTOP,YTOP,STATUS,IOSTAT
C
      INTEGER PLANE1,PLANE2
      PARAMETER (PLANE1=10)
      PARAMETER (PLANE2=11)
      CHARACTER COL1*1,COL2*1
      PARAMETER (COL1='W')
      PARAMETER (COL2='G')
C
C
      IOSTAT=0
      STATUS=0
      CALL RECUR (PLANE1,COL1,PLANE2,COL2,XBASE,YBASE,
     :            XTOP,YTOP,STATUS)
      IF (STATUS.EQ.0) THEN
        CALL WRITEI ('XBASE',XBASE,IOSTAT)
        CALL WRITEI ('YBASE',YBASE,IOSTAT)
        CALL WRITEI ('XTOP',XTOP,IOSTAT)
        CALL WRITEI ('YTOP',YTOP,IOSTAT)
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Invalid box. Try again.',IOSTAT)
      END IF
      END
