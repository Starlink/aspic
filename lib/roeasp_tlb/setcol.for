      SUBROUTINE SETCOL (IND1,IND2,COLR,COLG,COLB,COLOUR)
C+
C      SETCOL.
C
C      Subroutine to set a specified range of a colour table to
C      a given colour.
C
C
C  Given;
C  IND1    (I)  First index of range to be set.
C  IND2    (I)  Second index of range to be set.
C  COLR    (I)  Intensity for red gun (0 - 255).
C  COLG    (I)      "      "  green " (   "   ).
C  COLB    (I)      "      "  blue  " (   "   ).
C
C  Returned;
C  COLOUR  (I)  Colour table array with selected range set to
C               specified colour.
C
C  Subroutines called; None.
C  A C Davenhall./ROE/                   15/2/82.
C-
      INTEGER IND1,IND2,COLR,COLG,COLB
      INTEGER COLOUR(3,256)
C
      INTEGER I1,I2,COLR1,COLG1,COLB1
C
C    Force inidices to be within permitted range.
C
      I1=MAX(IND1,1)
      I1=MIN(I1,256)
      I2=MAX(IND2,1)
      I2=MIN(I2,256)
C
C    Force the colours to be within the permitted range.
C
      COLR1=MAX(COLR,0)
      COLR1=MIN(COLR1,255)
      COLG1=MAX(COLG,0)
      COLG1=MIN(COLG1,255)
      COLB1=MAX(COLB,0)
      COLB1=MIN(COLB1,255)

C
C    Now setup section of colour table to specified colour.
C
      DO I=I1,I2
        COLOUR(1,I)=COLR1
        COLOUR(2,I)=COLG1
        COLOUR(3,I)=COLB1
      END DO
      END
