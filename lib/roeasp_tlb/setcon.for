      SUBROUTINE SETCON (CENT,WID,COLR,COLG,COLB,COLOUR)
C+
C      SETCON.
C
C      Subroutine to add a continuously varying slice of colour,
C      peaked around a given value, and of given width, to the
C      colour table.
C
C  Given;
C   CENT   (I)   Value of the colour table at which the colour is
C                to be most intense.
C   WID    (I)   Total width of the colour table.
C   COLR   (I)   Max. intensity for Red gun.
C   COLG   (I)   Max. intensity for Green gun.
C   COLB   (I)   Max. intensity for Blue gun.
C   COLOUR (IA)  Colour table input.
C   
C  Returned;
C   COLOUR (IA)  Colour table input with the slice of colour added.
C
C  Subroutines called;
C   None.
C
C  Structure;
C   Force the colours input to be within the permitted range.
C   Force the central intensity to lie inside the colour table.
C   Force the width of the slice to be sensible.
C   Compute the range of the colour table where the slice will
C     be adding colour.
C   Do for all pts. where addition is necessary
C     Compute the intensity in each gun
C     add intensity to the value already in the table.
C   end do
C   Set supersaturated entries to the top of the table.
C
C   A C Davenhall./ROE/                                    3/5/82.
C-
      INTEGER CENT,WID,COLR,COLG,COLB
      INTEGER COLOUR(3,256)
C
C
      INTEGER CENT1,WID1,COLR1,COLG1,COLB1,I1,I2
      REAL RATIO
C
C      Force the colours input to be within the permitted range.
C
      COLR1=MAX(COLR,0)
      COLR1=MIN(COLR1,255)
      COLG1=MAX(COLG,0)
      COLG1=MIN(COLG1,255)
      COLB1=MAX(COLB,0)
      COLB1=MIN(COLB1,255)
C
C      Force the central intensity to be in the colour table.
C
      CENT1=MAX(CENT,1)
      CENT1=MIN(CENT1,256)
C
C      Force the width of the slice to be sensible.
C
      WID1=MAX(WID,1)
C
C      Compute the range of the colour table over which colours
C      are to be added.
C
      I1=CENT1-WID1
      I1=MAX(1,I1)
      I2=CENT1+WID1
      I2=MIN(I2,256)
C
C      Add colours of the appropriate intensity.
C
      DO I=I1,CENT1-1
        RATIO=1.0E0-((FLOAT(CENT1-I))/(FLOAT(WID1)))
        COLOUR(1,I)=COLOUR(1,I)+NINT(FLOAT(COLR1)*RATIO)
        COLOUR(2,I)=COLOUR(2,I)+NINT(FLOAT(COLG1)*RATIO)
        COLOUR(3,I)=COLOUR(3,I)+NINT(FLOAT(COLB1)*RATIO)
      END DO
      COLOUR(1,CENT1)=COLR1
      COLOUR(2,CENT1)=COLG1
      COLOUR(3,CENT1)=COLB1
      DO I=CENT1+1,I2
        RATIO=1.0E0-((FLOAT(I-CENT1))/(FLOAT(WID1)))
        COLOUR(1,I)=COLOUR(1,I)+NINT(FLOAT(COLR1)*RATIO)
        COLOUR(2,I)=COLOUR(2,I)+NINT(FLOAT(COLG1)*RATIO)
        COLOUR(3,I)=COLOUR(3,I)+NINT(FLOAT(COLB1)*RATIO)
      END DO
C
C      Check for supersaturated guns.
C
      DO I=1,256
        DO J=1,3
          IF (COLOUR(J,I).GT.255) COLOUR(J,I)=255
        END DO
      END DO
      END
