      SUBROUTINE COLSLIC1 (RED,GREEN,BLUE,COLOUR)
C+
C     COLSLIC1.
C
C     Subroutine to generate a linear ramp, spanning the entire
C     colour table containing a single colour of varying
C     intensity.
C
C  Given;
C   RED    (I)  Max. intensity in the Red gun.
C   GREEN  (I)   " .     "     "   "  Green" .
C   BLUE   (I)   " .     "     "   "  Blue " .
C
C  Returned;
C   COLOUR (IA) Colour table containing linear mono-chrome
C               ramp.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                           25/2/83.
C-
      INTEGER RED,GREEN,BLUE
      INTEGER COLOUR(3,256)
C
      REAL REDINC,GREINC,BLUINC
C
C
C    Set up the increment for each colour.
C
      REDINC=FLOAT(RED)/2.56E2
      GREINC=FLOAT(GREEN)/2.56E2
      BLUINC=FLOAT(BLUE)/2.56E2
C
C    Generate a linear ramp of the chosen colour.
C
      DO I=1,256
        COLOUR(1,I)=NINT(REDINC*FLOAT(I-1))
        COLOUR(2,I)=NINT(GREINC*FLOAT(I-1))
        COLOUR(3,I)=NINT(BLUINC*FLOAT(I-1))
      END DO
C
      END
