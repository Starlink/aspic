      FUNCTION FUNCT (I,XX,YY)
C+
C      Terms of fitting function for polynomial least
C      squares fit to background surrounding a star image.
C
C     Given;
C     I  (I)  Term to be evaluated.
C     XX (R)  Value of the X coord.
C     YY (R)    "   "   "  Y   "  .
C
C     Returned;
C     FUNCT (R) Value of the required term.
C
C     A C Davenhall./ROE/                                20/1/82.
C-
      INTEGER I
      REAL XX,YY,FUNCT
      FUNCT=0.0E0
      IF (I.EQ.1) FUNCT=1.0E0
      IF (I.EQ.2) FUNCT=XX
      IF (I.EQ.3) FUNCT=YY
      IF (I.EQ.4) FUNCT=XX*YY
      IF (I.EQ.5) FUNCT=XX**2
      IF (I.EQ.6) FUNCT=YY**2
      IF (I.EQ.7) FUNCT=(XX**2)*(YY**2)
      IF (I.EQ.8) FUNCT=XX**3
      IF (I.EQ.9) FUNCT=YY**3
      END
