      FUNCTION FUNC (XX,YY,A,MAXORD)
C+
C      Evaluates the function f(x,y) for polynomial fitting
C      to the background region around a star.
C
C   Given;
C   XX   (R)  X coord. at which the function is to be evaluated.
C   YY   (R)  Y   "  . "    "    "     "     "  "  "     "     .
C   A    (R)  Array of coefficients.
C   MAXORD (I) Size of array A.
C
C   Returned;
C   FUNC  (R)  Value of the function at the point XX, YY evaluated for
C              the coefficicents A.
C
C   Function called;  FUNCT.
C
C   A C Davenhall./ROE/                                    21/1/82.
C-
      REAL A(MAXORD)
      REAL XX,YY,SUM
      INTEGER MAXORD
      SUM=0.0E0
      DO I=1,MAXORD
        SUM=SUM+(A(I)*FUNCT(I,XX,YY))
      END DO
      FUNC=SUM
      END
