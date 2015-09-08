      SUBROUTINE ELPLR1 (A,B,RADIUS,SINTHA,COSTHA)
C+
C     ELPLR1.
C
C     Subroutine to compute the sine and cosine of the polar
C     angular coord of a specified ellipse at a given radius.
C
C  Given;
C   A      (R) Semi-major axis of the ellipse.
C   B      (R)  "  -minor  "   "   "     "   .
C   RADIUS (R) Radius at which the angular coord. os to be computed.
C
C  Returned;
C   SINTHA (R) Computed sine of angular coord.
C   COSTHA (R)    "   cosine "     "      "  .
C
C  A C Davenhall./ROE/                                      25/9/82.
C-
      REAL A,B,RADIUS,COSTHA,SINTHA
C
      REAL WORK,WORK1,WORK2
C
C 
      WORK=(A**2)+(B**2)
      WORK1=1.0E0+((A/RADIUS)**2)
      WORK2=ABS(1.0E0-((B/RADIUS)**2))
C
      SINTHA=SQRT((B**2)*WORK1/WORK)
      COSTHA=SQRT((B**2)*WORK2/WORK)
      END
