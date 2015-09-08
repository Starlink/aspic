      SUBROUTINE ELLPLR (A,B,SINTHA,COSTHA,RADIUS)
C+
C     ELLPLR.
C
C     Subroutine to compute the radial polar coord. from the
C     (sine and cosine of the) angular polar coord. of a
C     point lying along an ellipse of specified major and
C     minor axes, the major axis being horizontal and the
C     minor axis vertical and the ellipse being centred
C     on the origin.
C
C  Given;
C   A      (R) Semi major axis of the ellipse.
C   B      (R)  "   minor  "   "   "     "   .
C   SINTHA (R) Sine of the angular coord.
C   COSTHA (R) Cosine of the angular coord.
C
C  Returned;
C   RADIUS (R) Radius of point in polar coords.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                      17/9/82.
C  A C Davenhall./ROE/   {Modified}                         21/9/82.
C-
      REAL A,B,SINTHA,COSTHA,RADIUS
C
      REAL WORK,DENOM
C
C
      DENOM=((A*SINTHA)**2)+((B*COSTHA)**2)
      IF (DENOM.GT.1.0E-4) THEN
        WORK=((A*B)**2)/DENOM
        RADIUS=SQRT(WORK)
      ELSE
        RADIUS=0.0E0
      END IF
      END
