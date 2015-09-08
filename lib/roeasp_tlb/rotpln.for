      SUBROUTINE ROTPLN (MINVAL,MAXVAL,INCR,PLANE)
C+
C     ROTPLN.
C
C     Subroutine to change the overlay plane selected for
C     graphics on the Args. The plane will be "rotated"
C     ie. when the limits of the permitted planes are
C     exceeded the opposite permitted extrema will be 
C     adopted.
C
C  Given;
C   MINVAL  (I)  Min. permitted plane (normally 8).
C   MAXVAL  (I)  Max.     "       "   (   "    15).
C   INCR    (I)  Increment by which the plane is to be changed
C                (normally +1).
C   PLANE   (I)  Current overlay plane.
C
C  Returned;
C   PLANE   (I)  New overlay plane.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                     28/7/82.
C-
      INTEGER MINVAL,MAXVAL,INCR,PLANE
C
C    First check that the Max. permitted plane is greater
C    than the Min. permitted plane.
C
      IF (MAXVAL.GT.MINVAL) THEN
C
C    Increment the current plane.
C
        PLANE=PLANE+INCR
C
C    Check if either extrema has been passed and if so "rotate"
C    to the other extrema.
C       
        IF (PLANE.GT.MAXVAL) PLANE=MINVAL
        IF (PLANE.LT.MINVAL) PLANE=MAXVAL
      END IF
      END
