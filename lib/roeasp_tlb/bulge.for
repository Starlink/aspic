      FUNCTION BULGE (XPT,A,NT)
C+
C     Function:- BULGE.
C
C     Function to return the Log I value for a particular
C     radial distance along a profile following a 
C     de Vaucouleurs r**1/4 law.
C
C  Given;
C   XPT  (R)  Radial distance at which the Log I is to be evaluated.
C   A    (RA) Parameter array for the r**1/4 law;
C              A(1) = Log Io
C              A(2) = Ro
C   NT   (I)  Size of array A, at least 2.
C
C  Returned;
C   BULGE (R) Log I evaluated at radius XPT.
C
C  A C Davenhall./ROE/                                  5/8/82.
C  A C Davenhall./ROE/ {Modified}                      25/7/83.
C-
      IMPLICIT NONE
C
      INTEGER NT
      REAL A(NT)
      REAL XPT,BULGE
C
      REAL QUART,CONST
      PARAMETER (QUART=2.5E-1)
      PARAMETER (CONST=-3.3307E0)
C
      REAL WORK
C
C
      WORK=XPT/A(2)
      IF (WORK.LT.1.0E-6) WORK=1.0E-6
      BULGE=A(1)+(CONST*((WORK**QUART)-1.0E0))
C
      END
