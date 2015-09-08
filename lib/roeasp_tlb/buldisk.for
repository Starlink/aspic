      FUNCTION BULDISK (XPT,A,NT)
C+
C     Function:- BULDISK.
C
C     Function to return the Log I value calculated for a 
C     galaxy profile following a r**1/4 law plus
C     an exponential law.
C
C  Given;
C   XPT  (R)  Radius of point at which Log I is to be calulated.
C   A    (RA) Array holding parameters for luminosity law.
C              A(1) = Io (bulge)
C              A(2) = Ro (  "  )
C              A(3) = Io (disk)
C              A(4) = Alpha (disk)
C   NT   (I)  Size of array A (at least 4).
C
C  Returned;
C   BULDISK (R) Log I evaluated at radius XPT.
C
C  Subroutines called;
C   None.
C
C  A C Davenahll./St Andrews/                        Spring 81.
C  A C Davenhall./ROE/  {Modified}                   2/8/82.
C  A C Davenhall./ROE/  {   "    }                  25/7/83.
C-
      IMPLICIT NONE
C
      INTEGER NT
      REAL A(NT)
      REAL XPT,BULDISK
C
      REAL BULG,DISK,WORK
C
      REAL CONST,QUART
      PARAMETER (CONST=-3.3307E0)
      PARAMETER (QUART=2.5E-1)
C
C
      WORK=XPT/A(2)
      IF (WORK.LT.1.0E-6) WORK=1.0E-6
      BULG=A(1)*1.0E1**(CONST*((WORK**QUART)-1.0E0))
C
      WORK=-A(4)*XPT
      IF (WORK.GT.7.5E1) WORK=7.5E1
      DISK=A(3)*EXP(WORK)
C
      WORK=DISK+BULG
      IF (WORK.GT.1.0E-6) THEN
        BULDISK=ALOG10(WORK)
      ELSE
        BULDISK=-6.0E0
      END IF
C
      END
