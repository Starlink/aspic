      FUNCTION DERBD (XPT,A,NT,J)
C+
C     Function:- DERBD.
C
C     Function to evaluate the derivative of a composite
C     (r**1/4 law + exponential disk) profile at a given
C     radius with respect to any of the parameters.
C
C  Given;
C   XPT  (R)   Radius at which the derivative is to be evaluated.
C   A    (RA)  Array of parameters for luminosity law;
C               A(1) = Io (Bulge)      A(2) = Ro (Bulge)
C               A(3) = Io (Disk)       A(4) = Alpha (Disk)
C   NT   (I)   Size of array A, at least 4.
C   J    (I)   Parameter with respect to which the derivative
C              is to be evaluated.
c
C
C  Returned;
C   DERBD (R) Derivative evaluated at point XPT.
C
C  Function used;
C   BULDISK.
C
C  A C Davenhall./St Andrews/                             Spring 1981.
C  A C Davenhall./ROE/  {Modified}                            5/8/82.
C  A C Davenhall./ROE/  {   "    }                           25/7/83.
C-
      IMPLICIT NONE
C
      INTEGER NT,J
      REAL A(NT)
      REAL XPT,DERBD
C
C    Declare the type of the function used.
C
      REAL BULDISK
C
      REAL WORK,DUMMY,DUMMY1,DUMMY2
C
      REAL CONST,QUART,Q54
      PARAMETER (CONST=-3.3307E0)
      PARAMETER (QUART=2.5E-1)
      PARAMETER (Q54=-1.25E0)
C
C    CONST1 = (loge10)*A/4
C
      REAL CONST1
      PARAMETER (CONST1=1.917305E0)
C
C    CONST2 = Log10(e)
C
      REAL CONST2
      PARAMETER (CONST2=4.342945E-1)
C
C
C
      IF (J.EQ.1) THEN
        DUMMY=XPT/A(2)
        IF (DUMMY.LT.1.0E-6) DUMMY=1.0E-6
        WORK = 1.0E1**(((DUMMY**QUART)-1.0E0)*CONST)
      END IF
C
      IF (J.EQ.2) THEN
        DUMMY=XPT/A(2)
        IF (DUMMY.LT.1.0E-6) DUMMY=1.0E-6
        DUMMY1=A(2)
        IF (DUMMY1.LT.1.0E-6) DUMMY1=1.0E-6
        DUMMY2=XPT
        IF (DUMMY2.LT.1.0E-6) DUMMY2=1.0E-6
        WORK = A(1)*(1.0E1**(CONST*((DUMMY**QUART)
     :         -1.0E0)))*CONST1*(DUMMY1**QUART)*(DUMMY2**Q54)
      END IF
C
      IF (J.EQ.3)    WORK = EXP(-A(4)*XPT)
C
      IF (J.EQ.4)    WORK = -A(3)*XPT*EXP(-A(4)*XPT)
C
      DERBD=CONST2*WORK/BULDISK(XPT,A,NT)
C
      END
