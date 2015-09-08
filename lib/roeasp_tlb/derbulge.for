      FUNCTION DERBULGE (XPT,A,NT,J)
C+
C     Function:- DERBULGE.
C
C     Function to evaluate the derivative of an r**1/4 law
C     with respect to any of its parameters at a specified
C     radius.
c
C
C  Given;
C   XPT   (R)  Radius at which the derivative is to be evaluated.
C   A     (RA) Array to hold parameters for r**1/4 law.
C               A(1) = Log Io
C               A(2) = Ro
C   NT    (I)  Size of array A, at least 2.
C   J     (I)  Parameter derivative to evaluated with respect to.
C
C  Returned;
C   DERBULGE (R) Derivative evaluated at point XPT.
C
C  A C Davenhall./St Andrews/                              Spring 81.
C  A C Davenhall./ROE/  {Modified}                         5/8/82.
C  A C Davenhall./ROE/  {Modified}                        25/7/83.
C-
      IMPLICIT NONE
C
      INTEGER NT,J
      REAL A(NT)
      REAL XPT,DERBULGE
C
      REAL CONST,QUART,R5Y4
      PARAMETER (CONST=8.32675E-1)
      PARAMETER (QUART=2.5E-1)
      PARAMETER (R5Y4=-1.25E0)
C
      REAL WORK,WORK1
C
C
      IF (J.EQ.1)    DERBULGE=1.0E0
C
      IF (J.EQ.2) THEN
        IF (XPT.GT.1.0E-6) THEN
          WORK=XPT
        ELSE
          WORK=1.0E-6
        END IF
        IF (A(2).GT.1.0E-6) THEN
          WORK1=A(2)
        ELSE
          WORK1=1.0E-6
        END IF
        DERBULGE=CONST*(WORK**QUART)*(WORK1**R5Y4)
      END IF
C
      END
