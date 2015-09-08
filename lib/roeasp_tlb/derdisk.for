      FUNCTION DERDISK (XPT,A,NT,J)
C+
C     Function:- DERDISK.
C
C     Function to evaluate the derivative of an expontial disk
C     law with respect to either of its derivatives at a 
C     specified radius.
C
C  Given;
C   XPT   (R)  Radius at which the derivative is to be evaluated.
c   A     (RA) Array holding values for the exponential law
C              parameters.
C                A(1) = Io
C                A(2) = Alpha
C   NT    (I)  Size of array A, at least 2.
C   J     (I)  Parameter that the derivative is to be evaluated
C              with respect to.
C
C  Returned;
C   DERDISK (R) Derivative evaluated at the specified radius.
C
C  A C Davenhall./St Andrews/                               Spring 81.
C  A C Davenhall./ROE/                                      5/8/82.
C-
      IMPLICIT NONE
C
      INTEGER NT,J
      REAL A(NT)
      REAL XPT,DERDISK
C
C
      IF (J.EQ.1)    DERDISK=EXP(-A(2)*XPT)
      IF (J.EQ.2)    DERDISK=-A(1)*XPT*EXP(-A(2)*XPT)
      END
