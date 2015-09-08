      FUNCTION DISK (XPT,A,NT)
C+
C     Function:- DISK.
C
C     Function to evaluate a disk exponential law at a 
C     specified radius.
C
C  Given;
C   XPT    (R)  Radius at which the profile is to be evaluated.
C   A      (RA) Array of parameters for the law;
C                A(1) = Io
C                A(2) = Alpha
C   NT     (I)  Size of array A, at least 2.
C
C  Returned;
C   DISK   (R)  Exponential law evaluated at radius XPT.
C
C  A C Davenhall./St Andrews/                                  Spring 81 
C  A C Davenhall./ROE/      {Modified}                         5/8/82.
C-
      IMPLICIT NONE
C
      INTEGER NT
      REAL A(NT)
      REAL XPT,DISK
C
C
      DISK=A(1)*EXP(-A(2)*XPT)
      END
