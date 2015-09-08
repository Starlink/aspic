      SUBROUTINE WITHIN (ANGLE,LIM1,LIM2,INCL)
C+
C     WITHIN.
C
C     Subroutine to determine whether an input angle in radians
C     falls between 2 other specified (integer) angles in
C     degrees.
C
C  Given;
C   ANGLE  (R)  Angle which may or may not fall within the
C               specified bounds.
C   LIM1   (I)  Lower bound (degrees).
C   LIM2   (I)  Upper   "   (   "   ).
C   
C  Returned;
C   INCL   (L)  .TRUE. if the angle falls between LIM1 and LIM2,
C               otherwise .FALSE.
C
C  A C Davenhall./ROE/                                    5/7/82.
C
C  Plagairized from a logical function in ASPIC
C  (Which would have upset the E2D conventions about not
C  using functions).
C
C-
      REAL ANGLE
      INTEGER LIM1,LIM2
      LOGICAL INCL
C
      REAL FLIM1,FLIM2,REDDEG,ANGLED
C
C    The number of degrees in a radian.
C
      PARAMETER (RADDEG=5.729578E1)
C
C
      FLIM1=FLOAT(LIM1)
      FLIM2=FLOAT(LIM2)
      ANGLED=ANGLE*RADDEG
C
      INCL=.FALSE.
      IF (ANGLED.GE.FLIM1.AND.ANGLED.LE.FLIM2) INCL=.TRUE.
      END
