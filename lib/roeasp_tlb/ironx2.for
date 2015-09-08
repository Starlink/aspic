      SUBROUTINE IRONX2 (PLANE,COLOUR,MIN,MAX,INCR,VAL,ACCEPT)
C+
C     IRONX2.
C
C     Subroutine to increment, decrement or accept
C     one of the parameters defining a pseudo-cursor
C     for the iron cross routine.
C
C  Given;
C   PLANE (I) Args overlay plane in which graphics appear.
C   COLOUR (C) Colour for graphics.
C   MIN  (R) Minimum value for parameter.
C   MAX  (R) Maximum value for parameter.
C   INCR (R) Increment by which parameter is to be 
C            changed.
C   VAL  (R) Initial value for paramater.
C
C  Returned;
C   VAL  (R) Changed value for parameter.
C   ACCEPT (L) Acceptance flag.
C            = .TRUE. Value Ok.
C            = .FALSE. Value not Ok.
C
C  Subroutines called;
C   Args:- ARGS_USRCUR.
C
C  A C Davenhall./ROE/                           1/7/82.
C-
      REAL MIN,MAX,INCR,VAL
      LOGICAL ACCEPT
      INTEGER PLANE
      CHARACTER COLOUR*1
C
      INTEGER BUTT,XIN,YIN,XOUT,YOUT
      INTEGER*2 CURSOR(256)
C
C
C    Set up a totally transparent dummy array.
C
      DO I=1,256
        CURSOR(I)=0
      END DO
C
C    Read the buttons.
C
      XIN=0
      YIN=0
      CALL ARGS_USRCUR (PLANE,COLOUR,CURSOR,XIN,YIN,XOUT,YOUT,BUTT)
C
C    Check whether Ok and if not increment or decrement.
C
      ACCEPT=.FALSE.
      IF (BUTT.EQ.1) THEN
        ACCEPT=.TRUE.
      ELSE
        IF (BUTT.EQ.2) THEN
          IF (VAL.LT.MAX) THEN
            VAL=VAL+INCR
          ELSE
            VAL=MAX
          END IF
        ELSE IF (BUTT.EQ.3) THEN
          IF (VAL.GT.MIN) THEN
            VAL=VAL-INCR
          ELSE
            VAL=MIN
          END IF
        END IF
      END IF
      END
