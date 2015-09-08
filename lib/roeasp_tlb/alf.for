      SUBROUTINE ALF
C+
C     ALF.
C
C     Subroutine to convert a CIFER 2634 terminal from
C     graphics to alpha mode.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutine called;
C   Graphics:  WAIT.
C
C  Original;  C. D. Pike./RGO/                May 1982.
C  Modified;  A C Davenhall./ROE/             29/6/82.
C-
      BYTE ESC,T1,T2
      INTEGER STAT
      ESC = '1B'X
      T1 = '51'X
      CLOSE(6)
      WRITE(6) ESC,T1
      CALL WAIT (5.0E-2,STAT)
      T1 = '5E'X
      T2 = '54'X
      WRITE(6) ESC,T1,T2
      CALL WAIT (5.0E-2,STAT)
      T1 = '5A'X
      WRITE(6) ESC,T1
      CLOSE(6)
      END
