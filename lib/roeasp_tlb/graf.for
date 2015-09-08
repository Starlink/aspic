      SUBROUTINE GRAF
C     GRAF.
C
C     Subroutine to convert a CIFER 2634 terminal from
C     alpha to graphics mode.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   None.
C
C  Original;  C. D. Pike./RGO/                  May 1982.
C  Modified;  A C Davenhall./ROE/               29/6/82.
C-
      BYTE ESC,T1,T2
      ESC = '1B'X
      CLOSE(6)
      T1 = '51'X
      WRITE(6) ESC,T1
      T1 = '5E'X
      T2 = '12'X
      WRITE(6) ESC,T1,T2
      T1 = '5A'X
      WRITE(6) ESC,T1
      CLOSE(6)
      END
