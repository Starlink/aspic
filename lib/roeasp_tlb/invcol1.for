      SUBROUTINE INVCOL1 (NCOL,NLEVEL,INCOL,OUTCOL)
C+
C     INVCOL1.
C
C     Subroutine to invert a colour table.
C
C  Given;
C   NCOL   (I)  No. of colour guns.
C   NLEVEL (I)  No. of intensity levels for each gun.
C   INCOL  (I)  Input colour table to be inverted.
C
C  Returned;
C   OUTCOL (IA) Output, inverted colour table.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                 26/10/82.
C-
      INTEGER NCOL,NLEVEL
      INTEGER INCOL(NCOL,NLEVEL),OUTCOL(NCOL,NLEVEL)
C
C
      DO J=1,NLEVEL
        DO I=1,NCOL
          OUTCOL(I,J)=INCOL(I,NLEVEL+1-J)
        END DO
      END DO
      END
