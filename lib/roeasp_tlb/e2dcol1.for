      SUBROUTINE E2DCOL1 (NCOL,NLEVEL,IDCOL)
C+
C     E2DCOL1.
C
C     Generate an array holding the E2D default colour table.
C
C  Given;
C   NCOL    (I)  No. of colour guns.
C   NLEVEL  (I)  No. intensity levels in each gun.
C
C  Returned;
C   IDCOL   (IA) Array of intensity levels for each gun.
C
C  Subroutines called;
C   ...
C
C  B.D.Kelly./ROE/                                        1981.
C  A C Davenhall./ROE/                                    25/10/82.
C-
 
      INTEGER NCOL,NLEVEL
      INTEGER IDCOL(NCOL,NLEVEL)
      INTEGER INTVAL(3,216),KEY(12)
      DATA KEY/200,6,18,36,31,181,147,114,10,186,211,216/
C
C
C     SET UP MASTER COLOUR TABLE
C
      DO J=1,216
        INTVAL(1,J)=MOD(J-1,6)*51
        INTVAL(2,J)=MOD((J-1)/6,6)*51
        INTVAL(3,J)=MOD((J-1)/36,6)*51
      END DO
C
C    A reasonable set of colours are defined by array KEY.
C
      DO JCOL=1,12
        INT1=(JCOL-1)*21+1
        INT2=JCOL*21+1
        DO J=INT1,INT2
          DO IC=1,3
            IDCOL(IC,J)=INTVAL(IC,KEY(JCOL))
          END DO
        END DO
      END DO
C
C    Set top level white (for line graphics).
C
      DO J=254,256
        DO I=1,3
          IDCOL(I,J)=255
        END DO
      END DO
      END
