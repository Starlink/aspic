      SUBROUTINE SPECTRUM1 (NCOL,NLEVEL,IDCOL)
C+
C     SPECTRUM1.
C
C     Creates a colour table which shades from blue,
C     through green, to red.
C
C  Given;
C   NCOL   (I)  No. of colour guns.
C   NLEVEL (I)  No. of intensity levels in each gun.
C
C  Returned;
C   IDCOL  (IA) Colour table evaluated for each level
C               for each gun.
C
C  Subroutines called;
C   None.
C  B.D.Kelly/ROE/                         1981.
C  A C Davenhall./ROE/                    25/10/82.
C-

      INTEGER NCOL,NLEVEL
      INTEGER IDCOL(NCOL,NLEVEL)
C
      INTEGER N1,N2,N3,N4
C
      N1=NLEVEL/4
      N2=NLEVEL/2
      N3=N1+N2
      N4=NLEVEL
C
      DO J=1,N1
         IDCOL(1,J)=0
         IDCOL(2,J)=4*(J-1)
         IDCOL(3,J)=255
      ENDDO
C
      DO J=N1+1,N2
         IDCOL(1,J)=0
         IDCOL(2,J)=255
         IDCOL(3,J)=4*(128-J)
      ENDDO
C
      DO J=N2+1,N3
         IDCOL(1,J)=4*(J-129)
         IDCOL(2,J)=255
         IDCOL(3,J)=0
      ENDDO
C
      DO J=N3+1,N4
         IDCOL(1,J)=255
         IDCOL(2,J)=4*(256-J)
         IDCOL(3,J)=0
      ENDDO
C
      END
