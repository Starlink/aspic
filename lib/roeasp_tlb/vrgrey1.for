      SUBROUTINE VRGREY1 (BLACK,WHITE,NCOL,NLEVEL,IDCOL)
C+
C     VRGREY1.
C
C     Creates a grey colour table with variable stretch.
C     The black and white levels defining the end points of the
C     stretch are input as arguments.
C
C  Given;
C   BLACK  (I)  Intensity level corresponding to the black end point
C   WHITE  (I)  Intensity level corresponding to the white end point.
C   NCOL   (I)  No. of colour guns.
C   NLEVEL (I)  No. of intensity levels in each gun.
C
C  Returned;
C   IDCOL  (IA) Array holding the intensities comprising the 
C               generated colour table.
C
C  Subroutines called;
C   None.
C
C   H.Reitsema/9.4.1981
C   A C Davenhall./ROE/                                    25/10/82.
C-
      INTEGER BLACK,WHITE,NCOL,NLEVEL
      INTEGER IDCOL(NCOL,NLEVEL)
C
C
C   Set colour table.
C   Values outside grey range are set to black or white.
C
      DO J=1,256
         DO I=1,3
            IDCOL(I,J)=NINT(REAL(J-BLACK)*255.0/REAL(WHITE-BLACK))
            IDCOL(I,J)=MAX(0,IDCOL(I,J))
            IDCOL(I,J)=MIN(255,IDCOL(I,J))
         END DO
      END DO
      END
