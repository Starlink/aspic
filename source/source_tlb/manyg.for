C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   MANYG *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               MANYG
C
C
C          FUNCTION:-
C               It  generates  a  2-D  Starlink  image  which  contains   a
C               background  value, as many Gaussian "stars" as required and
C               some rather artificial noise.
C
C
C          USE:-
C               It may be used to produce a test image for any of the ASPIC
C               programs.
C
C
C
C         USER PARAMETERS:-
C
C         XYDIMES         128,128             The  two  dimensions  of  the
C                                             Starlink image being created.
C
C         OUTPUT                              The  name  of   the   created
C                                             image.
C
C         BKGND           10.0                The    constant    background
C                                             value.
C
C         XPOS                                The  x   co-ordinate   of   a
C                                             Gaussian.   A  null  response
C                                             causes exit  from  the  loop,
C                                             otherwise   it   returns   to
C                                             asking   for   more    "star"
C                                             positions.
C
C         YPOS                                The  y  co-ordinate  of   the
C                                             Gaussian to be added.
C
C         PEAKVAL         100 or previous     The central intensity of  the
C                                             Gaussian.
C
C         WIDTH           5.0 or previous     The  sigma  of  the  Gaussian
C                                             being added.
C
C
C         K F Hartley              RGO                             7-JAN-82
C
C
C
C
C--------------------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER IX(2),IPOUT,ISTAT
      REAL PARMS(4)
      IX(1)=128
      IX(2)=128
      CALL RDKEYI('XYDIMES',.TRUE.,2,IX,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      CALL WRIMAG('OUTPUT',FMT_R,IX,2,IPOUT,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      BACK=10.0
      CALL RDKEYR('BKGND',.TRUE.,1,BACK,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL) THEN
        CALL WRERR('HELL')
         GO TO 800
      END IF
      CALL FLAT(%VAL(IPOUT),IX(1),IX(2),BACK)
      WI=5.0
      PV=100.0
  100 CALL RDKEYR('XPOS',.FALSE.,1,XP,I,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         GO TO 700
      END IF
      PARMS(1)=XP
  110 CALL RDKEYR('YPOS',.FALSE.,1,YP,I,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         GO TO 700
      END IF
      PARMS(2)=YP
  120 CALL RDKEYR('PEAKVAL',.TRUE.,1,PV,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL) THEN
         CALL WRERR('HELL')
         GO TO 700
      END IF
      PARMS(3)=PV
  130 CALL RDKEYR('WIDTH',.TRUE.,1,WI,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL) THEN
         CALL WRERR('HELL')
         GO TO 700
      END IF
      PARMS(4)=WI
      CALL FILL(%VAL(IPOUT),IX(1),IX(2),PARMS)
      CALL CNPAR('XPOS',ISTAT)
      CALL CNPAR('YPOS',ISTAT)
      CALL CNPAR('PEAKVAL',ISTAT)
      CALL CNPAR('WIDTH',ISTAT)
      GO TO 100
  700 CONTINUE
      CALL ADDNOISE(%VAL(IPOUT),IX(1),IX(2))
  800 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
      SUBROUTINE FLAT(DATA,N,M,VALUE)
      REAL DATA(N,M)
      DO 200 J=1,M
         DO 100 I=1,N
           DATA(I,J)=VALUE
  100    CONTINUE
  200 CONTINUE
      END
      SUBROUTINE FILL(DATA,N,M,PARMS)
      REAL DATA(N,M),PARMS(4)
      XC=PARMS(1)
      YC=PARMS(2)
      H=PARMS(3)
      W=PARMS(4)
      I1=XC-4*W
      I2=XC+4*W
      J1=YC-4*W
      J2=YC+4*W
      DO 200 J=J1,J2
         Y=REAL(J)-YC
         IF (J.LT.1.OR.J.GT.M) GO TO 200
         DO 100 I=I1,I2
            X=REAL(I)-XC
            IF (I.LE.0.OR.I.GT.N) GO TO 100
            VAL=H*EXP(-(X*X+Y*Y)/(2.0*W*W))
            DATA(I,J)=DATA(I,J)+VAL
  100    CONTINUE
  200 CONTINUE
      END
      SUBROUTINE ADDNOISE(DATA,N,M)
      REAL DATA(N,M)
      ISEED=12345
      R=RAN(ISEED)
      DO 200 J=1,M
         DO 100 I=1,N
            V=DATA(I,J)
            V=V+SQRT(V)*(RAN(ISEED)-0.5)
            DATA(I,J)=V
  100    CONTINUE
  200 CONTINUE
      END
