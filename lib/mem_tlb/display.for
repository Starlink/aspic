C
      SUBROUTINE UMAP(IFILE,IPOS,F)
C Plots file IFILE at position IPOS on the ARGS
C F is scale factor. Factors > 1.0 cause the high regions of the image
C to be burned out, so that fainter structure may be seen.
C Version for packed transposed Fourier
      BYTE BUFF(4096)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      COMMON /MECOMF/ IFT,NDIM,N(3),NC(5)
      COMMON /OPCOMP/ ISW
C INVERT controls which way up the image is displayed:
C  FALSE causes the image to be drawn from the top downwards
C  TRUE causes the image to be drawn from the bottom upwards
      LOGICAL INVERT
      DATA INVERT/.FALSE./
C
      IF(MJ.NE.MK) GOTO 998
      LR=N(1)
      NJK=NJ
      MJK=MJ
      IF(IPOS.NE.1 .OR. IFILE.LE.20) GOTO 1
      LR=N(2)
      IF(ISW.EQ.0 .AND. N(2).LE.256) LR=2*N(2)
      NJK=NK
      MJK=MK
    1 CONTINUE
      NNR=MJK/LR
      NR=NNR*NJK
C Can't display images > 512 in either dimension
      IF(LR.GT.512 .OR. NR.GT.512) RETURN
C Work out where to display image
      NXMAX=N(1)
      J1=512/NXMAX
      IF(J1.LE.0) J1=1
      NYMAX=N(2)
      J2=512/NYMAX
      IF(J2.LE.0) J2=1
      J=IPOS-1
      IF(J.LT.0) J=0
      IX=MOD(J,J1)
      IX=IX*NXMAX
      J2=J1*J2
      IY=MOD(J,J2)/J1
      IF(INVERT) THEN
      IY=512 - NR*(IY+1)
      ELSE
      IY=512 - NR*IY -NNR
      ENDIF
C Find maximum of image
      RMAX=-1.0E30
      RMIN=+1.0E30
      DO 5 I1=1,NJK
      CALL UREAD(IFILE)
      CALL UMAP1(ST(KC(IFILE)),RMAX,RMIN,MJK)
    5 CONTINUE
      CALL UINIT
      WRITE(IOUT,100) RMAX,RMIN
      SCALE=0.0
      IF(RMAX.GT.0.0) SCALE=255.*F/RMAX
C and plot it
      DO 10 I=1,NJK
      CALL UREAD(IFILE)
      CALL UMAP2(ST(KC(IFILE)),BUFF,SCALE,LR,NNR,INVERT)
      CALL SRPXP(BUFF,LR,NNR,IX,IY,8,MJK)
      IF(INVERT) THEN
      IY=IY+NNR
      ELSE
      IY=IY-NNR
      ENDIF
   10 CONTINUE
      CALL UINIT
      RETURN
  998 CONTINUE
      WRITE(IOUT,800)
      RETURN
  100 FORMAT(' Maximum and minimum values are ',1PE10.3,E10.3)
  800 FORMAT('0Error calling UMAP',/' Only packed Fourier format ',
     *        'supported')
      END
C
      SUBROUTINE UMAP1(X,RMAX,RMIN,M)
      DIMENSION  X(1)
      DO 1 I=1,M
      IF(X(I).GT.RMAX) RMAX=X(I)
      IF(X(I).LT.RMIN) RMIN=X(I)
    1 CONTINUE
      RETURN
      END
C
      SUBROUTINE UMAP2(X,BUFF,SCALE,LR,NR,INVERT)
      DIMENSION  X(LR,1)
      BYTE BUFF(LR,1)
      LOGICAL INVERT
      K=NR
      INC=-1
      IF(INVERT) K=1
      IF(INVERT) INC=1
      DO 2 I=1,NR
      DO 1 J=1,LR
      T=SCALE*X(J,I)
      IF(T.LT.0.0) T=0.0
      IF(T.GT.255.0) T=255.0
      IF(T.GE.128.0) T=T-256.0
      BUFF(J,K)=T
    1 CONTINUE
    2 K=K+INC
      RETURN
      END
C
      SUBROUTINE DIAG
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
    1 WRITE(IOUT,100)
  100 FORMAT('$Area to display? ')
      IFILE=0
      READ(5,*,ERR=1,END=999) IFILE
      IF(IFILE.LE.0) RETURN
      IF(IFILE.GT.40) GOTO 1
      IF(KB(IFILE).LT.1) GOTO 1
    2 CONTINUE
      WRITE(IOUT,200)
  200 FORMAT('$Scale factor? ')
      READ(5,*,ERR=2,END=999) SCALE
      CALL UMAP(IFILE,1,SCALE)
      GOTO 1
  999 RETURN
      END
