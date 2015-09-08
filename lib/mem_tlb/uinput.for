C
      SUBROUTINE UINPUT(DISP,NITER,FLAT,ERR,VARERR,DEF,SCALE,NUM)
C Get options from user
      LOGICAL DISP,FLAT,VARERR
      COMMON /MECOMS/ ST(20480)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,NN(3),NC(5)
      COMMON /UCOMP/ IFRAME
      LOGICAL ANS
      CHARACTER*70 LINE
      CHARACTER*64 FILNAM
      INCLUDE 'STARDIR:ERRPAR.FOR/NOLIST'
      INCLUDE 'STARDIR:FMTPAR.FOR/NOLIST'
C
      SCALE=1.0
C ARGS display?
      DISP=.FALSE.
      CALL RDKEYL('ARGS',.TRUE.,1,DISP,I,ISTAT)
      IF(DISP) THEN
        CALL SRINIT(0,.TRUE.,ISTAT)
        IF(ISTAT.NE.0) THEN
          DISP=.FALSE.
          CALL WRUSER('Error allocating ARGS',ISTAT)
        ENDIF
      ENDIF
C Number of iterations
    1 NITER=1
      CALL RDKEYI('NITER',.TRUE.,1,NITER,I,ISTAT)
      IF(NITER.GE.0 .AND. NITER.LT.100) GOTO 2
        CALL CNPAR('NITER',ISTAT)
        GOTO 1
    2 CONTINUE
C Get name of work space file
    3 CONTINUE
      CALL RDKEYC('WKFILE',.FALSE.,1,FILNAM,I,ISTAT)
      IF(ISTAT.EQ.ERR_PARNUL.OR.FILNAM.EQ.' ') STOP
C Get dimensions of data
    4 CONTINUE
      CALL UIN('DATA',1,ISTAT)
      IF(ISTAT.EQ.ERR_FRMNUL) THEN
        CALL CNPAR('DATA',ISTAT)
        GOTO 4
      ENDIF
      WRITE(LINE,1000) NN(1),NN(2)
 1000 FORMAT(' Image dimensions are ',I6,' x ',I6)
      CALL WRUSER(LINE,ISTAT)
C Create work file
      ISIZE=(NN(1)*NN(2)/128)*10
      I=LIB$LOCC(' ',FILNAM)
      IF(I.GT.1) I=I-1
      IF(I.EQ.0) I=LEN(FILNAM)
      CALL UOPEN(FILNAM(:I),ISIZE,ISTAT)
      IF(ISTAT.NE.0) THEN
        I=0
        CALL SYS$GETMSG(%val(ISTAT),I,LINE,%val(15),)
        CALL WRUSER(LINE(:I),IISTAT)
        CALL CNPAR('WKFILE',IISTAT)
        GOTO 3
      ENDIF
C Set up addressing
      NTOT=NN(1)*NN(2)
      IF(NTOT.LT.4096) THEN
        CALL WRERR('TOOSMALL')
        STOP
      ENDIF
      NJ=NTOT/MJ
      IF(NN(1).GT.4096 .OR. NN(2).GT.2048) THEN
        CALL WRERR('TOOBIG')
        STOP
      ENDIF
      NK=NJ
      MK=MJ
      DO 9 I=1,40
    9 KB(I)=MK*(KB(I)-1)+1
      CALL MEINIT
C Set up NC
      NC(1)=1
      NC(2)=NN(2)/2+1
C Work out how many images fit on the screen
      JX=NN(1)
      JX=512/JX
      JY=1
      JY=NN(2)
      JY=512/JY
      NUM=JX*JY
      IF(NUM.LE.0) NUM=1
C Read in data
      CALL UIN('DATA',3,ISTAT)
      IF(DISP) THEN
        CALL UMAP(3,NUM,SCALE)
        CALL WRUSER(' Data displayed',ISTAT)
      ENDIF
C Read in PSF
   10 CONTINUE
      CALL UIN('PSF',7,ISTAT)
      IF(ISTAT.EQ.ERR_FRMNUL) THEN
        CALL CNPAR('PSF',ISTAT)
        GOTO 10
      ENDIF
      IF(DISP) THEN
        NUM1=MAX(NUM-1,1)
        CALL UMAP(7,NUM1,SCALE)
        CALL WRUSER(' Point spread function displayed',ISTAT)
      ENDIF
      NBYTES=4*NN(1)*NN(2)
      ISTAT=LIB$GET_VM(NBYTES,IWORK)
      IF(.NOT.ISTAT) CALL EXIT(ISTAT)
      CALL UZAP(%val(IWORK))
      CALL LIB$FREE_VM(NBYTES,IWORK)
C Create output file
   16 CONTINUE
      CALL WRIMAG('OUTPUT',FMT_R,NN,2,IFRAME,ISTAT)
      IF(ISTAT.EQ.ERR_NORMAL) GOTO 17
        CALL CNPAR('OUTPUT',ISTAT)
        GOTO 16
   17 CONTINUE
      CALL CYDSCR('DATA','OUTPUT',ISTAT)
      LINE='Maximum entropy'
      CALL DATE(LINE(18:))
      CALL ADDSCR('OUTPUT','HISTORY',LINE,1,ISTAT)
C Starting image
      CALL UIN('START',1,ISTAT)
      FLAT=ISTAT.EQ.ERR_FRMNUL
      IF(DISP.AND. .NOT.FLAT) THEN
        CALL UMAP(1,1,SCALE)
        CALL WRUSER(' Initial trial image displayed',ISTAT)
      ENDIF
C Noise level
      ERR=0.
      CALL RDKEYR('NOISE',.FALSE.,1,ERR,I,ISTAT)
      IF(ISTAT.EQ.ERR_PARNUL) THEN
C If no noise level specified, then read in file containing error map
   18   CONTINUE
        CALL UIN('ERRFILE',21,ISTAT)
        IF(ISTAT.NE.ERR_NORMAL) THEN
          CALL CNPAR('ERRFILE',ISTAT)
          GOTO 18
        ENDIF
        VARERR=.TRUE.
      ELSE
        IF(ERR.GT.0.0) GOTO 20
        CALL CNPAR('NOISE',ISTAT)
        GOTO 18
      ENDIF
C Default level
   20 CONTINUE
      DEF=0.0
      CALL RDKEYR('DEFAULT',.FALSE.,1,DEF,I,ISTAT)
      IF(DEF.LT.0.0) THEN
        CALL CNPAR('DEFAULT',ISTAT)
        GOTO 20
      ENDIF
      RETURN
      END
C
      SUBROUTINE UIN(NAME,IFILE,ISTAT)
      CHARACTER NAME*(*)
      DIMENSION  NAXISN(2)
      COMMON /MECOMF/ IFT,NDIM,NN(3),NC(5)
      INCLUDE 'STARDIR:ERRPAR.FOR/NOLIST'
      INCLUDE 'STARDIR:FMTPAR.FOR/NOLIST'
    1 CONTINUE
      CALL RDIMAG(NAME,FMT_R,2,NAXISN,I,IP,ISTAT)
      IF(ISTAT.EQ.ERR_FRMNUL) THEN
C Return if no frame specified
        RETURN
      ELSEIF(ISTAT.NE.ERR_NORMAL) THEN
C Reprompt for any other error
        CALL CNPAR(NAME,ISTAT)
        GOTO 1
      ELSEIF(NN(1).EQ.0) THEN
C If this is the first image then check dimensions and return
        DO 3 I=1,2
        II=2
        DO 2 J=1,12
        IF(NAXISN(I).EQ.II) GOTO 3
    2   II=2*II
        CALL WRERR('DIMINV')
        CALL FRDATA(NAME,ISTAT)
        CALL CNPAR(NAME,ISTAT)
        GOTO 1
    3   CONTINUE
        NN(1)=NAXISN(1)
        NN(2)=NAXISN(2)
        NN(3)=1
        RETURN
      ELSEIF(NN(1).NE.NAXISN(1) .OR. NN(2).NE.NAXISN(2)) THEN
        CALL WRERR('DIMINV',ISTAT)
        CALL FRDATA(NAME,ISTAT)
        CALL CNPAR(NAME,ISTAT)
        GOTO 1
      ENDIF
      CALL UIN1(%val(IP),IFILE)
      CALL FRDATA(NAME,ISTAT)
      RETURN
      END
C
      SUBROUTINE UIN1(FRAME,IFILE)
      DIMENSION FRAME(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      K=1
      IF(IFILE.LE.20) THEN
      NJK=NJ
      MJK=MJ
      ELSE
      NJK=NK
      MJK=MK
      ENDIF
      DO 1 I=1,NJK
      CALL UIN2(FRAME(K),ST(KC(IFILE)),MJK)
      K=K+MJ
      CALL UWRITE(IFILE)
    1 CONTINUE
      CALL UINIT
      RETURN
      END
C
      SUBROUTINE UIN2(X,Y,M)
      DIMENSION X(1),Y(1)
      DO 1 I=1,M
    1 Y(I)=X(I)
      RETURN
      END
C
      SUBROUTINE UZAP(W)
      DIMENSION  W(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFT,NDIM,NN(3),NC(5)
      COMMON /MECOMS/ ST(1)
      J=1
      K=1
      RMAX=0.
      DO 1 I=1,NJ
      CALL UREAD(7)
      CALL UZAP1(ST(KC(7)),RMAX,J,K,NACROS,NDOWN)
    1 CONTINUE
      CALL UINIT
      NACROS=1-NACROS
      NDOWN=1-NDOWN
      WRITE(IOUT,11) NACROS,NDOWN
      J=NACROS+1
      K=NDOWN+1
      IF(J.LE.0) J=J+NN(1)
      IF(K.LE.0) K=K+NN(2)
      DO 2 I=1,NJ
      CALL UREAD(7)
      CALL UZAP2(ST(KC(7)),W,NN(1),J,K)
    2 CONTINUE
      CALL UINIT
      J=1
      DO 3 I=1,NJ
      CALL MEMJ1(W(J),ST(KC(7)))
      J=J+MJ
      CALL UWRITE(7)
    3 CONTINUE
      CALL UINIT
      RETURN
   11 FORMAT(' PSF moved by ',I6,',',I6)
      END
C
      SUBROUTINE UZAP1(X,RMAX,J,K,NACROS,NDOWN)
      DIMENSION  X(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFT,NDIM,N(3),NC(5)
      N1=N(1)
      DO 2 I=1,MJ
      XX=X(I)
      IF(XX.LE.RMAX) GOTO 1
      RMAX=XX
      NACROS=J
      NDOWN=K
    1 J=J+1
      IF(J.LE.N1) GOTO 2
      J=1
      K=K+1
    2 CONTINUE
      RETURN
      END
C
      SUBROUTINE UZAP2(X,Y,N1,J,K)
      DIMENSION  X(1),Y(N1,1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFT,NDIM,N(3),NC(5)
      NEXT=N1
      DO 1 I=1,MJ
      Y(J,K)=X(I)
      J=J+1
      IF(J.GT.N1) J=1
      IF(I.LT.NEXT) GOTO 1
      NEXT=NEXT+N1
      K=K+1
      IF(K.LE.N(2)) GOTO 1
      K=1
    1 CONTINUE
      RETURN
      END
