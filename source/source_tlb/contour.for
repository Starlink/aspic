      PROGRAM CONT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   CONTOUR *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               CONTOUR  [PENS=TRUE BATCH=FALSE]
C
C
C          FUNCTION:-
C               It draws contours of a 2-d Starlink image on one of several
C               graphics  devices.  Options are available to select part of
C               the image only, and to smooth the data before contouring is
C               done.
C
C
C          USE:-
C               It  is  most  useful  for  generating  hard  copy   as   an
C               alternative to VERGREY.
C
C
C
C         USER PARAMETERS:-
C
C         DEVICE                              This is the  graphics  device
C                                             to  be used. It is  specified
C                                             by a character  string  which
C                                             is the GKS  device  number or
C                                             a    workstation    name   as
C                                             described in SGP 26
C
C         IN                                  This  is  the  2-D   Starlink
C                                             input image.
C
C         BLOCK           1                   This is the factor to be used
C                                             for  blocking the data before
C                                             contouring.
C
C         SIZE                                Only used for workstations 5,
C                                             7, 9, 10  (hardcopy devices)
C                                             and  defines the plot size in
C                                             cms..
C
C
C
C         WHOLE           Yes                 Decides if the whole or  part
C                                             of   the  picture  is  to  be
C                                             plotted.
C
C         SAMPLES                             If WHOLE=NO this defines  the
C                                             range  of pixels to be used -
C                                             4 values in the order Xstart,
C                                             Xend,Ystart,Yend.
C
C         HEIGHTS                             Up to 30 contour heights  may
C                                             be entered, separated by
C                                             commas or spaces.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         PENS            TRUE                If PENS  is  true  then  each
C                                             contour  level  will be drawn
C                                             using  a  different  GKS pen.
C                                             The  result  will  depend  on
C                                             the device i.e. the ARGS will
C                                             produce different colours but
C                                             a GOC will  produce different
C                                             line types.  If PENS is false
C                                             then the default  pen will be
C                                             used for all  contour levels.
C
C         BATCH           FALSE               In   interactive   mode   the
C                                             program will allow a user  to
C                                             type CONTROL_C  to  terminate
C                                             plotting   of   the   current 
C                                             contour level. If CONTOUR  is
C                                             run in BATCH mode then  since
C                                             CONTROL_C is  not  valid  the
C                                             program   will   fail  unless
C                                             BATCH is set to true.
C
C
C         J V Carey                RGO                             6-JAN-82
C
C modified to run over GKS
C         D J King                 RGO                            26-JAN-84
C
C
C--------------------------------------------------------------------------



C    CONTOUR PLOTTING PROGRAM ADAPTED FOR STARLINK INTERIM
C      ENVIRONMENT FROM M J CURRIE'S PROGRAM BY J V CAREY
C      1980 NOV 18
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      CHARACTER*12 DEV,DEVICE,TITLE*80
      CHARACTER*4 HEDX
      INTEGER STSAM,ENSAM,ENREC,STREC
      INTEGER STAT,OUTDIM,IDIM(2),IPOINT,BLK
      INTEGER SAMPLES(4),IDIMA(2),STATUS
      INTEGER*4 RESTAT
      CHARACTER*1 WHOLE
      REAL HT(30)
      LOGICAL CURSOR,BATCH,PENS
      INTEGER ITITLE(10)
      WHOLE = 'Y'
      CALL RDKEYL('PENS',.FALSE.,1,PENS,IVAL,STATUS)
      CALL RDKEYL('BATCH',.FALSE.,1,BATCH,IVAL,STATUS)
      IF (.NOT.BATCH) THEN
         CALL SETUP('TT')
         CALL ENABLE
      ENDIF
      BLK = 1
      ISTAT=-1
      DO WHILE (ISTAT.NE.0)
         CALL RDKEYC('DEVICE',.TRUE.,1,DEVICE,IVAL,STAT)
         IF (STAT.NE.ERR_NORMAL.AND.STAT.NE.ERR_PARNUL) THEN
            CALL WRERR('ERRDEV')
            CALL EXIT
         ELSE
            CALL DEVTRAN(DEVICE,IDEV,ICONID,ISTAT)
            IF (ISTAT.NE.0) THEN
               CALL CNPAR('DEVICE',STAT)
               CALL WRERR('ERRDIN')
            ENDIF
	    RESTAT=STR$UPCASE(DEVICE,DEVICE)
         ENDIF
      ENDDO
C
        CALL JBDEV(DEVICE)
      CALL JBINQ(XMAX,YMAX,CURSOR)
      SIZE=MIN(XMAX,YMAX)
      IF (IDEV.EQ.9.OR.
     +    IDEV.EQ.10.OR.
     +    IDEV.EQ.5.OR.
     +    IDEV.EQ.7) THEN
         CALL RDKEYR('SIZE',.TRUE.,1,SIZE,IVAL,STAT)
         IF (STAT.NE.ERR_NORMAL.AND.STAT.NE.ERR_PARNUL) THEN
            CALL WRERR('ERRSIZ')
            CALL EXIT
         ENDIF
      ENDIF
C
C     OPEN FILE
C
      CALL RDIMAG('IN',FMT_R,2,IDIM,OUTDIM,IPOINT,STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRFLB')
      CALL EXIT
      ENDIF
      CALL RDDSCR('IN','COMMENT',1,TITLE,IVAL,STATUS)
	IF(IVAL.EQ.0) THEN
	DO NNN = 1,80
	  TITLE(NNN:NNN)=' '
	ENDDO
	ENDIF
      READ(TITLE,900) ITITLE
900   FORMAT(10A4)
200   CALL RDKEYC('WHOLE',.TRUE.,1,WHOLE,IVAL,STAT)
      IF(STAT.NE.ERR_PARNUL.AND.STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRANS')
      CALL FRDATA(' ',STAT)
      CALL EXIT
      ENDIF
      RESTAT=STR$UPCASE(WHOLE,WHOLE)
      IF(WHOLE.NE.'Y'.AND.WHOLE.NE.'N') THEN
      CALL CNPAR('WHOLE',STAT)
      GO TO 200
      ENDIF
      IF(WHOLE.EQ.'N') THEN
      CALL WRUSER('STSAM,ENSAM,STREC,ENREC',STAT)
300   CALL RDKEYI('SAMPLES',.FALSE.,4,SAMPLES,IVAL,STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRSAM')
      CALL FRDATA(' ',STAT)
      CALL EXIT
      ENDIF
      IF(IVAL.LT.4) THEN
      CALL WRUSER('MORE VALUES REQUIRED',STAT)
      CALL CNPAR('SAMPLES',STAT)
      GO TO 300
      ENDIF
      STSAM = SAMPLES(1)
      ENSAM = SAMPLES(2)
      STREC = SAMPLES(3)
      ENREC = SAMPLES(4)
C
C    FORM NEW FRAME FOR PLOTTING
C
      IDIMA(1) = 1 + ENSAM - STSAM
      IDIMA(2) = 1 + ENREC - STREC
      CALL GETDYN('TEMFILE',FMT_R,IDIMA(1)*IDIMA(2),IPOINTA,STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRWRIM')
      CALL FRDATA(' ',STAT)
      CALL EXIT
      ENDIF
      CALL COPYARRAY(%VAL(IPOINT),%VAL(IPOINTA),STSAM,ENSAM,
     1              STREC,ENREC,IDIM(1),IDIM(2),IDIMA(1)*IDIMA(2))
      IPOINT = IPOINTA
      IDIM(1) = IDIMA(1)
      IDIM(2) = IDIMA(2)
      ELSE
      STREC = 1
      STSAM = 1
      ENREC = IDIM(2)
      ENSAM = IDIM(1)
      ENDIF
C
C     OBTAIN BLOCKING FACTOR AND BLOCK IF NEEDED
C
400   CALL RDKEYI('BLOCK',.TRUE.,1,BLK,IVAL,STAT)
      IF(STAT.NE.ERR_PARNUL.AND.STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRBLK')
      CALL FRDATA(' ',STAT)
      CALL EXIT
      ENDIF
      IF(BLK.LT.1) THEN
      CALL CNPAR('BLOCK',STAT)
      GO TO 400
      ENDIF
      IF(BLK.GT.1) THEN
      IDIMA(1) = (ENSAM - STSAM +1)/BLK
      IDIMA(2) = (ENREC - STREC +1)/BLK
      CALL GETDYN('ZFILE',FMT_R,IDIMA(1)*IDIMA(2),IPOINTA,
     1             STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRZFIL')
      CALL FRDATA(' ',STAT)
      CALL EXIT
      ENDIF
      CALL BLOCKIT(%VAL(IPOINT),%VAL(IPOINTA),BLK,
     1             IDIM(1),IDIM(2),IDIMA(1)*IDIMA(2))
      IF(BLK.EQ.1) THEN
      CALL FRDATA('IN',STAT)
	CALL CNPAR('IN',STAT)
      ELSE
      CALL FRDATA('TEMFILE',STAT)
	CALL CNPAR('TEMFILE',STAT)
      ENDIF
      IPOINT = IPOINTA
      IDIM(1) = IDIMA(1)
      IDIM(2) = IDIMA(2)
      ENDIF
      CALL RDKEYR('HEIGHTS',.FALSE.,30,HT,IVAL,STAT)
      IF(STAT.NE.ERR_NORMAL)THEN
      CALL WRERR('ERRHT')
      CALL FRDATA(' ',STAT)
      CALL EXIT
      ENDIF
      NH = IVAL
      CALL CONTOUR(IDEV,SIZE,HT,NH,STSAM,ENSAM,
     1             STREC,ENREC,%VAL(IPOINT),IDIM(1),
     2             IDIM(2),ITITLE,BLK,PENS)
      CALL FRDATA(' ',STAT)
      CALL EXIT
      END
      SUBROUTINE COPYARRAY(IDSA,IDSB,SS,ES,SR,ER,ID1,ID2,IDA1)
      REAL*4 IDSA(ID1,ID2),IDSB(IDA1)
      INTEGER SS,ES,SR,ER
      N = 1
      DO K = SR,ER
      DO J = SS,ES
      IDSB(N) = IDSA(J,K)
      N = N + 1
      ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE BLOCKIT(IDA,IDB,BLK,I1,I2,I3)
C
C    SMOOTHING ROUTINE
C
      REAL*4 IDA(I1,I2),IDB(I3)
	INTEGER*2 BLK
      AB = BLK * BLK
      KK = 1
      DO J = 1,I2-BLK+1,BLK
      DO K = 1,I1-BLK+1,BLK
      A= 0.0
      DO L = 0,BLK-1
      DO M = 0,BLK-1
      A = A + IDA(K+M,J+L)
      ENDDO
      ENDDO
      IDB(KK) = A/AB
      KK = KK + 1
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE CONTOUR(IDEV,SIZE,HT,NH,STSAM,ENSAM,
     1                  STREC,ENREC,IDENS,IX,IY,ITITLE,
     1                  IBLK,PENS)
      INTEGER STSAM,ENSAM,STREC,ENREC
      REAL*4 IDENS(IX,IY)
      INTEGER*4 ITITLE(10)
      REAL*4 XD(2),YD(2),AX,AY,HT(30)
      CHARACTER CURPOS*15,CHEIGHT*40
      LOGICAL PENS
      IF (IX.GE.IY) THEN
         AX=SIZE
         AY = AX * REAL(IY)/REAL(IX)
      ELSE
         AY=SIZE
         AX = AY * REAL(IX)/REAL(IY)
      ENDIF
      XD(1)=FLOAT(STSAM)
      XD(2)=FLOAT(ENSAM)
      YD(1)=FLOAT(STREC)
      YD(2)=FLOAT(ENREC)
      LC = 1 + ENSAM -STSAM
      LR = 1 + ENREC - STREC
      CALL JBAXES(XD,2,AX,'X-AXIS',6,YD,2,AY,'Y-AXIS',6)
      CALL TITLE(1,2,ITITLE,40)
      IF(IBLK.NE.1) THEN
      LC = LC/IBLK
      LR = LR/IBLK
      ENDIF
      DO 116 K=1,NH
        IF (PENS) THEN
		IF(IDEV.EQ.1.OR.IDEV.EQ.15) THEN
		MP = MOD(K,10)+1
		ELSE
		MP = MOD(K,3)+1
		ENDIF
	      CALL PEN(MP)
        ENDIF
      WRITE(CHEIGHT,9999) HT(K)
9999  FORMAT('DRAWING CONTOUR AT HEIGHT', F8.1)
      CALL WRUSER(CHEIGHT,ISTATUS)
116   CALL LEVEL(HT(K),IDENS,XD(1),REAL(IBLK),LC,
     1                        YD(1),REAL(IBLK),LR)
      CALL END PLT
      RETURN
      END
      SUBROUTINE JVCPOS(XA,YA,XX,YY,STSAM,STREC,
     1                 ENSAM,ENREC)
      COMMON/JBJUD/K,MISS,XL,X1,X2,XS,YL,Y1,Y2,YS,
     1    F,S,MS(2),GAP,IPOL,G(2)
      COMMON/JBPT/X,Y,INSIDE,M
      CALL JBNRNG(STSAM,STREC)
      AX = X
      AY = Y
      CALL JBNRNG(ENSAM,ENREC)
      BX = X
      BY = Y
      XX = (XA-AX)/(BX-AX)*(ENSAM-STSAM)+STSAM
      YY = (YA-AY)/(BY-AY)*(ENREC-STREC)+STREC
      RETURN
      END

      SUBROUTINE LEVEL(E,ZZ,XX,DX,NX,YY,DY,NY)
      LOGICAL CTRLC
      DIMENSION AX(2),AY(2),X(3),Y(3),Z(3),I(4),XA(2),YA(2)
      REAL*4 ZZ(1)
      COMMON/JBJUD/K,MISS,D11,DR(7),F,S,MS(2),GAP,IPOL,EE(2)
      EQUIVALENCE (I(1),I1),(I(2),I2),(I(3),I3),(I(4),I4)
      NX1=NX-1
      NY1=NY-1
      AY(1)=YY
      I3=1
      DO 50 JJJ=1,NY1
      AY(2)=AY(1)+DY
      Y(3)=AY(1)+DY*0.5
      DO 40 III=1,NX1
      I1=I3
      I2=I1+NX
      I3=I1+1
      I4=I3+NX
      B=ZZ(I4)-E
      DO 10 II=1,3
      IF(B*(ZZ(I(II))-E).LE.0.0)GO TO 20
10    CONTINUE
      GO TO 40
20    AX(2)=XX+FLOAT(III)*DX
      AX(1)=AX(2)-DX
      X(3)=(AX(1)+AX(2))*0.5
      Z(3)=(ZZ(I1)+ZZ(I2)+ZZ(I3)+ZZ(I4))*0.25
      KK=0
      LL=5
      DO 30 II=1,2
      X(1)=AX(II)
      Y(2)=AY(3-II)
      DO 30 JJ=1,2
      KK=KK+1
      LL=8-KK-LL
      X(2)=AX(JJ)
      Y(1)=AY(JJ)
      Z(1)=ZZ(I(KK))
      Z(2)=ZZ(I(LL))
      CALL JBLIMS(Z,3,B,IB,T,IT)
      IF(E.GT.T.OR.E.LE.B)GO TO 30
      CALL JBLINE(X,Y,Z,E,XA,YA,IB,IT)
      CALL JBJOIN(XA,YA)
30    CONTINUE
40    CONTINUE
      IF(CTRLC(0)) GO TO 55
      I3=I3+1
50    AY(1)=AY(2)
55    CONTINUE
      CALL JBPRGE
      K=1
      RETURN
      END
