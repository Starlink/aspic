C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   PATCH *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               PATCH  [ NOISE=f ]  [ ORDER=n ]  [ CURSOR=t ]  [ FILE=f ]
C
C
C          FUNCTION:-
C               It allows the user to replace several circular  patches  in
C               an image with a fitted noisy piece of synthetic data. It is
C               derived from a program, by  W  D  Pence (at  University  of
C               Sussex).The ARGS cursor and trackerball are used to control
C               the process.
C
C
C          USE:-
C               It may be used to remove large defects, bright galaxies  or
C               any other localised unwanted pixels. The result can be very
C               convincing. If  the  defects  are  more  like  strips  than
C               circular patches the program ZAPLIN should be used.
C
C
C
C         USER PARAMETERS:-
C
C         IN                                  The input 2-D image which  is
C                                             being  displayed on the ARGS.
C
C         OUTPUT                              The  new   image   with   the
C                                             patched regions.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         NOISE           1                   This is a noise factor  which
C                                             may  be  between 0 (no noise)
C                                             and 1 (noise level calculated
C                                             from the whole image).
C
C         ORDER           3                   This is the the degree of the
C                                             2-dimensional  surface  which
C                                             is to be fitted to an annulus
C                                             around the circular patch. It
C                                             may  be  0  (constant)  to  3
C                                             (bi-cubic).
C
C
C         CURSOR          TRUE                This defines if the cusor will
C                                             be used to define the patches.
C                                             If FALSE then the position and
C                                             size of the  patches  will  be
C                                             read from the file PATCHES.DAT
C
C
C         FILE            FALSE               This defines  if  the position
C                                             and size of  the  patches will
C                                             be   written   to   the   file
C                                             PATCHES.DAT
C
C
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Accept this size of patch  and  then  do  a  fit  and
C                     display  the results. AFTER THIS BUTTON has been used
C                     then GREEN means accept the result  and  move  on  to
C                     another  location,  whilst  RED  means  revert to the
C                     previous state.
C
C         WHITE 2     Decrease the size of the patch.
C
C         WHITE 3     Increase the size of the patch.
C
C         RED   4     Exit from the program.
C
C
C
C
C
C
C         D J King-W D Pence       RGO-U. of Sussex                7-JAN-82
C
C
C--------------------------------------------------------------------------



	PROGRAM PATCH
	PARAMETER NDIMER=200
	INTEGER DIM(2)
	LOGICAL TRIM,LLOG,CURSOR,FILE
	CHARACTER VALUE*80,COM*2
	REAL VLO,VHI,ZXC,ZYC,ER(NDIMER)
      INCLUDE 'INTERIM(FMTPAR)'
	CALL ARGS_NUMIM(IDMAX)
	IF (IDMAX.EQ.0) THEN
		CALL WRERR('NOIMS')
	ELSE
	CALL SRINIT(0,.FALSE.,JSTAT)
		IF (JSTAT.NE.0) THEN
			CALL WRERR('NOARGS')
		ELSE
			CALL ARGS_RDIM(IMX,IMY,ISX,ISY,I,I,JSTAT)
			IXOR=IMX-(ISX/2)
			IYOR=IMY-(ISY/2)
	CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
	CALL ASP_DZTOL('TRIM',VALUE,TRIM,JSTAT)
	CALL ASP_DZTOL('LOG',VALUE,LLOG,JSTAT)
	CALL ASP_DZTOF('PVLO',VALUE,VLO,JSTAT)
	CALL ASP_DZTOF('PVHI',VALUE,VHI,JSTAT)
	CALL ASP_DZTOF('ZXC',VALUE,ZXC,JSTAT)
	IXPOS=NINT(ZXC)
	IF (IXPOS.EQ.0) IXPOS=256
	CALL ASP_DZTOF('ZYC',VALUE,ZYC,JSTAT)
	IYPOS=NINT(ZYC)
	IF (IYPOS.EQ.0) IYPOS=256
	CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
	CALL RDIMAG('IN',FMT_R,2,DIM,IADIM,IPNT,JSTAT)
	CALL WRIMAG('OUTPUT',FMT_R,DIM,2,IOPNT,JSTAT)
	CALL OUTDATA(%VAL(IPNT),%VAL(IOPNT),DIM(1),DIM(2))
        CALL FRDATA('IN',JSTAT)
        CALL CNPAR('IN',JSTAT)
	ICOL=DIM(1)
	IROW=DIM(2)
C
C     SET UP ARRAY OF RANDOM ERRORS WITH GAUSSIAN DISTRIBUTION
C     FOR APPROXIMATING THE NOISE WHEN INTERPOLATING
C
	SIG=0.0
	CALL RDKEYR('NOISE',.TRUE.,1,SIG,NVALS,JSTAT)
C
C     USE NAG ROUTINE TO GENERATE NORMAL ERRORS, MEAN=0, SIGMA=1
C
      CALL G05CBF(0)
        DO 2 I=1,200
2       ER(I)=G05DDF(0.,SIG)
	NCODE=3
	CALL RDKEYI('ORDER',.TRUE.,1,NCODE,NVALS,JSTAT)
	CURSOR=.TRUE.
	CALL RDKEYL('CURSOR',.TRUE.,1,CURSOR,NVALS,JSTAT)
	IF (.NOT.CURSOR) OPEN(UNIT=9,STATUS='OLD',FILE='PATCHES.DAT')
	FILE=.FALSE.
	CALL RDKEYL('FILE',.TRUE.,1,FILE,NVALS,JSTAT)
	IF (FILE) OPEN(UNIT=9,STATUS='NEW',FILE='PATCHES.DAT')
	CALL STAR(%VAL(IOPNT),ICOL,IROW,TRIM,LLOG,VLO,VHI,
     1	IXPOS,IYPOS,IXOR,IYOR,ER,NDIMER,NCODE,CURSOR,FILE)
	CALL FRDATA('OUTPUT',JSTAT)
	CALL CNPAR('OUTPUT',JSTAT)
	ENDIF
	ENDIF
	IF (FILE.OR.CURSOR) CLOSE(UNIT=9)
	END
	SUBROUTINE OUTDATA(IN,OUT,N,M)
	REAL IN(N,M),OUT(N,M)
	DO 200 J=1,M
		DO 100 I=1,N
		OUT(I,J)=IN(I,J)
100		CONTINUE
200	CONTINUE
	RETURN
	END
      SUBROUTINE STAR(ARRAY,ICOL,IROW,TRIM,LLOG,VLO,VHI,
     1 IXPOS,IYPOS,IXOR,IYOR,ER,NDIMER,NCODE,CURSOR,FILE)
C
C ************************************************************
C
C     SUBROUTINE STAR(ARRAY,ICOL,IROW,TRIM,LLOG,VLO,VHI,
C                     IXPOS,IYPOS,IXOR,IYOR,ER,NDIMER,NCODE,
C                     CURSOR,FILE)
C
C     PACKAGE OF ROUTINES FOR FINDING AND REMOVING DEFECTS FROM
C     AN IMAGE ARRAY.  THE POSITION OF THE AREA TO DELETE MAY
C     BE SPECIFIED MANUALLY USING THE CURSOR, OF BY A SIMPLE
C     SEARCH ALGORITHM WHICH FINDS THE NEXT PIXEL ABOVE A
C     GIVEN THRESHOLD AND ALSO GREATER THAN ITS 8 NEIGHBOURING
C     PIXELS.  THE USER IS PROMPTED FOR THE DIAMETER OF A CIRCLE
C     OF PIXELS TO DELETE AROUND THIS POSITION, WHICH ARE
C     REPLACED BY AN INTERPOLATION OF A SURFACE FITTED TO
C     AN ANNULUS OF SURROUNDING PIXELS.  A CONSTANT, PLANAR,
C     QUADRATIC, OR CUBIC SURFACE MAY BE SPECIFIED (1,3,6, OR 10
C     TERMS).  THE INTERPOLATION IS DISPLAYED ON THE ARGS FOR
C     THE USER TO JUDGE THE QUALITY OF FIT.  IF NECESSARY, THE
C     ORIGINAL VALUES OF THE PIXELS MAY BE RESTORED AND A
C     DIFFERENT INTERPOLATION ATTEMPTED.
C
C     IRAY = 2 DIMENSIONAL IMAGE ARRAY (I*2)
C     ICOL = SIZE OF THE FIRST DIMENSION OF IRAY (I*4)
C     IROW = SIZE OF THE SECOND DIMENSION OF IRAY (I*4)
C
C     WRITTEN BY W D PENCE, UNIV. OF SUSSEX, OCT. 1980
C
C ****************************************************************
C
      REAL ARRAY(ICOL,IROW),ER(NDIMER)
	DIMENSION IRDATA(3)
	LOGICAL TRIM,LLOG,CURSOR,FILE
      CHARACTER BUFFER*80,INDCOL*1,CURCOL*1
C
	IRADIUS=20
	CURCOL='B'
	INDCOL='R'
	ITYPE=1
	NPOS=1
70	CONTINUE
	IF (CURSOR) THEN
		CALL ACURS(ITYPE,NPOS,IRADIUS,IXPOS,IYPOS,IRET,
     1  	           CURCOL,INDCOL,IRDATA)
		IF (IRET.EQ.0) GO TO 5
	NS=IRDATA(1)*2
	IXPOS=IRDATA(2)
	IYPOS=IRDATA(3)
	IS=IRDATA(2)-(IXOR)+1
	JS=IRDATA(3)-(IYOR)+1
	IRADIUS=IRDATA(1)
	ELSE
		READ (9,*,END=5) IS,JS,NS
	ENDIF
      CALL STRDELT(ARRAY,ICOL,IROW,IS,JS,TRIM,LLOG,VLO,VHI,
     1 NS,IXOR,IYOR,ER,NDIMER,NCODE,FILE,CURSOR)
      GO TO 70
5	CONTINUE
	END
      SUBROUTINE STRDELT(ARRAY,ICOL,IROW,IS,JS,TRIM,LLOG,VLO,VHI,
     1 NS,IXOR,IYOR,ER,NDIMER,NCODE,FILE,CURSOR)
C
C ****************************************************************
C
C     DELETES PIXELS WITHIN A CIRCLE AROUND POINT (IS,JS) AND
C     INTERPOLATES THEM WITH A POLYNOMIAL FITTED TO AN ANNULUS
C     OF SURROUNDING POINTS.
C
C ****************************************************************
C
      INCLUDE 'INTERIM(FMTPAR)'
      PARAMETER NDIM=1000
      REAL ARRAY(ICOL,IROW),ER(NDIMER)
      DOUBLE PRECISION ZZ,D(30),SE(30),RDOE(30),DUMMY
      CHARACTER ANS*1
	DIMENSION XX(NDIM,2),YY(NDIM),ZZ(NDIM)
	LOGICAL TRIM,LLOG,FILE,CURSOR
C
C
      IRAD2=(NS/2)**2
C
C     DEFINE LIMITS OF AREA TO DELETE
C
      I1=MAX(IS-NS/2,1)
      I2=MIN(IS+NS/2,ICOL)
      J1=MAX(JS-NS/2,1)
      J2=MIN(JS+NS/2,IROW)
C
C     TEMPORARILY STORE CURRENT ARRAY CONTENTS
C
      ISIZE=(I2-I1+1)*(J2-J1+1)
	CALL GETDYN('*STAR',FMT_R,ISIZE,IPNT,ISTATUS)
      CALL STRCOPY(ARRAY,IROW,ICOL,%VAL(IPNT),ISIZE,I1,I2,J1,J2)
	XOLD=ARRAY(IS,JS)
C
C     SET UP ARRAYS FOR NEQSOL
C
      FACTOR=2
      IF (NCODE .EQ. 0)FACTOR=1.5
      NWIDE=NS*FACTOR
      NWIDE2=(NWIDE+1)/2
      NWIDE2S=NWIDE2*NWIDE2
      NINC=(NWIDE-1)/30+1
      I11=MAX(IS-NWIDE2,1)
      I22=MIN(IS+NWIDE2,ICOL)
      J11=MAX(JS-NWIDE2,1)
      J22=MIN(JS+NWIDE2,IROW)
C
C     DEFINE NORMALIZING FACTORS SUCH THAT THE COORDS. OF
C     ALL THE POINTS ARE BETWEEN (-1,-1) AND (1,1).
C
      DX=2./(I22-I11)
      DY=2./(J22-J11)
      NPTS=0
      DO 20 J=J11,J22,NINC
      DO 20 I=I11,I22,NINC
C
C     CALC RADIUS, AND REJECT IF TOO CLOSE TO STAR CENTRE
C
      KRAD2=(J-JS)**2+(I-IS)**2
      IF (KRAD2 .LE. IRAD2 .OR. KRAD2 .GT. NWIDE2S)GO TO 20
      NPTS=NPTS+1
C
      XX(NPTS,1)=(I-I11)*DX-1.
      XX(NPTS,2)=(J-J11)*DY-1.
	YY(NPTS)=ARRAY(I,J)
20    CONTINUE
C
30    IF (NCODE .EQ. 3)THEN
        NTERM=10
      ELSE IF (NCODE .EQ. 1)THEN
        NTERM=3
      ELSE IF (NCODE .EQ. 2)THEN
        NTERM=6
      ELSE
        NTERM=1
      END IF
C
      IF (NPTS .LT. NTERM)THEN
       CALL WRUSER('ERROR: TOO FEW POINTS TO FIT BACKGROUND',ISTATUS)
        GO TO 200
      END IF
C
C     ERASE ANY PREVIOUS SOLUTION
C
      DO 35 I=1,30
35    D(I)=0.
C
      DO 40 KQ=1,2
C
C     FIT THE POLYNOMIAL
C
      CALL NEQSOL(XX,YY,ZZ,NDIM,NPTS,NTERM,1,0,D,SE,RDOE,SDOR)
C
C     DO A 2 SIGMA REJECTION
C
      TEST=2.*SDOR
      CALL REJECT(XX,YY,NDIM,D,NPTS,NTERM,TEST,NR,1,-1)
      NPTS=NR
      IF (NPTS .LT. NTERM)THEN
       CALL WRUSER('ERROR: TO FEW POINTS TO FIT BACKGROUND',ISTATUS)
        GO TO 200
      END IF
C
40    CONTINUE
C
C     REFIT POLYNOMIAL
C
      CALL NEQSOL(XX,YY,ZZ,NDIM,NPTS,NTERM,1,0,D,SE,RDOE,SD)
C
C     EVALUATE POLYNOMIAL AT EACH POINT WITHIN CIRCLE
C
      DO 60 J=J1,J2
C
C     FIND A RANDOM STARTING POINTS IN THE SEQUENCE OF NOISE POINTS
C
      XK=G05CAF(DUMMY)
      K=XK*199.
C
      YZ=(J-J11)*DY-1.
      DO 50 I=I1,I2
      IF ((J-JS)**2+(I-IS)**2 .GT. IRAD2)GO TO 50
      XZ=(I-I11)*DX-1.
      K=K+1
      IF (K .GT. 200)K=1
      XNOISE=ER(K)*SDOR
      VALUE=POLY(XZ,YZ,D)+XNOISE
	ARRAY(I,J)=VALUE
50    CONTINUE
C
C     PLOT NEW LINE ON ARGS
C
      CALL ARGSROW(ARRAY,ICOL,IROW,I1,I2,J,
     1     TRIM,LLOG,VLO,VHI,IXOR,IYOR)
C
60    CONTINUE
C
	IF (CURSOR) THEN
	CALL ARGS_LAMPS(1,0,0,1)
	CALL ARGS_CURS('+')
	CALL ARGS_CURC('I')
	CALL ARGS_RDCUR(' ',ID,IB,UX,UY)
	CALL ARGS_LAMPS(0,0,0,0)
      IF (IB .EQ. 4)THEN
      CALL STRRSTR(ARRAY,IROW,ICOL,%VAL(IPNT),ISIZE,I1,I2,J1,J2)
        DO 230 J=J1,J2
        CALL ARGSROW(ARRAY,ICOL,IROW,I1,I2,J,
     1     TRIM,LLOG,VLO,VHI,IXOR,IYOR)
230     CONTINUE
        CALL FRDATA('*STAR',ISTATUS)
      GO TO 300
      ELSE
	IF (FILE) WRITE (9,*) IS,JS,NS
      ENDIF
	ENDIF
C
200   CONTINUE
C
      CALL FRDATA('*STAR',ISTATUS)
C
300	CONTINUE
      END
      SUBROUTINE STRCOPY(ARRAY,IROW,ICOL,TEMP,ISIZE,I1,I2,J1,J2)
C
C     STORE CURRENT STAR IMAGE IN TEMPORARY ARRAY
C
      REAL ARRAY(ICOL,IROW),TEMP(ISIZE)
C
      N=0
      DO 10 J=J1,J2
      DO 10 I=I1,I2
      N=N+1
10    TEMP(N)=ARRAY(I,J)
      RETURN
C
      ENTRY STRRSTR(ARRAY,IROW,ICOL,ITEMP,ISIZE,I1,I2,J1,J2)
C
C     RESTORE ARRAY TO PREVIOUS STATE
C
      N=0
      DO 20 J=J1,J2
      DO 20 I=I1,I2
      N=N+1
20    ARRAY(I,J)=TEMP(N)
      END
      SUBROUTINE PROMPTC(PREFIX,NAME,BUFFER,LENGTH,ISTATUS)
      CHARACTER*(*) PREFIX,BUFFER,NAME
      CALL WRUSER(PREFIX,ISTAT)
      CALL CNPAR(NAME,ISTAT)
      CALL RDKEYC(NAME,.FALSE.,1,BUFFER,I,ISTATUS)
      IF (ISTATUS .NE. 0)RETURN
      ISIZE=LEN(BUFFER)
      DO 10 I=ISIZE,1,-1
        LENGTH=I
        IF (BUFFER(I:I) .NE. ' ')GO TO 20
10    CONTINUE
20    RETURN
      END
C
      SUBROUTINE PROMPTI(PREFIX,NAME,IVALUE,ISTATUS)
      CHARACTER*(*) PREFIX,NAME
      CALL WRUSER(PREFIX,ISTAT)
      CALL CNPAR(NAME,ISTAT)
      CALL RDKEYI(NAME,.FALSE.,1,IVALUE,I,ISTATUS)
      END
      SUBROUTINE NEQSOL(X,Y,Z,NDIM,NP,NF,NOTO,IOOR,D,SE,
     1 RDOE,SDOR)
C
C **************************************************************
C
C     SUBROUTINE NEQSOL(X,Y,Z,NDIM,NP,NF,NOTO,IOOR,D,SE,RDOE,SDOR)
C
C THIS SUBROUTINE COMPUTES THE COEFFICIENTS, D, WHICH DEFINE THAT LINEAR
C FUNCTION, Y, OF LINEARLY INDEPENDENT FUNCTIONS WHICH BEST FITS, IN THE
C LEAST SQUARES SENSE, A GIVEN SET OF DATA. OR EQUIVALENTLY, IT FINDS
C THE SOLUTION TO THE SYSTEM OF NORMAL EQUATIONS WHICH IS CALLED THE
C NORMAL EQUATIONS SOLUTION.
C
C     WRITTEN AND DOCUMENTED BY:
C
C     JONES, W B, OBITTS, D L, GALLET, R M, AND DE VAUCOULEURS, G,
C     'ASTRONOMICAL SURFACE PHOTOMETRY BY NUMMERICAL MAPPING
C     TECHNIQUES', PUBLICATION OF THE ASTRONOMY DEPARTMENT, UNIV.
C     OF TEXAS, AUSTIN, SERIES II, VOL. I, NO. 8, FEB. 1967
C
C     MODIFIED BY W D PENCE, UNIV. OF SUSSEX, SEPT. 1980
C
C     X(NP,2) = COORDINATES OF POINTS: X(I,1) = X COORD.  (R*4)
C                                      X(I,2) = Y COORD.  (R*4)
C     Y(NP) = VALUE OF POINT AT X(NP,1),X(NP,2)  (R*4)
C     Z = SCRATCH ARRAY  (DOUBLE PRECISION, R*8)
C     NDIM = FIRST DIMENSION OF X, Y AND Z (I*4)
C     NP = NO. OF POINTS IN LEAST SQUARES FIT  (I*4)
C     NF = NO. OF COEFS TO BE SOLVED FOR  (I*4)
C     NOTO = NO. OF TOTAL ORTHOGONALIZATIONS TO PERFORM (I*4)
C     IOOR = POSITIVE INTEGER IF OPTIONAL OUTPUT IS REQUIRED
C     D(NF) = COEFFICIENTS OF THE POLYNOMIAL (DOUBLE PRECISION, R*8)
C     SE(K) = RMS OF FIT USING ONLY THE FIRST K COEFFICIENTS (R*8)
C     RDOE(NF) = INDICATES THE STATISTICAL SIGNIFICANCE OF EACH TERM (R*8)
C     SDOR = STANDARD DEVIATION OF RESIDUALS  (R*4)
C
C ***************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H),(O-Z)
      REAL X(NDIM,2),Y(NDIM),SDOR,XGGG,YGGG
      DOUBLE PRECISION Z(NDIM),D(30)
      DIMENSION A(30),AKJ(30,30),AA(30),CE(30),DT(30),DYGC(30),
     1 GGSQT(30),GG(30,30),Q(30,30,2),RDOE(30),REE(30),RREE(30),RYG(30),
     2 SD(30),SE(30),S(30,30),YGC(30),YG(30)
      GO TO  (1,9)NOTO
C MPUTE THE SQUARE OF THE NORM OF THE VECTOR OF THE DEPENDENT VARIABLE,
C MPUTE THE MEAN OF THE DEPENDENT VARIABLE, YBAR.
1     FLNP=NP
      YY=Y(1)*Y(1)
      YBAR=Y(1)
      DO 2 I=2,NP
      YY=YY+Y(I)*Y(I)
2     YBAR=YBAR+Y(I)
C MPUTE THE MATRIX OF INNER PRODUCTS OF THE NORMALIZED FITTING FUNCTIONS
      YBAR=YBAR/FLNP
      IX=0
      JY=-1
      DO 8 J=1,NF
      IF (IX .NE. 0)THEN
      IX=IX-1
      JY=JY+1
      ELSE
      IX=JY+1
      JY=0
      END IF
      T=0.
      SUM=0.
      DO 4 I=1,NP
      GK=1.
      IF (IX .NE. 0) GK=X(I,1)**IX
      IF (JY .NE. 0) GK=GK*X(I,2)**JY
      Z(I)=GK
      T=T+GK*GK
4     SUM=SUM+Y(I)*GK
      GGSQT(J)=SQRT(T)
      YG(J)=SUM/GGSQT(J)
      GG(J,J)=1.0
      IF (J-1)8,8,5
5     KUL=J-1
      IXX=0
      JYY=-1
      DO 7 K=1,KUL
      IF (IXX .NE. 0)THEN
      IXX=IXX-1
      JYY=JYY+1
      ELSE
      IXX=JYY+1
      JYY=0
      END IF
      T=0.
      DO 6 I=1,NP
      GK=1.
      IF (IXX .NE. 0) GK=X(I,1)**IXX
      IF (JYY .NE. 0) GK=GK*X(I,2)**JYY
6     T=T+Z(I)*GK
      GG(J,K)=T/(GGSQT(J)*GGSQT(K))
7     GG(K,J)=GG(J,K)
8     CONTINUE
C MPUTE THE MATRIX OF COEFFICIENTS, Q, DEFINING THE ORTHOGONAL FUNCTIONS
C IN TERMS OF THE FITTING FUNCTIONS.
9     Q(1,1,NOTO)=1.
      S(1,1)=1.
      AA(1)=1.
      GO TO  (10,12)NOTO
10    DO 11 K=2,NF
11    AKJ(K,1)=-GG(K,1)
      GO TO 15
12    DO 14 K=2,NF
      SUM=0.
      DO 13 J=1,K
13    SUM=SUM+Q(K,J,1)*GG(J,1)
14     AKJ(K,1)=-SUM
15    DO 32 K=2,NF
      Q(K,K,NOTO)=1.
      S(K,K)=1.
      JUL=K-1
      DO 19 J=1,JUL
      GO TO (16,17)NOTO
16    T=AKJ(K,J)
      GO TO 19
17    T=0.
      DO 18 L=J,JUL
18    T=T+AKJ(K,L)*S(L,J)
19    S(K,J)=T
      DO 23 J=1,JUL
      SUM=0.
      DO 20 L=J,JUL
20    SUM=SUM+S(K,L)*Q(L,J,1)
      GO TO (21,22)NOTO
21    Q(K,J,1)=SUM
      GO TO 23
22    Q(K,J,2)=SUM+Q(K,J,1)
23    CONTINUE
      SUM=0.
      DO 25 J=1,K
      T=0.
      DO 24 L=1,K
C MPUTE THE VECTOR OF THE SQUARE OF THE NORM OF THE ORTHOGONAL FUNCTIONS
24    T=T+Q(K,L,NOTO)*GG(L,J)
25    SUM=SUM+Q(K,J,NOTO)*T
      AA(K)=SUM
      IF (K-NF)26,32,32
26    KPO=K+1
      DO 31 J=KPO,NF
      SUM=0.
      DO 30 L=1,K
      GO TO (27,28)NOTO
27    T=GG(J,L)
      GO TO 30
28    T=0.
      DO 29 M=1,J
29    T=T+Q(J,M,1)*GG(M,L)
30    SUM=SUM+Q(K,L,NOTO)*T
31    AKJ(J,K)=-SUM/AA(K)
32    CONTINUE
C MPUTE THE LEAST SQUARES COEFFICIENTS, A, FOR THE SOLUTION IN TERMS OF
C THE ORTHOGONAL FUNCTIONS.
      DO 34 K=1,NF
      SUM=0.
      DO 33 J=1,K
33    SUM=SUM+Q(K,J,NOTO)*YG(J)
34    A(K)=SUM/AA(K)
C MPUTE THE LEAST SQUARES COEFFICIENTS,D, FOR THE SOLUTION IN TERMS OF
C THE FITTING FUNCTIONS.
      DO 36 K=1,NF
      SUM=0.
      DO 35 J=K,NF
35    SUM=SUM+Q(J,K,NOTO)*A(J)
      D(K)=SUM/GGSQT(K)
36    DT(K)=SUM
C MPUTE THE STANDARD DEVIATION OF THE RESIDUALS, SDOR.
      SDOR=0.
      DO 38 I=1,NP
      XGGG=X(I,1)
      YGGG=X(I,2)
      SUM=POLY(XGGG,YGGG,D)
      T=Y(I)-SUM
38    SDOR=SDOR+T*T
      SDOR=SQRT(SDOR/(FLNP-NF))
      IF (IOOR)99,99,39
C MPUTE THE OPTIONAL OUTPUT ONLY IF (IOOR) IS POSITIVE.
39    DO 42 K=1,NF
C MPUTE THE CHECK FOR THE CONSISTENCY OF THE COEFFICIENTS D.
40    Z(K+200)=YG(K)
      SUM=0.
      DO 41 J=1,NF
41    SUM=SUM+DT(J)*GG(K,J)
      YGC(K)=SUM
      DYGC(K)=SUM-YG(K)
42    RYG(K)=DYGC(K)/YG(K)
C MPUTE THE ORTHONORMAL COEFFICIENTS, THE SUM OF THE SQUARES OF RESIDUALS,
C THE ESTIMATES OF THE STANDARD DEVIATION OF THE RESIDUALS, AND THE
C TERM SIGNIFICANCE RATIO.
      SD(1)=A(1)
      CE(1)=YY-A(1)*A(1)*AA(1)
      DEN=NP-1
      SE(1)=SQRT(CE(1)/DEN)
      RDOE(1)=SD(1)/SE(1)
      DO 50 K=2,NF
      IF (AA(K)) 43,44,45
43    SD(K)=-SQRT(-AA(K))*A(K)
      GO TO 46
44    SD(K)=0.
      GO TO 46
45    SD(K)=SQRT(AA(K))*A(K)
46    CE(K)=CE(K-1)-A(K)*A(K)*AA(K)
      DEN=NP-K
      IF (CE(K))47,48,49
47    SE(K)=-SQRT(-CE(K)/DEN)
      GO TO 50
48    SE(K)=1.0E-33
      GO TO 50
49    SE(K)=SQRT(CE(K)/DEN)
50    RDOE(K)=SD(K)/SE(K)
C MPUTE THE ESTIMATE OF THE ERROR DUE TO ROUNDING AND THE RELATIVE ERROR

C IN THE LEAST SQUARES COEFFICIENTS,A.
      DO 60 K=1,NF
      KMO=K-1
      KPO=K+1
      T=0.
      IF (KMO)55,55,51
51    DO 54 J=1,KMO
      SUM=0.
      DO 53 L=1,J
      T1=0.
      DO 52 M=1,K
52    T1=T1+Q(K,M,NOTO)*GG(L,M)
53    SUM=SUM+Q(J,L,NOTO)*T1
54    T=T+A(J)*SUM
      IF(K-NF)55,59,59
55    DO 58 J=KPO,NF
      SUM=0.
      DO 57 L=1,J
      T1=0.
      DO 56 M=1,K
56    T1=T1+Q(K,M,NOTO)*GG(L,M)
57    SUM=SUM+Q(J,L,NOTO)*T1
58    T=T+A(J)*SUM
59    REE(K)=-T/AA(K)
60    RREE(K)=REE(K)/A(K)

99    CONTINUE
      RETURN
      END
      SUBROUTINE ARGSROW(ARRAY,ICOL,IROW,I1,I2,J,
     1         TRIM,LLOG,VLO,VHI,IXOR,IYOR)
      REAL ARRAY(ICOL,IROW)
	LOGICAL TRIM,LLOG
      INCLUDE 'INTERIM(FMTPAR)'
      ISIZE=I2-I1+1
      CALL GETDYN('QROW',FMT_R,ISIZE,IPNT,ISTATUS)
      IF (ISTATUS .NE. 0)THEN
        PRINT *,'FAILED TO GET WORKSPACE IN ARGSROW'
        CALL FRDATA('QROW',ISTATUS)
        RETURN
      END IF
      CALL ARGROW2(ARRAY,ICOL,IROW,I1,I2,J,%VAL(IPNT),ISIZE,
     1      TRIM,LLOG,VLO,VHI,IXOR,IYOR)
      CALL FRDATA('QROW',ISTATUS)
      END
C
      SUBROUTINE ARGROW2(PIC,ICOL,IROW,I1,I2,J,IBUFF,ISIZE,
     1          TRIM,LLOG,VLO,VHI,IXOR,IYOR)
      INTEGER*2 IBUFF(ISIZE)
	REAL PIC(ICOL,IROW)
	LOGICAL TRIM,LLOG
      INTEGER*2 I2DUMMY
C
      K=0
      VL=VLO
      D=VHI-VLO
      IF (ABS(D).LT.1E-10) D=1E10
      IF (LLOG) THEN
         IF (VL.LT.1E-10) THEN
            SCALE=255.0/LOG(1E10*D+1)
         ELSE
            SCALE=255.0/LOG(VHI/VLO)
         ENDIF
      ELSE IF (TRIM) THEN
         SCALE=255.0/D
      ENDIF
	K=0
      DO I=I1,I2
	K=K+1
         IF (LLOG) THEN
            IF (VL.LT.1E-10) THEN
               V=SCALE*LOG(1E10*(PIC(I,J)-VL)+1)
            ELSE
		IF (PIC(I,J).EQ.0) THEN
			V=0.0
		ELSE
	               V=SCALE*LOG(PIC(I,J)/VL)
		ENDIF
            ENDIF
         ELSE IF (TRIM) THEN
            V=SCALE*(PIC(I,J)-VL)
         ELSE
            V=MOD(PIC(I,J),32768.0)
         ENDIF
         IBUFF(K)=NINT(MIN(MAX(V,0.0),255.0))
      END DO
      NX1=IXOR+I1-1
      NY1=IYOR+J-1
      NP=I2-I1+1
      CALL SRPXI2(IBUFF,ISIZE,NP,1,NX1,NY1,16,.FALSE.,I2DUMMY,1)
      END
      SUBROUTINE REJECT(X,Y,NDIM,D,NN,KK,TT,M,J)
C
C *****************************************************************
C
C     SUBROUTINE REJECT(X,Y,NDIM,D,N,K,T,M,J)
C
C     WRITTEN BY JONES ET AL., 1967
C     MODIFIED BY W D PENCE, 1980
C

C     REJECT ANY POINT WHOSE RESIDUAL IS GREATER THAN BETA*SIGMA
C     FROM THE POLYNOMIAL DEFINED BY THE COEFFICIENTS D.
C
C     X = INDEPENDENT VARIABLE (R*4)
C     Y = THE DEPENDENT VARIABLE (R*4)
C     NDIM = FIRST DIMENSION OF X AND Y  (I*4)
C     D(30) = ARRAY CONTAINING THE POLYNOMIAL COEFFICIENTS (R*8)
C     N = NUMBER OF VALUES TO BE TESTED
C     K = THE NUMBER OF TERMS IN THE POLYNOMIAL
C     T = THE TEST VALUE USED TO SPECIFY THE REJECTION LEVEL
C     M = THE MODIFIED NUMBER OF VALUES AFTER REJECTION
C     J = AN INDICATOR WHICH IS ZERO ONLY ON THE LAST CALL
C         ( THIS PREVENTS ANY POINTS FROM BEING REJECTED)
C
C ***********************************************************
C
      DIMENSION X(NDIM,2),Y(NDIM),INDXP(21),INDXN(21)
      DOUBLE PRECISION D(30)
      CHARACTER CONT*1,BUFFER*80
C
2000  FORMAT(' ')
201   FORMAT(10X,F9.4,4X,2F8.3,F12.3)
2020  FORMAT('0',24X,'THE AVERAGE RESIDUAL IS ',E15.8)
2021  FORMAT(6X,'THE SUM OF THE SQUARES OF ALL RESIDUALS IS ',E15.8)
2022  FORMAT(23X,'THE STANDARD DEVIATION IS ',E15.8)
2023  FORMAT(22X,'THE MEASURE OF SKEWNESS IS ',E15.8)
2024  FORMAT(6X,'OF ALL THE RESIDUALS,',I5,' WERE NEGATIVE,'
     1 ,' AND ',I5,' WERE POSITIVE.')
2025  FORMAT(10X,I6,' RESIDUALS HAVE BEEN REJECTED.')
2030  FORMAT(14X,' HISTOGRAM OF THE RESIDUALS BEFORE REJECTION.')
2031  FORMAT(16X,'NUMBER OF',6X,'ABSOLUTE RANGE',5X,'NUMBER OF')
2032  FORMAT(12X,'NEGATIVE RESIDUALS  OF RESIDUALS  POSITIVE RESIDUALS')
204   FORMAT(' ',I21,F12.3,' < R < ',F5.3,I11)
205   FORMAT(' ',I21,F12.1,' AND GREATER ',I10)
206   FORMAT(12X,'RESIDUAL',7X,'X',7X,'Y',12X,'D')
C
      N=NN
      T=TT
      AVR=0.
      DO 9 M=1,21
      INDXN(M)=0
9     INDXP(M)=0
      U=0.
      V=0.
      NPOS=0
      NNEG=0
      M=1
      DO 23 I=1,N
      X(M,1)=X(I,1)
      X(M,2)=X(I,2)
      Y(M)=Y(I)
      XG=X(I,1)
      YG=X(I,2)
      S=POLY(XG,YG,D)
C
C     R=RESIDUAL FROM POLYNOMIAL FIT
C
      R=Y(I)-S
      AVR=AVR+R
      IF (R)11,15,12
11    NNEG=NNEG+1
      S=-R
      GO TO 13
12    NPOS=NPOS+1
      S=R
C
C     DO NOT INCREMENT COUNTER M IF RESIDUAL IS GREATER THAN T LIMIT
C
13    IF (S-T)15,15,14
14    IF (J)15,15,16
15    M=M+1
16    U=U+R*R
      V=V+R*R*R
      IF (R)17,22,20
17    IF (R+.2)18,19,19
18    INDXN(21)=INDXN(21)+1
      GO TO 23
19    JJ=1.-100.*R
      INDXN(JJ)=INDXN(JJ)+1
      GO TO  23
20    IF (R-0.2)22,22,21
21    INDXP(21)=INDXP(21)+1
      GO TO 23
22    JJ=1.+100.*R
      INDXP(JJ)=INDXP(JJ)+1
23    CONTINUE
      M=M-1
C
      END
      FUNCTION POLY(Z1,Z2,D)
C
C **********************************************************************
C
C     FUNCTION POLY(X,Y,D)
C
C     EVALUATES THE POLYNOMIAL DEFINED BY THE COEFFICIENTS D, AT
C     THE NORMALIZED COORDINATE (X,Y).
C
C     X = NORMALIZED X COORDINATE  (R*4)
C     Y = NORMALIZED Y COORDINATE  (R*4)
C     D(30) = ARRAY CONTAINING THE COEFFICIENTS OF THE POLYNOMIAL (R*8)
C
C     WRITTEN BY W D PENCE, NOV. 1980
C
C **********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H),(O-Y)
      DIMENSION D(30)
      DATA YLAST/-2./
      SAVE YLAST
C
C     FIRST, CALC 1-D POLYNOMIAL IN X FOR GIVEN Y VALUE, IF
C     NOT ALREADY DONE
C
      X=Z1
      Y1=Z2
C
      IF (Y1 .NE. YLAST)THEN
      Y2=Y1*Y1
      Y3=Y2*Y1
      Y4=Y3*Y1
      Y5=Y4*Y1
      Y6=Y5*Y1
C
      C0=D(1)+D(3)*Y1+D(6)*Y2+D(10)*Y3+D(15)*Y4+D(21)*Y5+D(28)*Y6
      C1=     D(2)   +D(5)*Y1+D(9) *Y2+D(14)*Y3+D(20)*Y4+D(27)*Y5
      C2=             D(4)   +D(8) *Y1+D(13)*Y2+D(19)*Y3+D(26)*Y4
      C3=                     D(7)    +D(12)*Y1+D(18)*Y2+D(25)*Y3
      C4=                              D(11)   +D(17)*Y1+D(24)*Y2
      C5=                                       D(16)   +D(23)*Y1
      C6=D(22)+D(30)*Y1
      C7=D(29)
      END IF
C
C     EVALUATE POLYNOMIAL IN X
C
      POLY=((((((C7*X+C6)*X+C5)*X+C4)*X+C3)*X+C2)*X+C1)*X+C0
C
      YLAST=Y1
      RETURN
      END
