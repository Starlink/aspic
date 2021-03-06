C
      SUBROUTINE FFTV1(IN,WK,VT,C,B,MB,M1,N1,K2,LL,IFT)
      INTEGER IN(1),WK(1),VT(1)
      COMPLEX C(1),B(1)
C  IN = FLAG OR ADDRESS OF INPUT STREAM
C  OUT= FLAG OR ADDRESS OF OUTPUT STREAM
C  VT = FLAG OR ADDRESS OF VIRTUAL ARRAY
C  C = ADDRESS OF CORE ARRAY
C  B = ADDRESS OF BUFFER ARRAY FOR CORE TRANSPOSE
C  MB= SIZE OF BUFFER
C  ALL SIZES ARE OF COMPLEX NUMBERS
C  IFT = (1=FOURIER,-1=SCALED INVERSE,-2=TRANSPOSE)
C
C  SETS WK(I3 , I4 , I1 , I5 , I2) = IN(I1,  I2 , I3 , I4 , I5)
C          K2   LL   M1   N1   N1       M1   N1   K2   LL   N1
C
C  IN(I1,I2,I3,I4,I5)
C   --- FTURD -------------------> C(I1,I2,I3)
C   --- FOURIER(XFWD OR YBAK) ---> C(I1,I2,I3)
C   --- TPOSE -------------------> C(I1,I3,I2)
C   --- FTVWR -------------------> VT(I1,I3,I2,I4)
C   --- FTVRD -------------------> C(I1,I3,I4)
C   --- TPOSE -------------------> C(I3,I4,I1)
C   --- FTUWR -------------------> WK(I3,I4,I1,I5,I2)
C
      M2 = K2*LL
      MM = M1*M2
      NX = M1*N1
      MC = NX*K2
      M12 = M1*K2
      DO 4 I5=1,N1
      DO 1 I4=1,LL
      CALL FTURD(IN,MC,I4+(I5-1)*LL,C(1))
C  ..FOURIER BEGIN..
      IF(IFT)15,19,10
 10   CALL FTRFWD(C(1),NX,K2/2,B(1),MB,0)
      GOTO 19
 15   FACT = -1./FLOAT(IFT)
      IF(I4+I5.EQ.2) CALL FTRBAK(C(1),NX/2,1,B(1),MB,1,FACT)
      CALL FTCMPX(C(1),NX,K2,-1)
 19   CONTINUE
C  ..FOURIER END..
      CALL FTCT(C(1),M1,N1,1,K2,1,B(1),MB)
      CALL FTVWR(VT,MC,I4,C(1))
 1    CONTINUE
      DO 3 I2=1,N1
      DO 2 I4=1,LL
      CALL FTVRD(VT,M12,I2+(I4-1)*N1,C(1+(I4-1)*M12))
 2    CONTINUE
      CALL FTCT(C(1),1,M1,1,M2,1,B(1),MB)
      CALL FTUWR(WK,MM,I5+(I2-1)*N1,C(1))
 3    CONTINUE
 4    CONTINUE
      RETURN
      END
      SUBROUTINE FFTV2(OUT,WK,VT,C,B,MB,M2,N2,K1,LL,IFT)
      INTEGER OUT(1),WK(1),VT(1)
      COMPLEX C(1),B(1)
C  OUT= FLAG OR ADDRESS OF OUTPUT STREAM
C  WK = FLAG OR ADDRESS OF INPUT STREAM
C  VT = FLAG OR ADDRESS OF VIRTUAL ARRAY
C  C = ADDRESS OF CORE ARRAY
C  B = ADDRESS OF BUFFER ARRAY FOR CORE TRANSPOSE
C  MB= SIZE OF BUFFER
C  ALL SIZES ARE OF COMPLEX NUMBERS
C  IFT = (1=FOURIER,-1=SCALED INVERSE,-2=TRANSPOSE)
C
C  SETS OUT(I1 , I2 , I3 , I4 , I5) = WK(I1 , I3 , I4 , I2 , I5)
C           M2   N2   K1   LL   N2       M2   K1   LL   N2   N2
C
C   WK(I1,I3,I4,I2,I5)
C    --- FTURD -------------------> C(I1,I3,I4)
C    --- FTVWR -------------------> VT(I1,I3,I4,I2)
C    --- FTVRD -------------------> C(I1,I3,I2)
C    --- TPOSE -------------------> C(I1,I2,I3)
C    --- FOURIER(YFWD OR XBAK) ---> C(I1,I2,I3)
C    --- FTUWR -------------------> OUT(I1,I2,I3,I4,I5)
C
      M1=K1*LL
      MM=M1*M2
      NY=M2*N2
      MC=NY*K1
      M12=M2*K1
      DO 4 I5=1,N2
      DO 1 I2=1,N2
      CALL FTURD(WK,MM,I2+(I5-1)*N2,C(1))
      CALL FTVWR(VT,MM,I2,C(1))
 1    CONTINUE
      DO 3 I4=1,LL
      DO 2 I2=1,N2
      CALL FTVRD(VT,M12,I4+(I2-1)*LL,C(1+(I2-1)*M12))
 2    CONTINUE
      CALL FTCT(C(1),M2,K1,1,N2,1,B(1),MB)
C  ..FOURIER BEGIN..
      IF(IFT)15,19,10
 10   CALL FTCMPX(C(1),NY,K1,1)
      IF(I4+I5.EQ.2) CALL FTRFWD(C(1),NY/2,1,B(1),MB,1)
      GOTO 19
 15   FACT = -1./FLOAT(IFT)
      CALL FTRBAK(C(1),NY,K1/2,B(1),MB,0,FACT)
 19   CONTINUE
C  ..FOURIER END..
      CALL FTUWR(OUT,MC,I4+(I5-1)*LL,C(1))
 3    CONTINUE
 4    CONTINUE
      RETURN
      END
      SUBROUTINE FTCMPX(F,NCURR,NREM,IFT)
      COMPLEX F(1)
      CALL FTCMPY(F,NCURR,NREM,IFT)
      RETURN
      END
      SUBROUTINE FTCMPY(F,NCURR,NREM,IFT)
      COMPLEX F(1)
C  DO NREM CONSECUTIVE COMPLEX FOURIER TRANSFORMS EACH OF SIZE
C  NCURR COMPLEXES. IFT=(1=FORWARDS,-1=BACKWARDS)
      CALL FTCX1(F(1),1,NCURR,NREM)
      CALL FTCX2(F(1),1,NCURR,NREM,IFT)
      RETURN
      END
      SUBROUTINE FTCX1(DATA,NPREV,N,NREM)
C SFG ROUTINE BOTRV
      DIMENSION DATA(1)
      COMPLEX DATA,TEMP
      IP0 = 1
      IP1 = IP0*NPREV
      IP4 = IP1*N
      IP5 = IP4*NREM
      I4REV = 1
      I4MAX = IP4
      DO 60 I4=1,I4MAX,IP1
      IF(I4-I4REV) 10,30,30
 10   I1MAX = I4+IP1-IP0
      DO 20 I1=I4,I1MAX,IP0
      DO 20 I5=I1,IP5,IP4
      I5REV = I4REV+I5-I4
      TEMP = DATA(I5)
      DATA(I5) = DATA(I5REV)
 20   DATA(I5REV) = TEMP
 30   IP2 = IP4/2
 40   IF(I4REV-IP2) 60,60,50
 50   I4REV = I4REV-IP2
      IP2 = IP2/2
      IF(IP2-IP1) 60,40,40
 60   I4REV = I4REV+IP2
      RETURN
      END
      SUBROUTINE FTCX2(DATA,NPREV,N,NREM,ISIGN)
C SFG ROUTINE COOL2
      DIMENSION DATA(1)
      COMPLEX DATA,TEMP,WSTP,W,W2,W3,T0,T1,T2,T3,CMPLX
      TWOPI = 6.28318530717958647692*FLOAT(ISIGN)
      IP0 = 1
      IP1 = IP0*NPREV
      IP4 = IP1*N
      IP5 = IP4*NREM
      IP2 = IP1
      NPART = N
 10   IF(NPART-2) 50,30,20
 20   NPART = NPART/4
      GOTO 10
 30   IP3 = IP2*2
      I1MAX = IP1
      DO 40 I1=1,I1MAX,IP0
      DO 40 I5=I1,IP5,IP3
      I3A = I5
      I3B = I3A+IP2
      TEMP = DATA(I3B)
      DATA(I3B) = DATA(I3A)-TEMP
 40   DATA(I3A) = DATA(I3A)+TEMP
      GOTO 140
 50   IP3 = IP2*4
      THETA = TWOPI/FLOAT(IP3/IP1)
      SINTH = SIN(THETA/2.)
      WSTP = CMPLX(-2.*SINTH*SINTH,SIN(THETA))
      W = 1.
      DO 130 I2=1,IP2,IP1
      IF(I2-1) 70,70,60
 60   W2 = W*W
      W3 = W2*W
 70   I1MAX = I2+IP1-IP0
      DO 120 I1=I2,I1MAX,IP0
      DO 120 I5=I1,IP5,IP3
      I3A = I5
      I3B = I3A+IP2
      I3C = I3B+IP2
      I3D = I3C+IP2
      IF( I2-1) 90,90,80
 80   DATA(I3B) = W2*DATA(I3B)
      DATA(I3C) = W*DATA(I3C)
      DATA(I3D) = W3*DATA(I3D)
 90   T0 = DATA(I3A)+DATA(I3B)
      T1 = DATA(I3A)-DATA(I3B)
      T2 = DATA(I3C)+DATA(I3D)
      T3 = DATA(I3C)-DATA(I3D)
      DATA(I3A) = T0+T2
      DATA(I3C) = T0-T2
      TEMP = CMPLX(-AIMAG(T3),REAL(T3))
      IF(ISIGN) 100,100,110
 100  TEMP = -TEMP
 110  DATA(I3B) = T1+TEMP
 120  DATA(I3D) = T1-TEMP
 130  W = W*WSTP+W
 140  IP2 = IP3
      IF(IP3-IP4) 50,150,150
 150  RETURN
      END
      SUBROUTINE FTCT(X,M1,M2,M3,M4,M5,B,MAXB)
C  SETS XNEW(I1,I4,I3,I2,I5)=XOLD(I1,I2,I3,I4,I5)
C  FOR I1 = 1 TO M1, I2 = 1 TO M2, ... ,I5 = 1 TO M5
C  WITH M1,M2,M3,M4,M5 POWERS OF 2 (INCLUDING 2**0=1)
C
C  BUFFER B IS OF SIZE MAXB COMPLEXES (2*MAXB REALS).
C
C  IF MAXB IS POSITIVE ON ENTRY, IT IS UNCHANGED, BUT
C  IF MAXB IS SET TO ZERO ON ENTRY, IT WILL RETURN WITH
C  MINUS THE SIZE NEEDED (IN COMPLEXES) FOR OPTIMAL EFFICIENCY.
C  THE ROUTINE WILL, HOWEVER, WORK SATISFACTORILY WITH
C  MUCH SMALLER SIZES, INCLUDING 0.
      COMPLEX X(1),B(1)
      N1 = M1
      N2 = M2
      N3 = M3
      N4 = M4
      N5 = M5
      IF(M2.EQ.1)N2=N3
      IF(M2.EQ.1)N3=1
      IF(M4.EQ.1)N4=N3
      IF(M4.EQ.1)N3=1
 1    IF (N2-N4) 10,20,30
C  N2.LT.N4
 10   IF(N2*N3.EQ.1) RETURN
      I3 = N3*(N4/N2)
      CALL FTCT1(X,N1,N2,I3,N2,N5)
      N5 = N5*N2
      N4 = N4/N2
      N2 = N2*N3
      N3 = 1
      IF(MAXB.LE.0) MAXB=MIN0(MAXB,-N2*N4)
      IF(N2*N4.GT.MAXB) GOTO 1
      CALL FTCT2(X,N1,N2,N4,N5,B)
      RETURN
C  N2.EQ.N4
 20   CALL FTCT1(X,N1,N2,N3,N4,N5)
      RETURN
C  N2.GT.N4
 30   IF(N3*N4.EQ.1) RETURN
      I3 = N3*(N2/N4)
      CALL FTCT1(X,N1,N4,I3,N4,N5)
      N1 = N1*N4
      N2 = N2/N4
      N4 = N3*N4
      N3 = 1
      IF(MAXB.LE.0) MAXB=MIN0(MAXB,-N2*N4)
      IF(N2*N4.GT.MAXB) GOTO 1
      CALL FTCT2(X,N1,N2,N4,N5,B)
      RETURN
      END
      SUBROUTINE FTCT2(X,N1,N2,N4,N5,B)
C  SETS XNEW(I1,I4,I2,I5)=XOLD(I1,I2,I4,I5)
C  WITH N2*N4 FITTING INTO BUFFER B
      COMPLEX X(1),B(1)
      N24=N2*N4
      N124=N1*N24
      N1245=N124*N5
      DO 2 I=1,N1
      DO 2 J=I,N1245,N124
      K=J-N1
      DO 1 L=1,N4
      DO 1 M=L,N24,N4
      K=K+N1
 1    B(M)=X(K)
      K=J-N1
      DO 2 M=1,N24
      K=K+N1
 2    X(K)=B(M)
      RETURN
      END
      SUBROUTINE FTCT1(X,N1,N2,N3,N4,N5)
C  SETS XNEW(I1,I4,I3,I2,I5)=XOLD(I1,I2,I3,I4,I5)
C  WITH N2=N4
      COMPLEX X(1),A
      IF(N2.EQ.0) RETURN
      N12 = N1*N2
      N123 = N12*N3
      N1234 = N123*N4
      N12345 = N1234*N5
      DO 5 I=1,N1
      DO 4 K=I,N123,N12
      DO 3 M=K,N12345,N1234
      J1 = M+N1
      J2 = M+N123
      DO 2 J=2,N2
      L1 = J1
      L2 = J2
      DO 1 L=2,J
      A = X(L1)
      X(L1) = X(L2)
      X(L2) = A
      L1 = L1+N123
      L2 = L2+N1
 1    CONTINUE
      J1 = J1+N1
      J2 = J2+N123
 2    CONTINUE
 3    CONTINUE
 4    CONTINUE
 5    CONTINUE
      RETURN
      END
      SUBROUTINE FFTV(IN,OUT,WK,VT,C,B,N,M,IFT)
C
C  2-D  REAL  :
C   RECTANGULAR TRANSPOSE PACKED CONJUGATE SYMMETRIC POSITIVE FOURIER
C
C  FOURIER(IMAGE(JX,JY)) = SUM IMAGE(JX,JY)*EXP(2*PI*I*PHASE)
C   WHERE PHASE = (JX-1)*(KX-1)/N(1)+(JY-1)*(KY-1)/N(2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C          WRITTEN BY JOHN SKILLING                                    C
C                                                                      C
C          COPYRIGHT (C)                                               C
C          MAXIMUM ENTROPY DATA CONSULTANTS LTD.   JANUARY 1981        C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  IN = FLAG OR ADDRESS OF INPUT STREAM
C  OUT= FLAG OR ADDRESS OF OUTPUT STREAM
C  WK = FLAG OR ADDRESS OF WORKING STREAM
C    WK MAY NOT OVERWRITE IN
C    OUT MAY OVERWRITE WK OR IN
C  IN,OUT,WK ARE ACCESSED VIA SUBROUTINES FTURD AND FTUWR.
C  VT = FLAG OR ADDRESS OF COMPLEX VIRTUAL ARRAY, SIZE M(1) REALS
C    VT IS ACCESSED VIA SUBROUTINES FTVRD AND FTVWR
C  C = ADDRESS OF COMPLEX CORE ARRAY, SIZE M(2) REALS
C  B = ADDRESS OF COMPLEX CORE ARRAY, SIZE M(3) REALS, USED AS A
C    BUFFER FOR CORE TRANSPOSING.
C  N = INTEGER 2-VECTOR CONTAINING DIMENSIONS (XDIM,YDIM) OF REALS
C  INPUT SIZES ARE OF REAL NUMBERS,AND ARE POWERS OF 2.
C  IFT=(1=FOURIER,0=OFF,-1=SCALED INVERSE,-2=TRANSPOSE)
C
C FTURD(S,M,K,C) READS K'TH BLOCK OF M COMPLEXES FROM STREAM S TO C
C FTUWR(S,M,K,C) WRITES K'TH BLOCK OF M COMPLEXES FROM C TO STREAM S
C FTVRD(V,M,K,C) READS K'TH BLOCK OF M COMPLEXES FROM VIRTUAL TO C
C FTVWR(V,M,K,C) WRITES K'TH BLOCK OF M COMPLEXES FROM C TO VIRTUAL
C IN THESE I/O ROUTINES, S IS A FLAG OR ADDRESS IN,OUT,WK
C V IS THE FLAG OR ADDRESS VT, C IS A COMPLEX ADDRESS IN CORE
C AND THE BLOCKS OF M COMPLEXES ARE CONSECUTIVE STARTING AT K=1
      INTEGER IN(1),OUT(1),WK(1),VT(1),N(2),M(3)
      COMPLEX C(1),B(1)
      IF( N(1).LE.1 .OR. N(2).LE.1 ) STOP
      NX = N(1)/2
      NY = N(2)
      MV = N(1)*MIN0(M(1)/N(1),N(2))
 1    MV = MV/2
      IF( MV.LT.NY ) STOP
      M1 = MV/NY
      NN = NX/M1
      M2 = MV/NX
      MM = M1*M2
      MC = MIN0(MV,M(2)/2)
      IF(MC.LT.MM) GOTO 1
      IF(MC.LT.N(1).OR.MC.LT.N(2)) STOP
      LL = MV/MC
      K1 = M1/LL
      K2 = M2/LL
      MB = M(3)/2
      IF(M(3).LT.0) GOTO 3
C  PROGRAM HAS USED THE LARGEST MAXV <= M(1) , MAXC <= M(2) OBEYING
C    MOSAIC <= MAXC <= MAXV
C    WHERE MOSAIC = MAXV*MAXV/NTOT , NTOT = N(1)*N(2) .
C  THE USER MUST ENSURE THAT THE RESULTING MAXC OBEYS
C    MAXC >= 2*MAX(N(1),N(2)) .
C  THE MOSAIC SIZE FOR I/O TO IN,OUT,WK IS
C    MOSAIC=MAXV*MAXV/NTOT REALS, WHICH OUGHT TO BE AS LARGE
C    AS POSSIBLE, AND NOT LESS THAN THE RELEVANT BLOCKSIZES.
C  THE VIRTUAL I/O IS IN MULTIPLES OF  MAXC*MAXV/NTOT.
      IF(IFT.GT.0) GOTO 2
      I=M1
      M1=M2
      M2=I
      I=K1
      K1=K2
      K2=I
 2    CONTINUE
      CALL FFTV1( IN,WK,VT,C,B,MB,M1,NN,K2,LL,IFT)
      CALL FFTV2(OUT,WK,VT,C,B,MB,M2,NN,K1,LL,IFT)
      IF(MB.GT.0) RETURN
C  INSTALLATION SWITCH
C  IF MB=0 ON ENTRY, RETURN WITH
C  M(1) = -MAXV*MAXV/NTOT = -MOSAIC SIZE (REALS) FOR I/O TO IN,OUT,WK
C  M(2) = -MAXC*MAXV/NTOT = -PAGE SIZE (REALS) FOR I/O TO VT
C  M(3) = -IDEAL SIZE OF B (REALS) FOR OPTIMAL EFFICIENCY.
C  THE ROUTINE WILL STILL RUN UNDER THIS OPTION.
C  REAL ARITHMETIC AND ADDRESSING CAN BE SWITCHED OUT BY LINKING
C  NULL ROUTINES FTVRD,FTVWR,FTURD,FTUWR,FTCT1,FTCT2,
C  FTRT1,FTRT2,FTCX1, AND FTCX2.
 3    M(1) = -2*MM
      M(2) = -2*MM/LL
      M(3) = 2*MB
      RETURN
      END
      SUBROUTINE FTRBAK(F,NCURR,NREM,B,MB,IFLAG,FACT)
      COMPLEX F(1),B(1),X,CMPLX
C  DO 2*NREM CONSECUTIVE BACKWARD PACKED CONJUGATE SYMMETRIC TO REAL
C  TRANSFORMS EACH OF SIZE NC2=2*NCURR REALS
C  FACT=1.IS SCALED INVERSE  FACT=.5 IS TRANSPOSE
C  IFLAG=1 GIVES ALTERNATIVE EXIT
      NC2 = 2*NCURR
      CALL FTRT(F,1,2,NCURR,2,NREM,B,MB)
      IF(NCURR.LE.1) GOTO 6
      DO 5 J=2,NCURR
      J1 = J
      J2 = J+NCURR
      DO 5 K=1,NREM
      X = CMPLX(-AIMAG(F(J2)),REAL(F(J2)))
      F(J2) = FACT*(F(J1)-X)
      F(J1) = FACT*(F(J1)+X)
      J1 = J1+NC2
      J2 = J2+NC2
 5    CONTINUE
 6    IF(NCURR.LE.2) GOTO 4
      MM = NC2+NCURR+2
      L1 = NCURR+2
      L2 = NCURR+NCURR/2
      DO 3 J=L1,L2
      J1 =J
      J2 = MM-J
      DO 3 K=1,NREM
      X = F(J1)
      F(J1) = F(J2)
      F(J2) = X
      J1 = J1+NC2
      J2 = J2+NC2
 3    CONTINUE
 4    IF(IFLAG.EQ.1) RETURN
      CALL FTCMPX(F(1),NC2,NREM,-1)
      CALL FTRT(F,1,2,1,NC2,NREM,B,MB)
      RETURN
      END
      SUBROUTINE FTRFWD(F,NCURR,NREM,B,MB,IFLAG)
      COMPLEX F(1),B(1),X,CMPLX
C  DO 2*NREM CONSECUTIVE FORWARD REAL TO PACKED CONJUGATE
C        SYMMETRIC TRANSFORMS, EACH OF SIZE NC2=2*NCURR REALS
C  IFLAG =1 GIVES AN ALTERNATE ENTRY
      NC2 = 2*NCURR
      IF (IFLAG.EQ.1) GOTO 2
      CALL FTRT(F,1,NC2,1,2,NREM,B,MB)
      CALL FTCMPX(F(1),NC2,NREM,1)
 2    IF(NCURR.LE.2) GOTO 4
      MM=NC2+NCURR+2
      L1=NCURR+2
      L2=NCURR+NCURR/2
      DO 3 J=L1,L2
      J1=J
      J2=MM-J
      DO 3 K=1,NREM
      X=F(J1)
      F(J1)=F(J2)
      F(J2)=X
      J1=J1+NC2
      J2=J2+NC2
 3    CONTINUE
 4    IF(NCURR.LE.1) GOTO 6
      DO 5 J=2,NCURR
      J1=J
      J2=J+NCURR
      DO 5 K=1,NREM
      X = F(J2)-F(J1)
      F(J1) = .5*(F(J1)+F(J2))
      F(J2) = .5*CMPLX(-AIMAG(X),REAL(X))
      J1=J1+NC2
      J2=J2+NC2
 5    CONTINUE
 6    CALL FTRT(F,1,2,NCURR,2,NREM,B,MB)
      RETURN
      END
      SUBROUTINE FTRT(X,M1,M2,M3,M4,M5,B,MAXB)
C  SETS XNEW(I1,I4,I3,I2,I5)=XOLD(I1,I2,I3,I4,I5)
C  FOR I1 = 1 TO M1, I2 = 1 TO M2, ... ,I5 = 1 TO M5
C  WITH M1,M2,M3,M4,M5 POWERS OF 2 (INCLUDING 2**0=1)
C
C  BUFFER B IS OF SIZE MAXB COMPLEXES (2*MAXB REALS)
C
C  IF MAXB IS POSITIVE ON ENTRY, IT IS UNCHANGED, BUT
C  IF MAXB IS SET TO ZERO ON ENTRY, IT WILL RETURN WITH
C  MINUS THE SIZE NEEDED (IN COMPLEXES) FOR OPTIMAL EFFICIENCY.
C  THE ROUTINE WILL, HOWEVER, WORK SATISFACTORILY WITH
C  MUCH SMALLER SIZES, INCLUDING 0.
      REAL X(1),B(1)
      N1 = M1
      N2 = M2
      N3 = M3
      N4 = M4
      N5 = M5
      IF(M2.EQ.1)N2=N3
      IF(M2.EQ.1)N3=1
      IF(M4.EQ.1)N4=N3
      IF(M4.EQ.1)N3=1
 1    IF (N2-N4) 10,20,30
C  N2.LT.N4
 10   IF(N2*N3.EQ.1) RETURN
      I3 = N3*(N4/N2)
      CALL FTRT1(X,N1,N2,I3,N2,N5)
      N5 = N5*N2
      N4 = N4/N2
      N2 = N2*N3
      N3 = 1
      IF(MAXB.LE.0) MAXB=MIN0(MAXB,-N2*N4/2)
      IF(N2*N4.GT.MAXB*2) GOTO 1
      CALL FTRT2(X,N1,N2,N4,N5,B)
      RETURN
C  N2.EQ.N4
 20   CALL FTRT1(X,N1,N2,N3,N4,N5)
      RETURN
C  N2.GT.N4
 30   IF(N3*N4.EQ.1) RETURN
      I3 = N3*(N2/N4)
      CALL FTRT1(X,N1,N4,I3,N4,N5)
      N1 = N1*N4
      N2 = N2/N4
      N4 = N3*N4
      N3 = 1
      IF(MAXB.LE.0) MAXB=MIN0(MAXB,-N2*N4/2)
      IF(N2*N4.GT.MAXB*2) GOTO 1
      CALL FTRT2(X,N1,N2,N4,N5,B)
      RETURN
      END
      SUBROUTINE FTRT2(X,N1,N2,N4,N5,B)
C  SETS XNEW(I1,I4,I2,I5)=XOLD(I1,I2,I4,I5)
C  WITH N2*N4 FITTING INTO BUFFER B
      REAL X(1),B(1)
      N24=N2*N4
      N124=N1*N24
      N1245=N124*N5
      DO 2 I=1,N1
      DO 2 J=I,N1245,N124
      K=J-N1
      DO 1 L=1,N4
      DO 1 M=L,N24,N4
      K=K+N1
 1    B(M)=X(K)
      K=J-N1
      DO 2 M=1,N24
      K=K+N1
 2    X(K)=B(M)
      RETURN
      END
      SUBROUTINE FTRT1(X,N1,N2,N3,N4,N5)
C  SETS XNEW(I1,I4,I3,I2,I5)=XOLD(I1,I2,I3,I4,I5)
C  WITH N2=N4
      REAL X(1),A
      IF(N2.EQ.0) RETURN
      N12 = N1*N2
      N123 = N12*N3
      N1234 = N123*N4
      N12345 = N1234*N5
      DO 5 I=1,N1
      DO 4 K=I,N123,N12
      DO 3 M=K,N12345,N1234
      J1 = M+N1
      J2 = M+N123
      DO 2 J=2,N2
      L1 = J1
      L2 = J2
      DO 1 L=2,J
      A = X(L1)
      X(L1) = X(L2)
      X(L2) = A
      L1 = L1+N123
      L2 = L2+N1
 1    CONTINUE
      J1 = J1+N1
      J2 = J2+N123
 2    CONTINUE
 3    CONTINUE
 4    CONTINUE
 5    CONTINUE
      RETURN
      END
      SUBROUTINE FTVRD(V,M,K,C)
C READ THE K'TH BLOCK OF M COMPLEXES FROM V(I+(K-1)*M) TO C(I)
      COMPLEX C(1)
      COMPLEX V(1)
      J=(K-1)*M
      DO 1 I=1,M
      J=J+1
 1    C(I)=V(J)
      RETURN
      END
      SUBROUTINE FTVWR(V,M,K,C)
C WRITE THE K'TH BLOCK OF M COMPLEXES FROM C(I) TO V(I+(K-1)*M)
      COMPLEX C(1)
      COMPLEX V(1)
      J=(K-1)*M
      DO 1 I=1,M
      J=J+1
 1    V(J)=C(I)
      RETURN
      END
      SUBROUTINE FTURD(S,M,K,C)
C READ THE K'TH BLOCK OF M COMPLEXES FROM S(I+(K-1)*M) TO C(I)
      COMPLEX C(1)
      COMPLEX S(1)
      J=(K-1)*M
      DO 1 I=1,M
      J=J+1
 1    C(I)=S(J)
      RETURN
      END
      SUBROUTINE FTUWR(S,M,K,C)
C WRITE THE K'TH BLOCK OF M COMPLEXES FROM C(I) TO S(I+(K-1)*M)
      COMPLEX C(1)
      COMPLEX S(1)
      J=(K-1)*M
      DO 1 I=1,M
      J=J+1
 1    S(J)=C(I)
      RETURN
      END
