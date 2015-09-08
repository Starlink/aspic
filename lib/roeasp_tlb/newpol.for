      SUBROUTINE NEWPOL(II,K,N,XSINGL,YSINGL,L,DEVSIN,A)                        
C+
C   NEWPOL
C
C     POLYNOMIAL FIT
C
C     POLYFIT DETERMINES BEST ORDER
C     ENDFIT TAKES ORDER AS GIVEN
C     MAXIMUM 250 POINTS
C
C   Given      (arguments)
C   II          0: POLYFIT, 1: ENDFIT
C   K           MAX ORDER
C   N           NO. POINTS
C   XSINGL      ARRAY
C   YSINGL      ARRAY
C
C   Returned   (arguments)
C   L           BEST ORDER
C   DEVSIN      RMS DEV.
C   A           COEFF ARRAY (21)
C
C   Subroutines called :
C   POLVAL             : E2DLIB
C
C     J.A.COOKE/UOE/1976
C-
C     see Ascher and Forsyth, in
C      Journal of the Association for Computing Machinery,
C      vol. 5, no. 1, p9, 1958.
C
      REAL XSINGL(N),YSINGL(N)
      REAL*8 X(250),Y(250),A(21),P(21,22),PV(21,250),T(21),OA(21)
      REAL*8 SX,SY,YPS,XPS,PS,AA,BB,STR,PMS,SUM,YY,RDEV
      DO 1 J=1,N
      X(J)=XSINGL(J)
      Y(J)=YSINGL(J)
    1 CONTINUE
C  ZERO COEFF ARRAY
      DO 5 I=1,21
    5 A(I)=0.
C  ZERO P,PV
      DO 10 J=1,22
      DO 10 I=1,21
   10 P(I,J)=0.
      DO 15 J=1,250
      DO 15 I=1,21
   15 PV(I,J)=0.
C  SET & CALCULATE P0,P1,PV0,PV1,T0,T1
      SX=0.
      SY=0.
      DO 20 I=1,N
      SY=SY+Y(I)
   20 SX=SX+X(I)
      T(1)=SY/N
      P(1,2)=1.
      P(2,2)=-(SX/N)
      P(2,3)=1.
      DO 25 I=1,N
      PV(1,I)=1.
   25 PV(2,I)=X(I)-(SX/N)
      YPS=0
      XPS=0
      PS=0
      DO 30 I=1,N
      YPS=YPS+Y(I)*PV(2,I)
      XPS=XPS+X(I)*PV(2,I)*PV(2,I)
   30 PS=PS+PV(2,I)*PV(2,I)
      AA=XPS/PS
      BB=PS/N
      T(2)=YPS/PS
      A(1)=T(1)*P(1,2)+T(2)*P(2,2)
      A(2)=T(2)*P(2,3)
      STR=10000.
C  ENTER LOOP, SEEKING P2 (CORRESP. P(L=3))
C  SET POLY ORDER = L
C
      DO 100 LOOP=2,K
      L=LOOP
      LIM=L+2
C  EQUATE COEFFS IN GEN FN
      DO 40 J=2,LIM
   40 P(L+1,J)=P(L,J-1)-AA*P(L,J)-BB*P(L-1,J)
C  GET POLY VALUE
      DO 50 M=1,N
   50 PV(L+1,M)=(X(M)-AA)*PV(L,M)-BB*PV(L-1,M)
C  GET NUM,DEN FOR AA,BB,T
      PMS=PS
      XPS=0
      PS=0
      YPS=0
      DO 60 I=1,N
      XPS=XPS+X(I)*PV(L+1,I)*PV(L+1,I)
      PS=PS+PV(L+1,I)*PV(L+1,I)
   60 YPS=YPS+Y(I)*PV(L+1,I)
C  GET AA,BB
      AA=XPS/PS
      BB=PS/PMS
C  GET FOURIER COEFFS
      T(L+1)=YPS/PS
C  ADD NEW TERMS TO COEFF ARRAY
      DO 70 I=1,LIM
C  REMEMBER OLD ARRAY
      OA(I)=A(I)
   70 A(I)=A(I)+T(L+1)*P(L+1,I+1)
C  GET SIGMASQD
      SUM=0.
      DO 80 I=1,N
      CALL POLVAL(L,A,X(I),YY)
   80 SUM=SUM+(Y(I)-YY)*(Y(I)-YY)
C  GET RMS DEV
      RDEV=SQRT(SUM/DFLOAT(N))
      IF(II.EQ.0) THEN
        IF(RDEV.GT.STR) THEN
          DEVSIN=STR
          DO 85 I=1,21
   85     A(I)=0.0
          DO 90 I=1,L
   90     A(I)=OA(I)
          L=L-1
          RETURN
        ELSE
          STR=RDEV
        ENDIF
      ENDIF
  100 CONTINUE
      DO 110 I=L+2,21
  110 A(I)=0.0
      DEVSIN=RDEV
      RETURN
      END
C
C
C****************************************************************
C****************************************************************
