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
C++
