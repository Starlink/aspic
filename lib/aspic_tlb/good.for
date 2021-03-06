      SUBROUTINE GOOD(M,N1,F,W,IW,N,B,G,D,VAR)
      IMPLICIT REAL*8 (A-H,O-Z)
C GOODNESS OF FIT ANALYSIS
      DIMENSION W(IW),B(N1,N1),G(N1,N1),D(N),VAR(N)
      DO 1 I=1,N1
      DO 3 K=I,N1
3     G(I,K)=0.0D0
      DO 1 K=1,N
      IF(I.GT.N) GO TO 1
      G(I,K)=W((K-1)*N+I)
    1 CONTINUE
      IFAIL=0
C NAG INVERSION ROUTINE
      CALL F01ABF(G,N1,N,B,N1,D,IFAIL)
      IF(IFAIL.NE.0)THEN 
      WRITE(6,203) IFAIL
      WRITE(7,203)IFAIL
      ELSE
      WRITE(6,207)
      WRITE(7,207)
  207 FORMAT(1H0,'COMPUTED TRANSFORMATION IS O.K.')
      ENDIF
      FQZ = DFLOAT(M-N)
      FACTOR = F/FQZ
      DO 2 I=1,N
      VAR(I)=B(I,I)*FACTOR
    2 WRITE(7,204) I,VAR(I)
  203 FORMAT(1H0,'ERROR NUMBER OF INVERSE IS',I4)
  204 FORMAT(/,2X,'VAR(',I2,') = ',E16.8)
      RETURN
      END
