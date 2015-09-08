C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R SIMULX *
C      *            *
C      **************
C
C
C   PURPOSE
C
C      The simultaneous equations
C
C           Y = A.X
C
C      are solved for X.
C
C      This is taken from the AAO routine SIMULT, except that the
C      arrays have been extended to allow for 32 unknowns and there
C      is no separate work space for the coefficent matrix.
C
C
C   ARGUMENTS
C    IN
C      Y(N)    Real     The array of function values
C      A(N,N)  Real     The array of coefficents
C      N       Integer  The size of vector arrays; matrices are N*N
C    OUT
C      X(N)    Real     The vectorix
C
C
C   CALLS
C     None
C
C
C      K F HARTLEY, A J PENNY  RGO    STAFF AAO            83-2-23
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE SIMULX(Y,A,X,N)
C
C
C
      REAL Y(N),A(N,N),X(N)
      INTEGER ROW(32),COL(32)
      INTEGER P,Q,I,J,IA,JA,IK,JK,KM1,NP2,N50,K,IM1
C
C      PREPARATION
C
      DO I=1,N
         ROW(I)=I
         COL(I)=I
         X(I)=Y(I)
      ENDDO
C
C      SUCCESSIVE REDUCTION FROM N EQUATIONS IN N UNKNOWNS
C      TO N-1 EQUATIONS IN N-1 UNKNOWNS
C
      NP2 = N+2
      IF (N.GE.2) THEN
C
C      ALREADY ONE EQUATION IN ONE UNKNOWN
C
         DO N50 = 2,N
            K = NP2-N50
C
C      REDUCE EQUATION SET FROM K EQUATIONS IN K UNKNOWNS
C      TO K-1 EQUATIONS IN K-1 UNKNOWNS
C
C      FIRST FIND LARGEST ELEMENT FOR PIVOT
C
            IK = ROW(K)
            JK = COL(K)
            AMX = A(IK,JK)
            P = K
            Q = K
            DO I=1,K
               IK = ROW(I)
               DO J=1,K
                  JK = COL(J)
                  IF (AMX.LT.A(IK,JK)) THEN
                     P = I
                     Q = J
                     AMX = A(IK,JK)
                  ENDIF
               ENDDO
            ENDDO
            IF(ABS(AMX).LT.1.0E-30) AMX = SIGN(1.0E-30,AMX)
C
C      NOW HAVE LARGEST ELEMENT A(P,Q)
C
C      "INTERCHANGE" ROWS AND COLUMNS
C
            IK = ROW(P)
            ROW(P)=ROW(K)
            ROW(K)=IK
            JK = COL(Q)
            COL(Q)=COL(K)
            COL(K)=JK
C
C      "NORMALISE" CURRENT EQUATIONS IN VARIABLE TO BE ELIMINATED
C
            DO I=1,K
               IA = ROW(I)
               X(IA) = X(IA)/AMX
               DO J=1,K
                  JA = COL(J)
                  A(IA,JA)=A(IA,JA)/AMX
               ENDDO
            ENDDO
            KM1 = K-1
C
C      ELIMINATE KTH UNKNOWN
C
            DO I=1,KM1
               IA = ROW(I)
               FACTOR = A(IA,JK)
               X(IA)=X(IA)-FACTOR*X(IK)
               DO J=1,KM1
                  JA = COL(J)
                  A(IA,JA)=A(IA,JA)-FACTOR*A(IK,JA)
               ENDDO
            ENDDO
C
         ENDDO
C
      ENDIF
C
C      FIND FIRST SOLUTION
C
      IA = ROW(1)
      JA = COL(1)
      IF (ABS(A(IA,JA)).LT.1.0E-30) A(IA,JA) = SIGN(1.0E-30,A(IA,JA))
      X(IA)=X(IA)/A(IA,JA)
C
C      SUBSTITUTE TO FIND OTHER SOLUTIONS
C
      IF (N.GE.2) THEN
         DO I=2,N
            IA = ROW(I)
            IM1=I-1
            DO J=1,IM1
               JA = COL(J)
               IK = ROW(J)
               X(IA)=X(IA)-A(IA,JA)*X(IK)
            ENDDO
         ENDDO
      ENDIF
C
C      PUT RESULTS IN CORRECT ORDER
C
      DO I=1,N
         A(I,1)=X(I)
      ENDDO
      DO I=1,N
         IA = ROW(I)
         JA = COL(I)
         X(JA)=A(IA,1)
      ENDDO
C
C
C
      END



