C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R SOLSYM *
C      *            *
C      **************
C
C
C
C   PURPOSE
C This s/r solves a symmetric set of simultaneous equations
C   of the form  A.X = B
C   For a full description see AJPENNY's PDS software user's guide
C   The method is back interpolation
C
C   ARGUMENTS
C   IN
C      A    Real(NEQ,NEQ)    The input lh condition
C      NEQ  Integer          The size of A and B
C      N    Integer          The no of X parameters to solve for
C   IN/OUT
C      B    Real(NEQ)        On input, the rh conditions.  On exit, the
C                               solutions of X
C
C
C   CALLS
C     Grasp
C       PRODCR
C
C   A.J.PENNY                   RGO                    83-2-10
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE SOLSYM(A,B,NEQ,N)
C
      REAL A(NEQ,NEQ),B(NEQ)
C
      DO K = 1,N
C
         IF (K.NE.1) THEN
            DO J = K,N
               CALL PRODCR(A,NEQ,NEQ,J,A,NEQ,NEQ,K,1,K-1,S)
               A(J,K) = A(J,K) - S
            ENDDO
         ENDIF
C
         IF (K.NE.N) THEN
            AK = A(K,K)
            IF(ABS(AK).LT.1.0E-20)AK=SIGN(1.0E-20,AK)
            DO J = K+1,N
               A(K,J) = A(J,K)
               A(J,K) = A(J,K)/AK
            ENDDO
         ENDIF
C
      ENDDO
C
C
C
      DO K = 2,N
         S = 0.0
         DO J = 1,K-1
            S = S + A(K,J)*B(J)
         ENDDO
         B(K) = B(K) - S
      ENDDO
C
C
C
      IF (ABS(A(N,N)).LT.1.0E-20) A(N,N) = SIGN(1.0E-20,A(N,N))
      B(N) = B(N)/A(N,N)
      IF (N.NE.1) THEN
         IF (ABS(A(N-1,N-1)).LT.1.0E-20) A(N-1,N-1) =
     +                               SIGN(1.0E-20,A(N-1,N-1))
         B(N-1) = (B(N-1)-A(N-1,N)*B(N))/A(N-1,N-1)
         IF (N.GE.2) THEN
            DO J = 2,N-1
               S = 0.0
               DO K = 1,J
                  S = S + A(N-J,N-J+K)*B(N-J+K)
               ENDDO
               IF (ABS(A(N-J,N-J)).LT.1.0E-20) A(N-J,N-J) = 
     +                                    SIGN(1.0E-20,A(N-J,N-J))
               B(N-J) = (B(N-J)-S)/A(N-J,N-J)
            ENDDO
         ENDIF
      ENDIF
C
C
      END



