C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GAUPX  *
C      *            *
C      **************
C
C
C
C   PURPOSE
C  This s/r sets up terms for use in making the simultaneous eqns
C  to solve the 2D Gaussian fit done in AGAUSS (qv).
C
C   ARGUMENTS
C
C    IN
C       M    Integer        No of points wanted
C       R    Real           Gaussian radius
C       XO   Real           Position of centre of Gaussian
C       NF   Integer        =2 for fixed radii,=3 for varaible
C   OUT
C       F    Real(70,4)     )
C       SF   Real(4,4)      ) the output values
C       RA   Real(3)        )
C
C
C   CALLS
C     Grasp
C       PRODRR
C
C
C   A.J.PENNY                   RGO                    83-2-9
C
C -----------------------------------------------------------------
C
C
C
C
      SUBROUTINE GAUPX(M,R,XO,F,SF,RA,NF)
C
C
C
      REAL F(70,4),SF(4,4),RA(3)
C
      RR = 1.0/R
      RA(1) = 1.0
      RA(2) = 2.0*RR**2
      RA(3) = 2.0*RR**3
C
      DO J = 1,M
         XJ = J
         Z = XJ - XO
         ZR = Z*RR
         W = 0.0
         IF(ABS(ZR).LT.6.0) W = EXP(-1.0*ZR*ZR)
         F(J,1) = W
         W = Z*W
         F(J,2) = W
         F(J,3) = Z*W
      ENDDO
C
      DO K = 1,NF
         DO L = K,NF
            CALL PRODRR(F,70,4,K,F,70,4,L,1,M,S)
            SF(K,L) = S*RA(K)*RA(L)
            SF(L,K) = SF(K,L)
         ENDDO
         S = 0.0
         DO J = 1,M
            S = S + F(J,K)
         ENDDO
         SF(4,K) = S*RA(K)
         SF(K,4) = S*RA(K)
      ENDDO
C
      SF(4,4) = M
C
      END



