C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PRODRR *
C      *            *
C      **************
C
C
C
C   PURPOSE
C    This multiplies the rows of two arrays together.
C    It takes any row in each of the arrays and multiplies and sums
C    the elements of the same section in those rows.
C
C   ARGUMENTS
C   IN
C      A(MA,NA)  Real     1st array
C      MA        Integer  No of entries in a row in A
C      NA        Integer  No of entries in a column in A
C      JA        Integer  Row in A to multiply
C      B(MB,NB)  Real     1st array
C      MB        Integer  No of entries in a row in B
C      NB        Integer  No of entries in a column in B
C      JB        Integer  Row in B to multiply
C      NS        Integer  Start of setion of row
C      NE        Integer  End of section of row
C  OUT
C      S         Real     Output sum
C
C   CALLS
C     None
C   A.J.PENNY                   RGO                    83-2-19
C
C -----------------------------------------------------------------
C
C
      SUBROUTINE PRODRR(A,MA,NA,JA,B,MB,NB,JB,NS,NE,S)
C
C
C
      REAL A(MA,NA),B(MB,NB)
C
C
C
      S = 0.0
      DO K = NS,NE
         S = S + A(K,JA)*B(K,JB)
      ENDDO
C
C
C
      END




C
