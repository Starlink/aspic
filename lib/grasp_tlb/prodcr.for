C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PRODCR *
C      *            *
C      **************
C
C
C
C   PURPOSE
C    This multiplies a column in one array by a row in another.
C    It takes a row in an array and a column in another and multiplies
C    and sums the elements of the same section in the column and row.
C
C   ARGUMENTS
C   IN
C      A(MA,NA)  Real     1st array
C      MA        Integer  No of entries in a row in A
C      NA        Integer  No of entries in a column in A
C      JA        Integer  Column in A to multiply
C      B(MB,NB)  Real     1st array
C      MB        Integer  No of entries in a row in B
C      NB        Integer  No of entries in a column in B
C      JB        Integer  Row in B to multiply
C      NS        Integer  Start of setion of row and column
C      NE        Integer  End of section of row and column
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
      SUBROUTINE PRODCR(A,MA,NA,JA,B,MB,NB,JB,NS,NE,S)
C
C
C
      REAL A(MA,NA),B(MB,NB)
C
C
C
      S = 0.0
      DO K = NS,NE
         S = S + A(JA,K)*B(K,JB)
      ENDDO
C
C
C
      END




C
