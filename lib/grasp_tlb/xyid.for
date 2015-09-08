C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYID   *
C      *            *
C      **************
C
C
C   PURPOSE
C      Extracts the Identifier from the desired record in an XY list.
C
C   ARGUMENTS
C  IN
C    DATA   Real(NITEM,LSTLEN)    The XY list.
C    NITEM  Int                   No of parameters in list + 5
C    LSTLEN Int                   No of records
C    L      Int                   Record to look at
C  OUT
C    NAME   Character*20           The wanted Identifier
C
C
C
C   CALLS
C     None
C
C   USES
C     Byte arrays
C
C
C   A.J.PENNY                   RGO                    83-3-6
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE XYID(DATA,NITEM,LSTLEN,L,NAME)
C
C
C
      CHARACTER*20 NAME
      BYTE DATA(NITEM*4,LSTLEN)
C
C
C
      DO K = 1,20
         NAME(K:K) = CHAR(DATA(K,L))
      ENDDO
C
C
C
      END



