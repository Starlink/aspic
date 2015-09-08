C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYPTID *
C      *            *
C      **************
C
C
C   PURPOSE
C      Writes an Identifier on the desired record of an XYlist.
C
C   ARGUMENTS
C
C IN
C    NAME   Character*20           The Identifier
C OUT
C    DATA   Real(NITEM,LSTLEN)    The XY list.
C    NITEM  Int                   No of parameters in list + 5
C    LSTLEN Int                   No of records
C    L      Int                   Record to write on
C    ISTAT  Int                   Success flag (=0 if ok)
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
C   A.J.PENNY                   RGO                    84-2-6 19:56
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE XYPTID(NAME,DATA,NITEM,LSTLEN,L,ISTAT)
C
C
C
      CHARACTER*20 NAME
      BYTE DATA(NITEM*4,LSTLEN)
C
C
C
      IF (NITEM.GE.5.AND.L.GE.1.AND.L.LE.LSTLEN) THEN
         DO K = 1,20
            DATA(K,L) = ICHAR(NAME(K:K))
         ENDDO
      ELSE
         CALL WRUSER('XYlist access out of bounds',IERR)
         ISTAT = 1
      ENDIF
C
C
C
      END



