C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYGTID *
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
C    ISTAT  Int                   Flag for success (=0 if ok)
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
C   A.J.PENNY                   RGO                    84-2-6 19:52
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE XYGTID(DATA,NITEM,LSTLEN,L,NAME,ISTAT)
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
            NAME(K:K) = CHAR(DATA(K,L))
         ENDDO
         ISTAT = 0
      ELSE
         CALL WRUSER('XYlist access out of bounds',IERR)
         NAME = ' '
         ISTAT = 1
      ENDIF
C
C
C
      END



