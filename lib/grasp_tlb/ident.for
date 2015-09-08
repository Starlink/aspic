C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R IDENT *
C      *           *
C      *************
C
C
C  PURPOSE
C      To load an EDRS format XY list array with identifiers
C      of #1 to #n.
C
C  METHOD
C      Load the identifiers one by one into IDBUF, and then
C      transfer them byte by byte into the correct place in
C      the XY list array.
C
C  ARGUMENTS
C    (IN/OUT)
C
C     DATA
C        The XY list array
C
C     (IN)
C
C     NITEM
C        The number of 4 byte words in a record of DATA. (i.e.
C            5+number of parameters)
C     LENOUT
C        The number of records in DATA
C
C
C  CALLS
C        EDRS package
C               LBGONE
C
C  NOTES
C        Uses byte arrays
C
C  WRITTEN BY
C      A.J.Penny                                        82-7-7
C --------------------------------------------------------------
C
C
C
      SUBROUTINE IDENT(DATA,NITEM,LENOUT)
C
C
C
      BYTE DATA(NITEM*4,LENOUT)
      CHARACTER IDBUF*20
C
C
C
      DO K = 1,LENOUT
         WRITE(IDBUF,'(I20)')K
         IDBUF(1:1) = '#'
         CALL LBGONE(IDBUF(2:))
         DO J = 1,20
            DATA(J,K) = ICHAR(IDBUF(J:J))
         ENDDO
      ENDDO
C
C
C
      END



