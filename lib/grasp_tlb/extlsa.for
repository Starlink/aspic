C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     **************
C     *            *
C     * S/R EXTLSA *
C     *            *
C     **************
C
C
C  PURPOSE
C      To copy parts of an array to another array. The copying is
C      done in bytes. This is suitable for the EDRS XYlist format.
C        Parts of records from the input array, starting at a given 
C      record number and ending at another are copied to another
C      record section and position in record in the output array
C
C  METHOD
C      Copy by bytes
C
C  ARGUMENTS
C     (IN)
C
C     LIST
C        The input data
C     LITEM
C        The number of 4 byte words in a record in LIST
C     LREC
C        The number of records in LIST
C     LSTART
C        The record in the LIST to start copying from
C     LFIN
C        The record in the LIST to finish copying from
C     LBEG
C        The first 4byte word in a record in LIST to be transfered
C     LEND
C        The last 4byte word in a record in LIST to be transfered
C
C      (IN/OUT)
C
C     DATA
C        The output array
C     NITEM
C        The number of 4 byte words in a record in DATA
C     NREC
C        The number of records in DATA
C
C     NSTART
C        The first record in DATA to be written to
C     NBEG
C        The first 4byte word in a record in DATA to written to
C
C  CALLS
C     None
C
C  NOTES
C     Uses byte arrays
C
C  WRITTEN BY
C     A.J.Penny
C ------------------------------------------------------------
C
C
C
      SUBROUTINE EXTLSA(LIST,LITEM,LREC,LSTART,LFIN,LBEG,LEND,
     +                  DATA,NITEM,NREC,NSTART,NBEG)
C
C
C
      BYTE LIST(4*LITEM,LREC),DATA(4*NITEM,NREC)
C
C  Copy the bytes from LIST to DATA
C
      NDIFA = (NBEG-LBEG)*4
      NDIFB = NSTART-LSTART
      LBEGA = 1 + (LBEG-1)*4
      LENDA = LEND*4
      DO K = LSTART,LFIN
         DO J = LBEGA,LENDA
            DATA(NDIFA+J,NDIFB+K) = LIST(J,K)
         ENDDO
      ENDDO
C
C
C
      END



