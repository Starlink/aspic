C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYLWRB *
C      *            *
C      **************
C
C
C  PURPOSE
C      To open an
C      EDRS format XY list, placing identifiers in the format #1
C      to #n, with headers for the parameters
C
C
C  ARGUMENTS
C     (IN)
C    FILE   Char*(*)    The STARLINK PARAMETER name of the file
C    TITLE  Char*(*)    The STARLINK PARAMETER name of its title
C    NITEM  Int         The number of parameters in a record + 5
C    LSTLEN Int         The number of records
C    HEADER Char*20(NITEM-5) The headers
C
C     (OUT)
C
C    IPOUT  INT         VAX %val pointer to file
C    ISTAT  Int         Error flag. = 0 for correct run through, 1 for fault
C
C
C  CALLS
C
C     This file
C          IDENT
C     Grasp
C          PTHEAD
C     EDRS package
C          LBGONE,PTDSCR
C     STARLINK
C          RDKEYC,WRUSER
C
C  NOTES
C     Uses VAX %VAL facility
C
C  WRITTEN BY
C       A.J.Penny           RGO                   84-2-6  17:53
C -----------------------------------------------------------------



      SUBROUTINE XYLWRB(FILE,TITLE,NITEM,LSTLEN,HEADER,IPOUT,ISTAT)
C
C
C
      CHARACTER*(*) HEADER(NITEM-5),FILE,TITLE
      CHARACTER CTITLE*72,PRBUF*40,CVAL
      LOGICAL VALID
C
C
C
      VALID = .TRUE.
C
C  Check there is something to open
C
      IF (LSTLEN.LT.1.OR.NITEM.LT.6) THEN
         CALL WRUSER('ZERO OUTPUT ENTRIES, NO LIST OUTPUT',IERR1)
         VALID = .FALSE.
      ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         CALL GTXYLW(FILE,.FALSE.,NITEM,LSTLEN,IPOUT,IERR1)
         IF (IERR1.NE.0) THEN
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Load the #1...#n identifiers
C
      IF (VALID) THEN
         CALL IDENT(%VAL(IPOUT),NITEM,LSTLEN)
      ENDIF
C
C Tell user how many entries in output list
C
      IF (VALID) THEN
         WRITE(PRBUF,104)LSTLEN
         IF(LEN.EQ.1) PRBUF(28:)='ENTRY'
  104    FORMAT('   OUTPUT LIST HAS ',I7,' ENTRIES')
         CALL LBGONE(PRBUF(20:))
         CALL WRUSER(PRBUF,IERR)
      ENDIF
C
C  Load the descriptors
C
      IF (VALID) THEN
         CALL RDKEYC(TITLE,.FALSE.,1,CTITLE,NVAL,ISTAT)
         CALL PTDSCR(FILE,'NITEM','INTEGER',NITEM,RVAL,CVAL,IERR)
         CALL PTDSCR(FILE,'LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,IERR)
         CALL PTDSCR(FILE,'TITLE','CHARACTER',IVAL,RVAL,CTITLE,IERR)
         DO K = 1,NITEM-5
            CALL PTHEAD(FILE,K,HEADER(K),IERR)
         ENDDO
      ENDIF
C
C Set s/r error flag
C
      IF (VALID) THEN
         ISTAT = 0
      ELSE
         KERR = 1
      ENDIF
C
C
C
      END


