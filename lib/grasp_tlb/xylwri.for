C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYLWRA *
C      *            *
C      **************
C
C
C  PURPOSE
C      To take an array of parameters with headers, open a disk file
C      and load them into it in the XYlist format, together with
C      identifiers as #1,#2,..#n.
C
C  METHOD
C      Open on disk an appropriate XY list, transfer the data
C      from the array to the list, add identifiers, load a
C      title, the headers and file descriptors.
C
C  STARLINK PARAMETERS
C
C       XYWOUT                         The name to be given to the
C                                      XY list
C
C       XYWTIT         Output          The title to be added to list
C
C
C  ARGUMENTS
C     (IN)
C
C       DATA
C          The array containing the parameters
C       NUMVAL
C          The number of parameters in a record in DATA
C       LENOUT
C          The number of records in DATA
C       LSTART
C          The first record to be output
C       LFIN
C          The last record to be output
C       LBEG
C          The first parameter in each record to be output
C       LEND
C          The last paramter in each record to be output
C       HEADER
C          The headers to be added
C
C     (OUT)
C
C       KERR
C          An error flag. = 0 for correct run through, 1 for fault
C
C
C  CALLS
C
C     This file
C          IDENT
C     Grasp
C          EXTLSA,PTHEAD
C     EDRS package
C          GTXYLW,LBGONE,PTDSCR
C     STARLINK
C          RDKEYC,WRUSER
C
C  NOTES
C     Uses VAX %VAL facility
C
C  WRITTEN BY
C       A.J.Penny                                    83-2-15
C -----------------------------------------------------------------



      SUBROUTINE XYLWRA(DATA,NUMVAL,LENOUT,LSTART,LFIN,LBEG,LEND,
     +                  HEADER,KERR)
C
C
C
      REAL DATA(NUMVAL,LENOUT)
      CHARACTER*30 HEADER(NUMVAL)
      CHARACTER TITLE(1)*30,PRBUF*40,CVAL*1
      LOGICAL*1 VALID
C
C
C
      VALID = .TRUE.
C
C  Check that there are some values to store
C
      IF (LENOUT.LE.0.OR.LSTART.GT.LFIN.OR.LBEG.GT.LEND) THEN
         CALL WRUSER('ZERO OUTPUT ENTRIES, NO LIST OUTPUT',IERR1)
         VALID = .FALSE.
      ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         NITEM = LEND - LBEG + 1 + 5
         LSTLEN = LFIN - LSTART + 1
         CALL WRUSER('OUTPUT FILE NAME IS ?',ISTAT)
         CALL GTXYLW('XYWOUT',.FALSE.,NITEM,LSTLEN,IPOUT,IERR1)
         IF (IERR1.NE.0) THEN
            CALL WRUSER('CANT STORE OUTPUT LIST',IERR2)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C
C
      IF (VALID) THEN
C
C  Copy data to output list and put in identifiers
C
         CALL EXTLSA(DATA,NUMVAL,LENOUT,LSTART,LFIN,LBEG,LEND,
     +               %VAL(IPOUT),NITEM,LSTLEN,1,6)
         CALL IDENT(%VAL(IPOUT),NITEM,LSTLEN)
C
C Tell user how many entries in output list
C
         WRITE(PRBUF,104)LSTLEN
         IF(LENOUT.EQ.1) PRBUF(28:)='ENTRY'
  104    FORMAT('   OUTPUT LIST HAS ',I7,' ENTRIES')
         CALL LBGONE(PRBUF(20:))
         CALL WRUSER(PRBUF,ISTAT)
C
C Get title to output list and store it and the headers and descriptors
C
         CALL WRUSER('OUTPUT FILE TITLE IS ?',ISTAT)
         TITLE(1)='Output'
         CALL RDKEYC('XYWTIT',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL PTDSCR('XYWOUT','NITEM','INTEGER',NITEM,RVAL,CVAL,IERR)
         CALL PTDSCR('XYWOUT','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,
     +               IERR)
         CALL PTDSCR('XYWOUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +	       IERR)
         CALL PTHEAD('XYWOUT',NUMVAL,HEADER,IERR)
C
C
C
      ENDIF
C
C Set s/r error flag
C
      IF (VALID) THEN
         KERR = 0
      ELSE
         KERR = 1
      ENDIF
C
C
C
      END



