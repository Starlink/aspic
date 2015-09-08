C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYCUT   *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYCUT
C
C
C          FUNCTION:-
C            This extracts a portion of an XYlist and writes it to
C            a new file. You select the section of columns to be
C            extracted (the parameter section) and the section of rows
C            to be extracted (the record section). Identifiers and
C            Headers are copied over correctly.
C
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the
C                                             input XY file
C         OUTPUT                              The name of the output
C                                             XY file.
C
C         PARSECT         All                 The section of parameters in
C                                             each record to copy. (The
C                                             identifier is not a parameter.)
C
C         RECSECT         All                 The section of records to be
C                                             copied over.
C
C         TITLE                              The Title to be added to the
C                                             output file
C
C
C         A J Penny            RGO                                82-11-4
C
C
C--------------------------------------------------------------------------



*
*
*  CALLS
*      EDRS Package
*            GTXYLR,GTDSCR,GETCMD
*      STARLINK:
*            WRUSER,FRDATA,RDKEYR,RDKEYI,RDKEYC
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-11-4
* ------------------------------------------------------------



      PROGRAM XYCUT
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,TEXT*72,TEXTH*7,HEADER*30
      INTEGER KX(2),KY(2)

      LOGICAL*1 VALID
C
C  Set continuation

C
      VALID = .TRUE.
C
C  Obtain input XY list
C
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERR)
      IF(IERR.NE.0) VALID = .FALSE.
C
C  Extract Title
C
      IF (VALID) THEN
         TITLE = 'Output from XYCUT'
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +               IERR)
         WRITE (TEXT,902)TITLE
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(TEXT,ISTAT)
         NPARMA = NITEM - 5
         WRITE(TEXT,900)NPARMA
  900    FORMAT(' ','NO OF PARAMS = ',I5)
         CALL WRUSER(TEXT,ISTAT)
         WRITE (TEXT,904)LSTLEN
  904    FORMAT(' ','NO OF RECORDS = ',I5)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C  Check that there are some values to store
C
      IF (VALID) THEN
         IF (LSTLEN.EQ.0.OR.NITEM.EQ.0) THEN
            CALL WRUSER('FILE ZERO',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get section to save
C
      IF (VALID) THEN
         KX(1) = 1
         KX(2) = NITEM - 5
         CALL RDKEYI('PARSECT',.TRUE.,2,KX,KN,ISTAT)
         IF (ISTAT.NE.ERR_NORMAL.AND.ISTAT.NE.ERR_PARNUL) THEN
            CALL WRUSER('BAD REPLY',ISTAT)
            VALID = .FALSE.
         ELSE
            IF (KX(2).LT.KX(1)) THEN
               KA = KX(1)
               KX(1) = KX(2)
               KX(2) = KA
            ENDIF
            IF(KX(1).LT.1.OR.KX(1).GT.NITEM.OR.KX(2).LT.1.OR.
     +         KX(2).GT.NITEM) THEN
               CALL WRUSER('OUT OF RANGE',ISTAT)
               VALID = .FALSE.
            ENDIF
C
            KY(1) = 1
            KY(2) = LSTLEN
            CALL RDKEYI('RECSECT',.TRUE.,2,KY,KN,ISTAT)
            IF(ISTAT.NE.ERR_NORMAL.AND.ISTAT.NE.ERR_PARNUL) THEN
               CALL WRUSER('BAD REPLY',ISTAT)
               VALID = .FALSE.
            ELSE
               IF (KY(2).LT.KY(1)) THEN
                  KA = KY(1)
                  KY(1) = KY(2)
                  KY(2) = KA
               ENDIF
               IF(KY(1).LT.1.OR.KY(1).GT.LSTLEN.OR.KY(2).LT.1.OR.
     +            KY(2).GT.LSTLEN) THEN
                  CALL WRUSER('OUT OF RANGE',ISTAT)
                  VALID = .FALSE.
               ENDIF
            ENDIF
C
         ENDIF
      ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         NOUT = KX(2) - KX(1) + 1 + 5
         LENOUT = KY(2) - KY(1) + 1
         CALL GTXYLW('OUTPUT',.FALSE.,NOUT,LENOUT,IPOUT,IERR)
         IF (IERR.NE.0) THEN
            CALL WRUSER('CANT STORE OUTPUT LIST',IERR1)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C
      IF (VALID) THEN
C
C Get title to output list and store it and the descriptors
C
         CALL PTDSCR('OUTPUT','NITEM','INTEGER',NOUT,RVAL,CVAL,
     +               IERR)
         CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LENOUT,RVAL,CVAL,
     +               IERR)
         CALL CHARLN(TITLE,KLEN)
         IF (KLEN.EQ.0) TITLE = 'Output from XYCUT'
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	       IERR)
C
C  Do the loading of the Output from the Input
C
          KYA = KY(1)
          KYB = KY(2)
          KXA = KX(1) + 5
          KXB = KX(2) + 5
          CALL EXTLSA(%VAL(IPIN),NITEM,LSTLEN,KYA,KYB,KXA,KXB,
     +                %VAL(IPOUT),NOUT,LENOUT,1,6)
          CALL EXTLSA(%VAL(IPIN),NITEM,LSTLEN,KYA,KYB,1,5,
     +                %VAL(IPOUT),NOUT,LENOUT,1,1)
C
C  Load the Headers
C
         DO K = KX(1),KX(2)
            WRITE(TEXTH,905)K
  905       FORMAT('HEAD',I3.3)
            CALL GTDSCR('INPUT',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
            J = K - KX(1) + 1
            WRITE(TEXTH,905)J
            CALL PTDSCR('OUTPUT',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
         ENDDO
C
C  Type no of entries
C
         IF (NOK.NE.1) THEN
            WRITE(TEXT,920)LENOUT
  920       FORMAT(T22,I7,' OUTPUT ENTRIES')
            CALL WRUSER(TEXT,ISTAT)
         ELSE
            CALL WRUSER('                      1 OUTPUT ENTRY',ISTAT)
         ENDIF
      ENDIF
C
C  Free data area
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



