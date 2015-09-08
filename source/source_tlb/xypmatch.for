C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYPMATCH*
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYPMATCH
C
C
C          FUNCTION:-
C               This takes two XYlists and makes two new ones
C               which are copies of the old ones, but only
C               contain the entries which match in X,Y position.
C                  The new file entries are in the same order as
C               order they occur in the 1st file.
C
C               If the 2nd file has more than one star near enough to
C               the position of the star in the 1st, only the nearest
C               star in the second is taken.
C
C
C
C
C         USER PARAMETERS:-
C
C         INPUTA                              This is the name of the
C                                             1st input XY file
C
C         INPUTB                              This is the name of the
C                                             2nd input XY file.
C
C         MISMATCH      1.0                   Distance two stars can be
C                                             apart and still taken as
C                                             the same.
C
C         TITLEA                             The Title to be added to that
C                                             output file
C
C         OUTPUTB                             The name of the output
C                                             XY file for the 2nd file.
C
C         TITLEB                             The Title to be added to that
C                                             output file
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



      PROGRAM XYMATCH
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLEA*30,TITLEB*30,PRBUF*72
      LOGICAL*1 VALID
C
C  Set continuation

C
      VALID = .TRUE.
C
C  Obtain input XY list
C
      CALL GTXYLR('INPUTA',.FALSE.,NITEMA,LSTA,IPINA,IERR)
      IF(IERR.NE.0) VALID = .FALSE.
C
C  Extract Title
C
      IF (VALID) THEN
         TITLEA = 'Output from XYPMATCH'
         CALL GTDSCR('INPUTA','TITLE','CHARACTER',IVAL,RVAL,TITLEA,
     +               IERR)
         WRITE (PRBUF,902)TITLEA
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C  Check that there are some values to store
C
      IF (VALID) THEN
         IF (LSTA.EQ.0.OR.NITEMA.EQ.0) THEN
            CALL WRUSER('FILE ZERO',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Obtain 2nd input XY list
C
      CALL GTXYLR('INPUTB',.FALSE.,NITEMB,LSTB,IPINB,IERR)
      IF(IERR.NE.0) VALID = .FALSE.
C
C  Extract Title
C
      IF (VALID) THEN
         TITLEB = 'Output from XYPMATCH'
         CALL GTDSCR('INPUTB','TITLE','CHARACTER',IVAL,RVAL,TITLEB,
     +               IERR)
         WRITE (PRBUF,902)TITLEB
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C  Check that there are some values to store
C
      IF (VALID) THEN
         IF (LSTB.EQ.0.OR.NITEMB.EQ.0) THEN
            CALL WRUSER('FILE ZERO',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get distance tolerance
C
      IF (VALID) THEN
         TOLERN = 1.0
         CALL GETPAR('MISMATCH','REAL',1,0.0,1.0E10,.TRUE.,IVAL,
     +               TOLERN,ISTAT)
         IF (ISTAT.NE.0) VALID = .FALSE.
      ENDIF
C
C  Find no of entries passed
C
        IF (VALID) THEN
           CALL CHECKL(%VAL(IPINA),NITEMA,LSTA,%VAL(IPINB),
     +                 NITEMB,LSTB,TOLERN,NOKA,NOKB)
           IF (NOKA.EQ.0.AND.NOKB.EQ.0) THEN
              VALID = .FALSE.
              CALL WRUSER('NO ENTRIES COPIED',ISTAT)
           ELSE
              WRITE(PRBUF,903)NOKA
  903         FORMAT(' ','NO OF MATCHES = ',I5)
              CALL WRUSER(PRBUF,ISTAT)
           ENDIF
        ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         IF (NOKA.NE.0) THEN
            CALL GTXYLW('OUTPUTA',.FALSE.,NITEMA,NOKA,IPOUTA,IERR)
            IF (IERR.NE.0) THEN
               CALL WRUSER('CANT STORE OUTPUT LIST',IERR1)
               VALID = .FALSE.
            ELSE
C
C Get title to output list and store it and the descriptors
C
               CALL PTDSCR('OUTPUTA','NITEM','INTEGER',NITEMA,
     +                     RVAL,CVAL,IERR)
               CALL PTDSCR('OUTPUTA','LSTLEN','INTEGER',NOKA,
     +                     RVAL,CVAL,IERR)
               CALL RDKEYC('TITLE',.TRUE.,1,TITLEA,NVAL,ISTAT)
               CALL CNPAR('TITLE',ISTAT)
               CALL PTDSCR('OUTPUTA','TITLE','CHARACTER',IVAL,
     +	             RVAL,TITLEA,IERR)
            ENDIF
         ENDIF
      ENDIF
C
C  Open 2nd output list
C
      IF (VALID) THEN
         IF (NOKB.NE.0) THEN
            CALL GTXYLW('OUTPUTB',.FALSE.,NITEMB,NOKB,IPOUTB,IERR)
            IF (IERR.NE.0) THEN
               CALL WRUSER('CANT STORE OUTPUT LIST',IERR1)
               VALID = .FALSE.
            ELSE
C
C Get title to output list and store it and the descriptors
C
               CALL PTDSCR('OUTPUTB','NITEM','INTEGER',NITEMB,
     +                     RVAL,CVAL,IERR)
               CALL PTDSCR('OUTPUTB','LSTLEN','INTEGER',NOKB,
     +                     RVAL,CVAL,IERR)
               CALL RDKEYC('TITLE',.TRUE.,1,TITLEB,NVAL,ISTAT)
               CALL PTDSCR('OUTPUTB','TITLE','CHARACTER',IVAL,
     +	             RVAL,TITLEB,IERR)
            ENDIF
         ENDIF
      ENDIF
C
C  Do the loading of the Output from the Input
C
      IF (VALID) THEN
         IF (NOKA.NE.0) THEN
            CALL TRANS(%VAL(IPINA),NITEMA,LSTA,%VAL(IPINB),
     +                 NITEMB,LSTB,%VAL(IPOUTA),NITEMA,NOKA,
     +                 %VAL(IPOUTB),NITEMB,NOKB,TOLERN)
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



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R CHECKL *
C      *            *
C      **************
C
C
C    A J PENNY               RGO                      82-11-4
C ------------------------------------------------------------
C
C
C
      SUBROUTINE CHECKL(DATA,NITEMA,LSTA,DATB,NITEMB,
     +                  LSTB,TOLERN,NOKA,NOKB)
C
C
C
      REAL DATA(NITEMA,LSTA),DATB(NITEMB,LSTB)
C
C
C
      TTOL = TOLERN*TOLERN
      NOKA = 0
      DO K = 1,LSTA
         XA = DATA(6,K)
         YA = DATA(7,K)
         KD = 0
         L = 0
         DO WHILE (L.LT.LSTB.AND.KD.EQ.0)
            L = L + 1
            XB = DATB(6,L)
            YB = DATB(7,L)
            DD = (XB-XA)*(XB-XA) + (YB-YA)*(YB-YA)
            IF (DD.LE.TTOL) KD = 1
         ENDDO
         IF (KD.EQ.1) NOKA = NOKA + 1
      ENDDO
C
      NOKB = NOKA
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R TRANS  *
C      *            *
C      **************
C
C
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE TRANS(DATA,NITEMA,LSTA,DATB,NITEMB,LSTB,
     +                 OUTA,NITOUA,LSTOUA,OUTB,NITOUB,LSTOUB,
     +                 TOLERN)
C
C
C
      REAL DATA(NITEMA,LSTA),DATB(NITEMB,LSTB),OUTA(NITOUA,LSTOUA)
      REAL OUTB(NITOUB,LSTOUB)
C
C
C
      TTOL = TOLERN*TOLERN
      KOUT = 0
      DO K = 1,LSTA
         XA = DATA(6,K)
         YA = DATA(7,K)
         DMIN = -1.0
         DO L = 1,LSTB
            XB = DATB(6,L)
            YB = DATB(7,L)
            DD = (XB-XA)*(XB-XA) + (YB-YA)*(YB-YA)
            IF (DD.LE.TTOL) THEN
               IF (DMIN.LT.-0.5) THEN
                  DMIN = DD
                  KDMIN = L
               ELSE
                  IF (DD.LT.DMIN) THEN
                     DMIN = DD
                     KDMIN = L
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF (DMIN.GT.-0.5) THEN
            KOUT = KOUT + 1
            CALL EXTLSA(DATA,NITEMA,LSTA,K,K,1,NITEMA,
     +                  OUTA,NITOUA,LSTOUA,KOUT,1)
            CALL EXTLSA(DATB,NITEMB,LSTB,KDMIN,KDMIN,1,NITEMB,
     +                  OUTB,NITOUB,LSTOUB,KOUT,1)
         ENDIF
      ENDDO
C
C
C
      END



