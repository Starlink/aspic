C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C
C
C                     ******************* 
C                     *                 *
C                     * Program XYMATCH * 
C                     *                 *
C                     ******************* 
C
C
C
C          CALLING SEQUENCE:- 
C               XYMATCH 
C
C
C          FUNCTION:- 
C               This takes two XY lists and makes two new ones
C               which are copies of the old ones, but only
C               contain the entries which do (or optionally do not)
C               have identifiers which occur in the other file.
C                  The new file entries are in the same order as
C               in their old files, except if choosing the
C               matching entries, the 2nd file entries are in the
C               order they occur in the 1st file.
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
C         OPTION        Accept                Accept or reject stars
C                                             who have names in common
C                                             with the other file.
C                                             Choices are ACCEPT,REJECT.
C
C         OUTPUTA                             The name of the output
C                                             XY file for the 1st file.
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
      INCLUDE 'INTERIM(ERRPAR)/NOLIST'
      INCLUDE 'INTERIM(FMTPAR)/NOLIST'
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
         TITLEA = 'Output from XYMATCH'
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
         TITLEB = 'Output from XYMATCH'
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
C  Get wether accept or reject common stars
C
      IF (VALID) THEN
         KACC = 1
         CALL WRUSER('ACCEPT OR REJECT COMMON STARS ?',ISTAT)
         CALL GETCMD('OPTION','ACCEPT,REJECT.',1,KACC,PRBUF,
     +               KTEXT,IERR)
         IF (KACC.NE.1) KACC = 2
      ENDIF
C
C  Find no of entries passed
C
        IF (VALID) THEN
           CALL CHECKL(%VAL(IPINA),NITEMA,LSTA,%VAL(IPINB),
     +                 NITEMB,LSTB,KACC,NOKA,NOKB)
           IF (NOKA.EQ.0.AND.NOKB.EQ.0) THEN
              VALID = .FALSE.
              CALL WRUSER('NO ENTRIES COPIED',ISTAT)
           ELSE
              WRITE(PRBUF,903)NOKA,NOKB
  903         FORMAT(' ','NO IN 1ST = ',I5,'   NO IN 2ND = ',I5)
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
               CALL RDKEYC('TITLEA',.TRUE.,1,TITLEA,NVAL,ISTAT)
               CALL CNPAR('TITLEA',ISTAT)
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
               CALL RDKEYC('TITLEB',.TRUE.,1,TITLEB,NVAL,ISTAT)
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
     +                 KACC)
         ENDIF
         IF (NOKB.NE.0) THEN
            IF (KACC.EQ.1) THEN
               CALL TRANSB(%VAL(IPINA),NITEMA,LSTA,%VAL(IPINB),
     +                     NITEMB,LSTB,%VAL(IPOUTB),NITEMB,NOKB)
            ELSE
               CALL TRANS(%VAL(IPINB),NITEMB,LSTB,%VAL(IPINA),
     +                    NITEMA,LSTA,%VAL(IPOUTB),NITEMB,NOKB,
     +                    KACC)
            ENDIF
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
     +                  LSTB,KACC,NOKA,NOKB)
C
C
C
      REAL DATA(NITEMA,LSTA),DATB(NITEMB,LSTB),TEMPA(5),TEMPB(5)
C
C
C
      NOKA = 0
      DO K = 1,LSTA
         DO J = 1,5
            TEMPA(J) = DATA(J,K)
         ENDDO
         KD = 0
         L = 0
         DO WHILE (L.LT.LSTB.AND.KD.EQ.0)
            L = L + 1
            DO J = 1,5
               TEMPB(J) = DATB(J,L)
            ENDDO
            CALL CHENAM(TEMPA,TEMPB,KD)
         ENDDO
         KDA = 2
         IF (KD.EQ.1) KDA = 1
         IF (KDA.EQ.KACC) NOKA = NOKA + 1
      ENDDO
C
C
C
      NOKB = 0
      DO K = 1,LSTB
         DO J = 1,5
            TEMPB(J) = DATB(J,K)
         ENDDO
         KD = 0
         L = 0
         DO WHILE (L.LT.LSTA.AND.KD.EQ.0)
            L = L + 1
            DO J = 1,5
               TEMPA(J) = DATA(J,L)
            ENDDO
            CALL CHENAM(TEMPA,TEMPB,KD)
         ENDDO
         KDA = 2
         IF (KD.EQ.1) KDA = 1
         IF (KDA.EQ.KACC) NOKB = NOKB + 1
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R CHENAM *
C      *            *
C      **************
C
C
C      A J PENNY               RGO                   82-11-4
C ----------------------------------------------------------
C
C
C
      SUBROUTINE CHENAM(KA,KB,KD)
C
C
C
      INTEGER KA(5),KB(5)
C
C
C
      KD = 1
      DO K = 1,5
         IF (KA(K).NE.KB(K)) KD = 0
      ENDDO
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
     +                 OUT,NITOUT,LSTOUT,KACC)
C
C
C
      REAL DATA(NITEMA,LSTA),DATB(NITEMB,LSTB),OUT(NITOUT,LSTOUT)
      REAL TEMPA(5),TEMPB(5)
C
C
C
      KOUT = 0
      DO K = 1,LSTA
         DO J = 1,5
            TEMPA(J) = DATA(J,K)
         ENDDO
         KD = 0
         L = 0
         DO WHILE (L.LT.LSTB.AND.KD.EQ.0)
            L = L + 1
            DO J = 1,5
               TEMPB(J) = DATB(J,L)
            ENDDO
            CALL CHENAM(TEMPA,TEMPB,KD)
         ENDDO
         KDA = 2
         IF (KD.EQ.1) KDA = 1
         IF (KDA.EQ.KACC) THEN
            KOUT = KOUT + 1
            CALL EXTLSA(DATA,NITEMA,LSTA,K,K,1,NITEMA,
     +                  OUT,NITOUT,LSTOUT,KOUT,1)
         ENDIF
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ***************
C      *             *
C      * S/R TRANSB  *
C      *             *
C      ***************
C
C
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE TRANSB(DATA,NITEMA,LSTA,DATB,NITEMB,LSTB,
     +                 OUT,NITOUT,LSTOUT)
C
C
C
      REAL DATA(NITEMA,LSTA),DATB(NITEMB,LSTB),OUT(NITOUT,LSTOUT)
      REAL TEMPA(5),TEMPB(5)
C
C
C
      KOUT = 0
      DO K = 1,LSTA
         DO J = 1,5
            TEMPA(J) = DATA(J,K)
         ENDDO
         KD = 0
         L = 0
         DO WHILE (L.LT.LSTB.AND.KD.EQ.0)
            L = L + 1
            DO J = 1,5
               TEMPB(J) = DATB(J,L)
            ENDDO
            CALL CHENAM(TEMPA,TEMPB,KD)
         ENDDO
         IF (KD.EQ.1) THEN
            KOUT = KOUT + 1
            CALL EXTLSA(DATB,NITEMB,LSTB,L,L,1,NITEMB,
     +                  OUT,NITOUT,LSTOUT,KOUT,1)
         ENDIF
      ENDDO
C
C
C
      END



