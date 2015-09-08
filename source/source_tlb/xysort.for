C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYSORT *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYSORT
C
C
C          FUNCTION:-
C               This takes an XY file and makes a new one with
C               identical entries, but sorted on the values of
C               the user defined parameter number, either in
C               ascending or descending order.
C
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the
C                                             input XY file
C
C         PARNUM                              The number of the parameter
C                                             to sort on
C
C         OPTION         Ascending            Order for sorting
C                                             Choices are ASCENDING,DESCENDING
C
C         OUTPUT                              The name of the output
C                                             XY file for the file.
C
C         TITLE                               The Title to be added to that
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



      PROGRAM XYSORT
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,PRBUF*72
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
         TITLE = 'Output from XYSORT'
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +               IERR)
         WRITE (PRBUF,902)TITLE
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(PRBUF,ISTAT)
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
C  Get parameter number to sort on
C
      IF (VALID) THEN
         ALIM = REAL(NITEM-5)
         CALL GETPAR('PARNUM','INTEGER',1,1.0,ALIM,.FALSE.,KPAR,
     +               RVAL,IERR)
         KPAR = KPAR + 5
      ENDIF
C
C  Get ascending or descending
C
      IF (VALID) THEN
         KACC = 1
         CALL WRUSER('ASCENDING OR DESCENDING SORT ?',ISTAT)
         CALL GETCMD('OPTION','ASCENDING,DESCENDING.',1,KACC,PRBUF,
     +               KTEXT,IERR)
         IF (KACC.NE.1) KACC = 2
      ENDIF
C
C  Get working space for sort
C
      IF (VALID) THEN
         CALL GETDYN('NUMBER',FMT_SL,LSTLEN,IPNUM,IERR)
         CALL GETDYN('VALUES',FMT_R,LSTLEN,IPVAL,IERRA)
         IF (IERR.NE.0.OR.IERRA.NE.O) THEN
            CALL WRUSER('NO SPACE FOR WORKING',ISTAT)
            VALID = .FALSE.
         ENDIF
       ENDIF
C
C  Copy the sorting parameter into the working area and perform sort
C
      IF (VALID) THEN
         CALL EXTLSA(%VAL(IPIN),NITEM,LSTLEN,1,LSTLEN,KPAR,KPAR,
     +               %VAL(IPVAL),1,LSTLEN,1,1)
         CALL SORT(%VAL(IPVAL),LSTLEN,KACC,%VAL(IPNUM))
      ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,NITEM,LSTLEN,IPOUT,IERR)
         IF (IERR.NE.0) THEN
            CALL WRUSER('CANT STORE OUTPUT LIST',IERR1)
            VALID = .FALSE.
         ELSE
C
C Get title to output list and store it and the descriptors
C
            CALL PTDSCR('OUTPUT','NITEM','INTEGER',NITEM,
     +                  RVAL,CVAL,IERR)
            CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,
     +                  RVAL,CVAL,IERR)
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,
     +	          RVAL,TITLE,IERR)
         ENDIF
      ENDIF
C
C  Do the loading of the Output from the Input
C
      IF (VALID) THEN
         CALL TRANS(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPOUT),
     +              %VAL(IPNUM))
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
C      * S/R SORT   *
C      *            *
C      **************
C
C
C    A J PENNY               RGO                      82-11-4
C ------------------------------------------------------------
C
C
C
      SUBROUTINE SORT(VAL,LSTLEN,KACC,NUM)
C
C
C
      REAL VAL(LSTLEN)
      INTEGER NUM(LSTLEN)
C
C
C
      DO K = 1,LSTLEN
         NUM(K) = K
      ENDDO
C
C
C
      IF (KACC.EQ.1.AND.LSTLEN.GT.1) THEN
         DO I = LSTLEN,2,-1
            DO J = 1,I-1
               JG = J + 1
               IF (VAL(J).GT.VAL(JG)) THEN
                  VK = VAL(J)
                  VAL(J) = VAL(JG)
                  VAL(JG) = VK
                  K = NUM(J)
                  NUM(J) = NUM(JG)
                  NUM(JG) = K
               ENDIF
            ENDDO
         ENDDO
      ENDIF
C
C
C
      IF (KACC.EQ.2.AND.LSTLEN.GT.1) THEN
         DO I = LSTLEN,2,-1
            DO J = 1,I-1
               JG = J + 1
               IF (VAL(J).LT.VAL(JG)) THEN
                  VK = VAL(J)
                  VAL(J) = VAL(JG)
                  VAL(JG) = VK
                  K = NUM(J)
                  NUM(J) = NUM(JG)
                  NUM(JG) = K
               ENDIF
            ENDDO
         ENDDO
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R TRANS  *
C      *            *
C      **************
C
C
C      A J PENNY               RGO                   82-11-4
C ----------------------------------------------------------
C
C
C
      SUBROUTINE TRANS(DATA,NITEM,LSTLEN,OUTPUT,NUM)
C
C
C
      REAL DATA(NITEM,LSTLEN),OUTPUT(NITEM,LSTLEN)
      INTEGER NUM(LSTLEN)
C
C
C
      DO K = 1,LSTLEN
         KOUT = NUM(K)
         CALL EXTLSA(DATA,NITEM,LSTLEN,KOUT,KOUT,1,NITEM,
     +               OUTPUT,NITEM,LSTLEN,K,1)
      ENDDO
C
C
C
      END



