C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYWEED  *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYWEED
C
C
C          FUNCTION:-
C               This makes a new XY list which is a copy of the old
C               XY list, but only those entries whose Nth parameter
C               falls in the assigned range or outside it are
C               copied over.
C
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the
C                                             input XY file
C
C
C         OPTION        Accept                Accept or reject stars
C                                             whose parameter falls in
C                                             the given range.
C
C         TOPLIM                              The upper acceptable
C                                             value of the parameter
C
C         BOTLIM                              The lower acceptable
C                                             value of the parameter
C
C         PARNUM                              The number of the
C                                             parameter that is to be
C                                             tested. (the star name
C                                             is not counted as a
C                                             parameter, the 1st one
C                                             after it is parameter no 1)
C
C         OUTPUT                              The name of the output
C                                             XY file.
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



      PROGRAM XYWEED
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,PRBUF*72
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
         TITLE = 'Output from XYWEED'
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +               IERR)
         WRITE (PRBUF,902)TITLE
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(PRBUF,ISTAT)
         NPARMA = NITEM - 5
         WRITE(PRBUF,900)NPARMA
  900    FORMAT(' ','NO OF PARAMS = ',I5)
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
C  Get wether accept or reject inside the limits
C
      IF (VALID) THEN
         KACC = 1
         CALL WRUSER('ACCEPT OR REJECT INSIDE THE LIMITS ?',ISTAT)
         CALL GETCMD('OPTION','ACCEPT,REJECT.',1,KACC,PRBUF,
     +               KTEXT,IERR)
         IF (KACC.NE.1) KACC = 2
      ENDIF
C
C  Get the limits
C
      IF (VALID) THEN
         CALL RDKEYR('TOPLIM',.FALSE.,1,TOPLIM,NVAL,ISTAT)
         CALL CNPAR('TOPLIM',ISTAT)
         CALL RDKEYR('BOTLIM',.FALSE.,1,BOTLIM,NVAL,ISTAT)
         CALL CNPAR('BOTLIM',ISTAT)
      ENDIF
C
C  Get position of test parameter
C
       IF (VALID) THEN
          CALL RDKEYI('PARNUM',.FALSE.,1,KPAR,NVAL,ISTAT)
          CALL CNPAR('PARNUM',ISTAT)
       ENDIF
C
C  Find no of entries passed
C
        IF (VALID) THEN
           CALL CHECKL(%VAL(IPIN),NITEM,LSTLEN,KPAR,BOTLIM,
     +                 TOPLIM,KACC,NOK)
           IF (NOK.EQ.0) THEN
              VALID = .FALSE.
              CALL WRUSER('NO ENTRIES IN RANGE',ISTAT)
           ELSE
              WRITE(PRBUF,903)NOK
  903         FORMAT(' ','NO IN RANGE = ',I5)
              CALL WRUSER(PRBUF,ISTAT)
           ENDIF
        ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,NITEM,NOK,IPOUT,IERR)
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
         CALL PTDSCR('OUTPUT','NITEM','INTEGER',NITEM,RVAL,CVAL,
     +               IERR)
         CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',NOK,RVAL,CVAL,
     +               IERR)
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	       IERR)
C
C  Do the loading of the Output from the Input
C
          CALL DOWEED(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPOUT),NOK,
     +                BOTLIM,TOPLIM,KPAR,KACC)
C
C  Type no of entries
C
         IF (NOK.NE.1) THEN
            WRITE(PRBUF,920)NOK
  920       FORMAT(T22,I7,' OUTPUT ENTRIES')
            CALL WRUSER(PRBUF,ISTAT)
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
      SUBROUTINE CHECKL(DATA,NITEM,LSTLEN,KPAR,BOTLIM,
     +                  TOPLIM,KACC,NOK)
C
C
C
      REAL DATA(NITEM,LSTLEN)
C
C
C
      KPARA = KPAR + 5
      NOK = 0
      DO K = 1,LSTLEN
         D = DATA(KPARA,K)
         IF (KACC.EQ.1) THEN
            IF (D.GE.BOTLIM.AND.D.LE.TOPLIM) NOK = NOK + 1
         ELSE
            IF (D.LT.BOTLIM.OR.D.GT.TOPLIM) NOK = NOK + 1
         ENDIF
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
C      * S/R DOWEED *
C      *            *
C      **************
C
C
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE DOWEED(DATA,NITEM,LSTLEN,OUT,NOK,BOT,TOP,KPAR,
     +                  KACC)
C
C
C
      REAL DATA(NITEM,LSTLEN),OUT(NITEM,NOK)
C
C
C
      KPARA = KPAR + 5
      N = 0
      DO K = 1,LSTLEN
         D = DATA(KPARA,K)
         KFLAG = 2
         IF (D.GE.BOT.AND.D.LE.TOP) KFLAG = 1
         IF (KFLAG.EQ.KACC) THEN
            N = N + 1
            CALL EXTLSA(DATA,NITEM,LSTLEN,K,K,1,NITEM,
     +                  OUT,NITEM,NOK,N,1)
         ENDIF
      ENDDO
C
C
C
      END



