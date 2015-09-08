C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYWEEDA *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYWEEDA
C
C
C          FUNCTION:-
C               This makes a new XY list which is a copy of the old
C               XY list, but only those entries whose X,Y lie in
C               a pixel of the reference image which has a non-zero
C               value
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
C         IMAGE                               The reference image which
C                                             defines the acceptable
C                                             areas.
C
C         OPTION        Accept                Choose wether to Accept or
C                                             Reject stars inside the
C                                             areas.
C

C         OUTPUT                              The name of the output
C                                             XY file.
C
C         TITLE                              The Title to be added to the
C                                             output file
C
C
C         A J Penny            RGO                                83-1-4
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
      CHARACTER*72 TITLEA
      INTEGER NAXIS(2)
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
         CALL CHARLN(TITLE,KLEN)
         IF (KLEN.EQ.0) TITLE = 'Output from XYWEEDA'
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
C  Get Reference image
C
      IF (VALID) THEN
         CALL GTIMG(IPIM,NAXIS,BSCALE,BZERO,INVAL,TITLEA,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C  Get wether accept or reject inside the limits
C
      IF (VALID) THEN
         KACC = 1
         CALL WRUSER('ACCEPT OR REJECT INSIDE THE AREAS ?',ISTAT)
         CALL GETCMD('OPTION','ACCEPT,REJECT.',1,KACC,PRBUF,
     +               KTEXT,IERR)
         IF (KACC.NE.1) KACC = 2
      ENDIF
C
C  Find no of entries passed
C
        IF (VALID) THEN
           CALL CHECKL(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPIM),NAXIS(1),
     +                 NAXIS(2),KACC,NOK)
           IF (NOK.EQ.0) THEN
              VALID = .FALSE.
              CALL WRUSER('NO ENTRIES IN AREAS',ISTAT)
           ELSE
              WRITE(PRBUF,903)NOK
  903         FORMAT(' ','NO IN AREAS = ',I5)
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
     +                %VAL(IPIM),NAXIS(1),NAXIS(2),KACC)
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
C    A J PENNY               RGO                      83-1-4
C ------------------------------------------------------------
C
C
C
      SUBROUTINE CHECKL(DATA,NITEM,LSTLEN,IMAGE,NX,NY,KACC,NOK)
C
C
C
      REAL DATA(NITEM,LSTLEN)
      INTEGER*2 IMAGE(NX,NY)
C
C
C
C
C
C
      NOK = 0
      DO K = 1,LSTLEN
         KX = DATA(6,K)
         KY = DATA(7,K)
         IF(KX.LT.1.OR.KX.GT.NX.OR.KY.LT.1.OR.KY.GT.NY)THEN
            IF (KACC.EQ.2) NOK = NOK + 1
         ELSE
            IF (IMAGE(KX,KY).EQ.1) THEN
               IF (KACC.EQ.1) NOK = NOK + 1
            ELSE
               IF (KACC.EQ.2) NOK = NOK + 1
            ENDIF
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
      SUBROUTINE DOWEED(DATA,NITEM,LSTLEN,OUT,NOK,IMAGE,NX,NY,
     +                  KACC)
C
C
C
      REAL DATA(NITEM,LSTLEN),OUT(NITEM,NOK)
      INTEGER*2 IMAGE(NX,NY)
C
C
C
      N = 0
      DO K = 1,LSTLEN
         KFLAG = 0
         KX = DATA(6,K)
         KY = DATA(7,K)
         IF(KX.LT.1.OR.KX.GT.NX.OR.KY.LT.1.OR.KY.GT.NY)THEN
            IF (KACC.EQ.2) KFLAG = 1
         ELSE
            IF (IMAGE(KX,KY).EQ.1) THEN
               IF (KACC.EQ.1) KFLAG = 1
            ELSE
               IF (KACC.EQ.2) KFLAG = 1
            ENDIF
         ENDIF
         IF (KFLAG.EQ.1) THEN
            N = N + 1
            CALL EXTLSA(DATA,NITEM,LSTLEN,K,K,1,NITEM,
     +                  OUT,NITEM,NOK,N,1)
         ENDIF
      ENDDO
C
C
C
      END



