C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C
C
C                     ******************* 
C                     *                 *
C                     * Program DENSAV * 
C                     *                 *
C                     ******************* 
C
C
C
C          CALLING SEQUENCE:- 
C               DENSAV 
C
C
C          FUNCTION:- 
C               It allows one to correct for the effect of density
C               averaging when using a measuring machine to measure
C               a raster over a star image
C               For a Gaussian profile, this correction is :-
C
C               DM = -1.04*PeakDensity*(ap width/e-folding radius)**2
C
C               This program does a corrn of DM = -1*Factor*PeakHeight
C               where you input the Factor.
C               Beware the fact it wants it in terms of star height, and
C               not star density
C
C
C          USE:- 
C               Take the file from the output of LORMUL,LORCUR,LORSIM,
C               GAUMAG  or any similar program, input the 'Factor',
C               and write an identical file with the mags corrected.
C
C
C
C         USER PARAMETERS:- 
C
C         INPUT                               This is the name of the  .BDF 
C                                             input file
C
C         FACTOR                              The correction factor
C
C         OUTPUT                              The name of the .BDF output
C                                             output file.
C
C         TITLE                              The Title to be added to the
C                                             output file
C
C
C         A J Penny            RGO                                21-SEP-82
C
C
C-------------------------------------------------------------------------- 



*
*
*  ARGUMENTS
*        None
*
*  STARLINK PARAMETERS
*     INPUT
*            The input EDRS XY file
*     FACTOR
*     OUTPUT
*     TITLE
*
*  CALLS
*      EDRS Package
*            GTXYLR,GTDSCR
*      STARLINK:
*            WRUSER,FRDATA
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-9-21
* ------------------------------------------------------------



      PROGRAM DENSAV
C
C
C
      INCLUDE 'STARDIR:ERRPAR.FOR/NOLIST'
      INCLUDE 'STARDIR:FMTPAR.FOR/NOLIST'
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
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERRXY)
      IF(IERRXY.NE.0) VALID = .FALSE.
C
C  Extract Title
C
      IF (VALID) THEN
         TITLE = ' '
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,IERR)
         WRITE (PRBUF,902)TITLE
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C  Check that there are some values to work on
C
      IF (VALID) THEN
         IF (LSTLEN.LE.0) THEN
            CALL WRUSER('ZERO ENTRIES, NO LIST OUTPUT',IERRI)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get the correction factor
C
      IF (VALID) THEN
         CALL RDKEYR('FACTOR',.FALSE.,1,FACTOR,NVAL,ISTAT)
         IF (ISTAT.NE.0) VALID = .FALSE.
      ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,NITEM,LSTLEN,IPOUT,IERR)
         IF (IERR.NE.0) THEN
            CALL WRUSER('CANT STORE OUTPUT LIST',IERR1)
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
         CALL EXTLSA(%VAL(IPIN),NITEM,LSTLEN,1,LSTLEN,1,NITEM,
     +               %VAL(IPOUT),NITEM,LSTLEN,1,1)
C Get title to output list and store it and the descriptors
C
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL PTDSCR('OUTPUT','NITEM','INTEGER',NITEM,RVAL,CVAL,IERR)
         CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,
     +               IERR)
         CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	       IERR)
C
C  Do the correction
C
         CALL CORR(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPOUT),FACTOR)
C
C  Type no of entries
C
         IF (LSTLEN.NE.1) THEN
            WRITE(PRBUF,920)LSTLEN
  920       FORMAT(T22,I7,' LIST ENTRIES')
            CALL WRUSER(PRBUF,ISTAT)
  921       FORMAT (' ',22X,I6,' LIST ENTRIES')
         ELSE
            CALL WRUSER('                         1 LIST ENTRY',ISTAT)
  922       FORMAT (' ',22X,'   ONE LIST ENTRY')
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ************
C      *          *
C      * S/R CORR *
C      *          *
C      ************
C
C
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE CORR(DATA,NITEM,LSTLEN,RES,FACTOR)
C
C
C
      REAL DATA(NITEM,LSTLEN),RES(NITEM,LSTLEN)
C
C
C
      DO K = 1,LSTLEN
         DM = -1.0*FACTOR*DATA(14,K)
         RES(8,K) = DATA(8,K) + CORN
      ENDDO
C
C
C
      END



