C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program MAGCOR  *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               MAGCOR
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
C               This program does a corrn of DM = -0.01*Factor*PeakHeight
C               where you input the Factor.
C               Beware the fact it wants it in terms of star height, and
C               not star density and for 0.01 mags. For the PDS, where a
C               density of 1 is at 754, and using an aperture 1/5th
C               the radius, the factor is 0.00055, if you assume a perfect
C               aperture and a perfect Gaussian, with a Gaussian fit.
C
C
C          USE:-
C               Take the file from the output of LORMUL,LORCUR,LORSIM,
C               GAUMAG  or any similar program, input the 'Factor',
C               and write an identical file with the mags corrected.
C
C               For the input file it must be an XYlist with the mags
C               as parameter 3 and heights as parameter 9.
C
C               The corrections can be typed out as it goes.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the  .BDF
C                                             input file
C
C         FACTOR          0.00055             The correction factor
C
C         TYPING          YES                 Flag for typing out corrections
C                                             as it goes.
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
C
C
C
C
C   CALLS
C     Starlink,Aspic,Edrs
C     GTXYLR,GTDSCR,WRUSER,RDKEYR,GTXYLW,RDKEYC,PTDSCR,FRDAT
C
C     Grasp
C     EXTLSA
C
C     This file
C     CORR
C
C   USES
C     %VAL facility
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    82-NOV
*
*
* ------------------------------------------------------------



      PROGRAM MAGCOR
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
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERRXY)
      IF(IERRXY.NE.0) VALID = .FALSE.
C
C  Extract Title
C
      IF (VALID) THEN
         TITLE = 'Output from MAGCOR'
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,IERR)
         WRITE (PRBUF,902)TITLE
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C  Check that there are some values to store
C
      IF (VALID) THEN
         IF (LSTLEN.EQ.0) THEN
            CALL WRUSER('ZERO OUTPUT ENTRIES, NO LIST OUTPUT',
     +                  IERRI)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get the correction factor and typing flag
C
      IF (VALID) THEN
         FACTOR = 0.00055
         CALL RDKEYR('FACTOR',.TRUE.,1,FACTOR,NVAL,ISTAT)
         KTYPE = 1
         CALL GETCMD('TYPING','YES,NO.',1,KTYPE,TEXT,KT,IERR)
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
C
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
         CALL CORR(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPOUT),FACTOR,KTYPE)
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
C
C   PURPOSE
C     Takes the magnitudes of stars (DATA(8,K)) and adjusts
C     them for density averaging using their heights (DATA(14,K))
C
C   ARGUMENTS
C     IN
C       DATA    Input array of results
C       NITEM   No of parameters+5 = total width
C       LSTLEN  No of entries
C       FACTOR  Correction factor
C       KTYPE   Flag for typing out corrections
C     OUT
C       RES     Corrected magnitudes
C
C   CALLS
C     Starlink,Aspic,Edrs
C       WRUSER
C
C
C
C   A.J.PENNY                   RGO                    82-NOV
C ---------------------------------------------------------
C
C
C
      SUBROUTINE CORR(DATA,NITEM,LSTLEN,RES,FACTOR,KTYPE)
C
C
C
      REAL DATA(NITEM,LSTLEN),RES(NITEM,LSTLEN)
      CHARACTER PRBUF*72
C
C
C
      IF (KTYPE.EQ.1) THEN
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER(' STAR NO   CORRN    MAG       HEIGHT',ISTAT)
      ENDIF
C
C
C
      DO K = 1,LSTLEN
         DM = -0.01*FACTOR*DATA(14,K)
         IF (KTYPE.EQ.1) THEN
            WRITE(PRBUF,900)K,DM,DATA(8,K),DATA(14,K)
  900       FORMAT (' ',I5,3X,F7.2,F8.2,F12.2)
            CALL WRUSER(PRBUF,ISTAT)
         ENDIF
         RES(8,K) = DATA(8,K) + DM
      ENDDO
C
C
C
      END



