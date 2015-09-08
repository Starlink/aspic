C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C      ******************
C      *                *
C      * Program   ASXY *
C      *                *
C      ******************
C
C
C
C          CALLING SEQUENCE:-
C               ASXY [ ILEVEL=n]
C
C
C          FUNCTION:-
C            It computes the mean,standard deviation,normalised
C         standard deviation and number of valid points in an area
C         of a Starlink EDRS format image.
C            The calculations only use those points within 2.5 standard
C         deviations of the mean after 2 iterations after the first
C         iteration using all the data.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The input image
C
C         XSTART        1                     The X coord of the
C                                             bottom lh corner of the
C                                             area to be analysed
C
C         XEND          X limit               The X coord of the bottom
C                                             rh corner
C
C         YSTART        1                     The Y coord of the bottom
C                                             lh corner
C
C         YEND          Y limit               The Y coord of the top lh
C                                             corner
C
C         AVERAGE                             The computed  average
C
C         STDDEV                              The computed standard
C                                             deviation
C
C         ERRNOM                              STDDEV/AVERAGE, the normalised
C                                             standard deviation
C
C         NGOOD                               This is the number of  "good"
C                                             pixels used in the calcs.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         ILEVEL          2                   The "interaction" level.
C                                             If = 2, the results are
C                                             typed out.
C
C
C
C         A J PENNY                RGO             1-AUG-82
C--------------------------------------------------------------------------


*    Calls
*     This file
*         IMASPA
*     EDRS package
*		GETPAR,GTDSCR,LBGONE.
*	STARLINK:
*		RDIMAG,WRKEYR,WRUSER,FRDATA.
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	A.J. PENNY
* ----------------------------------------------------------------------
C
C
C
      PROGRAM ASXY
C
C
C
      CHARACTER CVAL*1,PRBUF*50,TEXT*72,TITLE(1)*30
      CHARACTER*72 TXT
      LOGICAL*1 VALID
      REAL AV(1),STD(1),ERRNOM(1)
      INTEGER NPT(1),NSIZE(2)
C
      INCLUDE 'INTERIM(FMTPAR)'
C
C   Set continuation flag
C
      VALID = .TRUE.
C
C --------------------------------------------------------------
C
C   Get the image data array
C
      CALL GTIMAG(IPIN,NSIZE,BSCALE,BZERO,INVAL,IERR)
      NPIX = NSIZE(1)
      NLINES = NSIZE(2)
C
C  If cant pick up input frame, set the continuation flag to no
C
      IF (IERR.NE.0) THEN
         VALID = .FALSE.
         CALL WRUSER('NO VALID IMAGE',ISTAT)
      ENDIF
C
C ----------------------------------------------------------------
C
C  Get limits of area
      IF (VALID) THEN
         NXA = 1
         NXB = NPIX
         NYA = 1
         NYB = NLINES
         APIX = FLOAT(NPIX)
         ALINES = FLOAT(NLINES)
         CALL GETPAR('XSTART','INTEGER',1,1.0,APIX,.TRUE.,NXA,VAL,IERR)
         AXA = FLOAT(NXA)
         CALL GETPAR('XEND','INTEGER',1,AXA,APIX,.TRUE.,NXB,VAL,IERR)
         CALL GETPAR('YSTART','INTEGER',1,1.0,ALINES,.TRUE.,NYA,VAL,
     +               IERR)
         AYA = FLOAT(NYA)
         CALL GETPAR('YEND','INTEGER',1,AYA,ALINES,.TRUE.,NYB,VAL,IERR)
      ENDIF
C
C ---------------------------------------------------------------
C
C  Get mean and std dev of area
C
      IF (VALID) THEN
         CALL IMASPA(%VAL(IPIN),NPIX,NLINES,NXA,NXB,NYA,NYB,
     +               INVAL,AV(1),STD(1),NPT(1))
C
C  Scale the results by image scale
C
         AV(1) = AV(1)*BSCALE + BZERO
         STD(1) = STD(1)*BSCALE
         IF (ABS(AV(1)).LT.1.0E-15) THEN
            ERRNOM(1) = 1.0E-15
         ELSE
            ERRNOM(1) = STD(1)/AV(1)
         ENDIF
      ELSE
C  If no image, set default values
         AV(1) = 0.0
         STD(1) = 0.0
         ERRNOM(1) = 0.0
         NPT(1) = 0
      ENDIF
C
C WRITE RESULT TO THE ENVIRONMENT
C
      CALL WRKEYR('AVERAGE',AV,1,ISTAT)
      CALL WRKEYR('STDDEV',STD,1,ISTAT)
      CALL WRKEYR('ERRNOM',ERRNOM,1,ISTAT)
      CALL WRKEYI('NGOOD',NPT,1,ISTAT)
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,IERR)
C
C IF ILEVEL IS 2 OR MORE, PRINT THE RESULT FOR THE USER
C
      IF(ILEVEL.GE.2) THEN
	   WRITE(PRBUF,10) NPT(1),AV(1)
   10	   FORMAT(' AVERAGE OF ',I8,' PIXEL(S) IS ',SS,G13.6)
	   CALL LBGONE(PRBUF(13:))
         CALL WRUSER(PRBUF,ISTAT)
         WRITE(PRBUF,11)STD(1)
   11    FORMAT(' STD DEV = ',SS,G13.6)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C RELEASE IMAGE FRAME AND RETURN
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



