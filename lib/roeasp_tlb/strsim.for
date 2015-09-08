      SUBROUTINE STRSIM (STNDRD,IXEXT,IYEXT,PIXSIZ,APERTR,EXPOSE,SKYBRI,
     :                   STMULT,BETA,R,ATMABS,EFFIC,
     :                   THRESH,MAXSTR,MBIN,NBIN,
     :                   MGLMFN,NLMFN,GFLAG,GMXBIN,GBIN,GLMFN,
     :                   GNLMFN,NGLOB,DIST,GABSOR,RCORE,RTIDAL,
     :                   UNIT3,GXCORD,GYCORD,GAPMAG,
     :                   NSTAR,XCORD,YCORD,APMAG,IMAGE,STATUS)
C+
C      STRSIM.
C
C      Subroutine to simulate a star field given an apparent 
C     luminosity function, plus appropriate parameters for
C     the observed image being simulated; exposure time,
C     sky brightness etc.
C
C  Given;
C  STNDRD  (I)  Flag to determine whether or not a standard field
C               is produced;
C                  = 0 - Standard field.
C                  = 1 - Random field.
C  IXEXT   (I)  X size of array to hold simulated field.
C  IYEXT   (I)  Y  "   "    "   "   "      "        "  .
C  PIXSIZ  (R)  Size of a pixel side (arcseconds).
C  APERTR  (R)  Telescope aperture diameter (cm).
C  EXPOSE  (R)  Exposure time (seconds).
C  SKYBRI  (R)  Sky background brightness (magnitudes/sq.arcsec).
C  STMULT  (R)  Galactic latitude/star density factor 
C               (=1.0 for a field at the poles).
C  BETA    (R)  Moffat's quantity beta.
C  R       (R)  Moffat's quantity r (arcsec).
C  ATMABS  (R)  Total atmospheric absorption.
C  EFFIC   (R)  Fraction of light transmiited through, and detected
C               by, the combination of telescope and detector.
C  THRESH  (R)  Threshold for plotting images generated (magnitudes).
C  MAXSTR  (I)  Max. permitted no. of stars to be generated.
C               (=size of arrays XCORD, YCORD & APMAG, below).
C  MBIN    (I)  Max. permitted no. of bins in apparent luminosity
C               function (=size of arrays MGLMFN & NLMFN).
C  NBIN    (I)  No. of bins in apparent luminosity function.
C  MGLMFN  (RA) Magnitude steps for apparent luminosity function.
C  NLMFN   (RA) No. of stars/sq.degree at each magnitude step.
C  GFLAG   (L)  Flag indicating whether globular cluster is to
C               placed in field.
C  GMXBIN  (I)  Max. permitted no. of bins in the cluster luninosity
C               function.
C  GBIN    (I)  Actual no. of bins in the cluster luminosity function.
C  GLMFN   (RA) Magnitude bins for the cluster luminosity function.
C  GNLMFN  (RA) Number bins for the cluster luminosity function.
C  NGLOB   (I)  No. fo stars to be put in the globular cluster.
C  DIST    (R)  distance to the globular cluster (pc).
C  GABSOR  (R)  Total absorption along the l-o-s to the globular
C               cluster.
C  RCORE   (R)  King's core radius for the cluster (pc).
C  RTIDAL  (R)  King's tidal radius for the cluster (pc).
C  UNIT3   (I)  Fortran unit no. for output to print file.
C
C  Used;
C  GXCORD  (RA) 
C  GYCORD  (RA)
C  GAPMAC  (RA)
C
C  Returned;
C  NSTAR   (I)  No. of star images generated.
C  XCORD   (RA) X coords. of star images generated.
C  YCORD   (RA) Y   "   . "   "     "       "     .
C  APMAG   (RA) Apparent magnitudes of star images generated.
C  IMAGE   (RA) Array containing simulated star field.
C  STATUS  (I)  Max(input status, internal status). The internal
C               status = 0 if the output listing is printed correctly,
C               and is non-zero if there is an error in printing the
C               file.
C
C  Subroutines called;
C   SIMUL:-    STRFLD, STRPLT, BACKGR, MOFFAT, ATMOS, INSTEF,
C              POISON.
C   Graphics:- GDEVI1, STOPLOT.
C   NAG:-      G05CCF.
C   System:-   DATE, TIME.
C
C  Structure:-
C   Generate positions and apparent magnitudes of stars.
C   produce plot of star field on Versatec.
C   Set the image array to zero.
C   compute the no. of photons coresponding to the background level.
C   generate a background.
C   add stellar (Moffat) profiles to the background.
C   Put Poisson noise on the image.
C   Print out the field parameters.
C   Print out the star catalogue.
C
C   A C Davenhall./ROE/                                     4/3/82.
C   A C Davenhall./ROE/                                    30/8/82.
C   A C Davenhall./ROE/  {Modified}                        25/12/82.
C   A C Davenhall./ROE/  {   "    }                         22/4/83.
C   based on routines by R. S. Stobie./ROE/.
C-
      INTEGER STNDRD,IXEXT,IYEXT,MAXSTR,NSTAR,UNIT3,STATUS
      INTEGER NBIN,MBIN,GMXBIN,GBIN,NGLOB
      REAL PIXSIZ,APERTR,EXPOSE,SKYBRI,STMULT,BETA,R,THRESH,
     :     ATMABS,EFFIC,DIST,GABSOR,RCORE,RTIDAL
      REAL MGLMFN(MBIN),NLMFN(MBIN)
      REAL GLMFN(GMXBIN),GNLMFN(GMXBIN)
      REAL XCORD(MAXSTR),YCORD(MAXSTR),APMAG(MAXSTR)
      REAL GXCORD(MAXSTR),GYCORD(MAXSTR),GAPMAG(MAXSTR)
      REAL IMAGE(IXEXT,IYEXT)
      LOGICAL GFLAG
C
C
      REAL XSIZE,YSIZE,AREA,PI,B0
      REAL XMIN,XMAX,YMIN,YMAX
      REAL BCONST,RPIX,RTPIX
      INTEGER ISTAT,PAGE,LNCNT
      INTEGER GSTAT,FSTAT,NFIELD,IOSTAT,SFLAG
      INTEGER FAISTR
      REAL TOTPHOT(1000),LIMPHOT(1000),MAGAPP(1000)
      REAL SKYTHR
      LOGICAL FLAG
      CHARACTER DATEB*9,TIMEB*8
      PARAMETER (PI=3.14159265E0)
      PARAMETER (SKYTHR=1.0E-2)
C
      REAL SKYARC,SKYPIX,ABSRAT,TOPATS
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
C      No. of photons for a 0th magnitude star per sq.cm, per second,
C      in the B band.
C
      PARAMETER (B0=1.4342E6)
C
C      If a random field it to be produced randomise the seed
C      used for the random number generators.
C
      IF (STNDRD.EQ.1) CALL G05CCF
C
C      Generate positons and apparent magnitudes for field stars.
C
      XSIZE=FLOAT(IXEXT)
      YSIZE=FLOAT(IYEXT)
      AREA=(PIXSIZ*XSIZE/3.6E3)*(PIXSIZ*YSIZE/3.6E3)
      CALL STRFLD (MGLMFN,NLMFN,NBIN,MBIN,AREA,STMULT,MAXSTR,
     :             XSIZE,YSIZE,NFIELD,XCORD,YCORD,APMAG,FSTAT)
C
C      If required attempt to generate the globular cluster stars.
C
      IF (GFLAG.AND.ISTAT.EQ.0) THEN
        CALL KING (GLMFN,GNLMFN,GBIN,GMXBIN,DIST,GABSOR,
     :             XSIZE,YSIZE,RCORE,RTIDAL,PIXSIZ,MAXSTR,
     :             NGLOB,GXCORD,GYCORD,GAPMAG,RTPIX,GSTAT)
C
C      Attempt to append the globular cluster stars to the
C      list of field stars previously generated.
C
        IF (NFIELD+NGLOB.LE.MAXSTR) THEN
          NSTAR=NFIELD+NGLOB
          FLAG=.FALSE.
        ELSE
          NSTAR=MAXSTR
          FLAG=.TRUE.
        END IF
        II=0
        DO I=NFIELD+1,NSTAR
          II=II+1
          XCORD(I)=GXCORD(II)
          YCORD(I)=GYCORD(II)
          APMAG(I)=GAPMAG(II)
        END DO
      ELSE
        NSTAR=NFIELD
      END IF
C
C      Produce plot of star field on the Versatec.
C
      CALL GDEVI1 (3,.TRUE.)
      XMIN=0.0E0
      YMIN=0.0E0
      XMAX=FLOAT(IXEXT)
      YMAX=FLOAT(IYEXT)
      CALL STRPLT (MAXSTR,NSTAR,XCORD,YCORD,APMAG,THRESH,
     :             XMIN,XMAX,YMIN,YMAX)
      CALL STOPLOT
C
C      initialise the array that will contain the star field to
C      zero.
C
      DO J=1,IYEXT
        DO I=1,IXEXT
          IMAGE(I,J)=0.0E0
        END DO
      END DO
C
C      Compute the no. of photons seen from a 0th magnitude
C      for this exposure time and aperture.
C
      BCONST=B0*EXPOSE*PI*((APERTR/2.0E0)**2)
C
C      Compute the number of photons per square arcsec corresponding
C      to the input sky background.
C
      SKYARC=BCONST*(1.0E1**(-SKYBRI/POGSON))
      TOPATS=SKYARC
C
C      Scale the number of photons per square arcsec. to give the
C      number per pixel.
C
      SKYPIX=SKYARC*PIXSIZ*PIXSIZ
C
C      Add the background per pixel onto all the pixels in
C      the image frame.
C
      CALL CADD1 (IXEXT,IYEXT,IMAGE,SKYPIX,IXEXT,IYEXT,IMAGE)
C
C      Add stellar profiles to the background.
C
      CALL MOFFAT (MAXSTR,NSTAR,XCORD,YCORD,APMAG,BETA,R,PIXSIZ,
     :             SKYTHR,SKYBRI,BCONST,IXEXT,IYEXT,IMAGE,
     :             TOTPHOT,LIMPHOT,MAGAPP,FAISTR)
C
C      Dim the image frame, lists of total and thresholded luminosities
C      and no. of background photons to allow for atmospheric 
C      absorption.
C
      CALL ATMOS (ATMABS,IXEXT,IYEXT,IMAGE)
      CALL ATMOS (ATMABS,1,NSTAR,TOTPHOT)
      CALL ATMOS (ATMABS,1,NSTAR,LIMPHOT)
      CALL ATMOS (ATMABS,1,1,SKYARC)
      CALL ATMOS (ATMABS,1,1,SKYPIX)
C
C      Dim the image frame, lists of total and thresholded luminosities
C      and no. of background photons to allow for the combined light
C      losses in the telescope and detector.
C
      CALL INSTEF (EFFIC,IXEXT,IYEXT,IMAGE)
      CALL INSTEF (EFFIC,1,NSTAR,TOTPHOT)
      CALL INSTEF (EFFIC,1,NSTAR,LIMPHOT)
      CALL INSTEF (EFFIC,1,1,SKYARC)
      CALL INSTEF (EFFIC,1,1,SKYPIX)
      ABSRAT=TOPATS/SKYARC
C
C      Put noise on the image frame.
C
      CALL POISON (IXEXT,IYEXT,IMAGE)
C
C      print out the field and star parameters.
C
      OPEN(UNIT=UNIT3,FILE='STARSIM.TMP',STATUS='NEW',
     :     IOSTAT=IOSTAT)
      IF (IOSTAT.EQ.0) THEN
C
C      Obtain the time and date from the system and write the title
C      line.
C
        CALL TIME (TIMEB)
        CALL DATE (DATEB)
        WRITE(UNIT3,2004) TIMEB,DATEB
 2004   FORMAT(1H1//1X,A8,1X,A9,36X,'SIMULATED STAR FIELD.',43X,
     :         'Page 1.',5(/))
C
C      Output error messages if any have been generated.
C
        IF (FSTAT.EQ.1) WRITE(UNIT3,3000)
 3000   FORMAT(/5X,'***ERROR***; Attempt to generate too many field ',
     :   'stars.',/)
        IF (FSTAT.EQ.2) WRITE(UNIT3,3001) MBIN
 3001   FORMAT(/5X,'***ERROR***; Too many points in field luminosity ',
     :   'function.'//18X,'Maximum permitted = ',I3/)
        IF (GFLAG) THEN
          IF (GSTAT.EQ.1) WRITE(UNIT3,3002) GMXBIN
 3002     FORMAT(/5X,'***ERROR***; Too many points in the globular ',
     :     'cluster luminosity function.'//18X,
     :     'Maximum permitted = ',I3/)
          IF (GSTAT.EQ.2) WRITE(UNIT3,3004)
 3004     FORMAT(/5X,'***ERROR***; Attempt to generate too many ',
     :     'cluster stars.'/)
          IF (GSTAT.EQ.3) WRITE(UNIT3,3003)
 3003     FORMAT(/5X,'*WARNING*; Some globular cluster stars fell ',
     :     'outside the image frame.'/)
        END IF
        IF (FLAG) WRITE(UNIT3,3004)
C
C     Output results.
C
        WRITE(UNIT3,2005) IXEXT,IYEXT,PIXSIZ,APERTR,EXPOSE,
     :                    SKYBRI,SKYARC,SKYPIX,STMULT
 2005 FORMAT(/45X,'Parameters for the star field generated:-',4(/),
     :   35X,'Image size (pixels);   X direction = ',I3,3X,
     :  'Y direction = 'I3//35X,
     :  'Size of each (square) pixel side = ',0PF12.4,2X,
     :  'arcseconds.'//35X,
     :  'Telescope aperture = ',0PF9.1,2X,'cm.'//35X,
     :  'Exposure time = ',0PF10.2,2X,'seconds.'//35X,
     :  'Sky brightness = ',0PF10.2,2X,'mag./sq.arcsec.'//35X,
     :  'Corresponding number of detected photons per sq. arcsec. = ',
     :   1PE12.3//35X,
     :  'Corresponding number of detected photons per pixel = ',
     :   1PE12.3//35X,
     :  'Galactic latitude dependant star density factor = ',
     :   0PF10.2//)
        WRITE(UNIT3,2006) BETA,R
 2006 FORMAT(35X,'Moffat''s parameters for the stellar profiles;'//
     :  35X,'Beta = ',0PF10.2,5X,'R = ',0PF10.2,2X,'arcseconds.'//)
        WRITE(UNIT3,2007) THRESH
 2007 FORMAT(35X,'Magnitude threshold for plotting star images =',
     :  0PF10.2///)
        WRITE(UNIT3,2008) ATMABS,EFFIC,ABSRAT
 2008 FORMAT(/35X,'Total absorption in the (terrestrial) ',
     :         'atmosphere = ',0PF9.3,' magnitudes.'//35X,
     :         'Fraction of incident light detected = ',F6.3//35X,
     :         'Ratio of number of photons at top of atmosphere to ',
     :         'number detected = ',1PE12.3//)
        IF (GFLAG) WRITE(UNIT3,2009) NGLOB,DIST,GABSOR,RCORE,RTIDAL
 2009   FORMAT(//35X,'Parameters for the Globular Cluster;'//35X,
     :   'Number of stars in the cluster = ',I4//35X,
     :   'Distance to the cluster = ',1PE12.4,' pc',//35X,
     :   'Total interstellar absorption to the cluster = ',0PF8.3,
     :   ' magnitudes.',//35X,
     :   'Core radius = ',1PE12.4,' pc',5X,'Tidal radius = ',1PE12.4,
     :   ' pc'//)
        IF (FAISTR.GT.0) WRITE(UNIT3,2011) FAISTR
 2011   FORMAT(//35X,'Number of subthreshold stars excluded from ',
     :         'the image frame = ',I4//)
C
        PAGE=2
        WRITE(UNIT3,2000) PAGE
 2000 FORMAT(1H1/46X,'Parameters of Star Images in the Field.',35X,
     :  'Page',I2,'.'///
     : 10X,'X Position',5X,'Y Position',5X,'True apparent',
     : 5X,'Total number of',5X,'Photons inside',5X,'Apparent magnitude'/
     : 42X,'magnitude',11X,'Photons',10X,'Sky boundary',
     : 7X,'applied to image'//)
        LNCNT=0
        IF (GFLAG) THEN 
          WRITE(UNIT3,2010)
 2010     FORMAT(/54X,'- - - Field Stars - - -'/)
          LNCNT=LNCNT+3
        END IF
        DO I=1,NSTAR
          LNCNT=LNCNT+1
          IF (LNCNT.GT.50) THEN
            LNCNT=0
            PAGE=PAGE+1
            WRITE(UNIT3,2000) PAGE
          END IF
          IF (GFLAG.AND.I.EQ.NFIELD+1) THEN
            IF (LNCNT.GT.45) THEN
              LNCNT=3
              PAGE=PAGE+1
              WRITE(UNIT3,2000) PAGE
            ELSE
              LNCNT=LNCNT+3
            END IF
            WRITE(UNIT3,2013)
 2013       FORMAT(/49X,'- - - Globular Cluster Stars - - -'/)
          END IF
          IF (LIMPHOT(I).GT.0.0E0) THEN
            IF (APMAG(I).LT.THRESH) THEN
              WRITE(UNIT3,2002) I,XCORD(I),YCORD(I),APMAG(I),
     :                          TOTPHOT(I),LIMPHOT(I),MAGAPP(I)
 2002         FORMAT(3X,I4,2X,0PF8.1,7X,F8.1,10X,F7.2,9X,1PE12.3,7X,
     :               1PE12.3,12X,0PF7.2,9X,'####')
            ELSE
              WRITE(UNIT3,2003) I,XCORD(I),YCORD(I),APMAG(I),TOTPHOT(I),
     :                            LIMPHOT(I),MAGAPP(I)
 2003 FORMAT(3X,I4,2X,0PF8.1,7X,F8.1,10X,F7.2,9X,1PE12.3,7X,1PE12.3
     :         ,12X,0PF7.2)
            END IF
          ELSE
            WRITE(UNIT3,2012) I,XCORD(I),YCORD(I),APMAG(I),
     :                        TOTPHOT(I),LIMPHOT(I)
 2012       FORMAT(3X,I4,2X,0PF8.1,7X,F8.1,10X,F7.2,9X,1PE12.3,7X,
     :             1PE12.3,7X,'Not applied to image.')
          END IF
        END DO
        WRITE(UNIT3,2001)
 2001 FORMAT(//41X,'#### - Star is brighter than plotting threshold.'
     :         /////)
        CLOSE(UNIT=UNIT3,DISPOSE='PRINT/DELETE')
      END IF
      STATUS=MAX(IOSTAT,STATUS)
      END
