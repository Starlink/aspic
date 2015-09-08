C
C++++++++++++++++++++++++++++++++++++++++++++++
C
C
C        *******************
C        *                 *
C        * PROGRAM PUTSTAR *
C        *                 *
C        *******************
C
C
C
C
C      FUNCTION
C
C           To make an image with stars or galaxies in it.
C
C           If stars are put in they all have the same profile,
C           but if galaxies are put in each one can have a different
C           radius.
C
C           An image is set up with a flat level background and
C           with objects placed at positions and heights, and
C           for galaxies, radii, given by reading from an input
C           XYlist. The heights are calculated from the
C           input magnitudes as H*(10**((30-m)/2.5))/(R*R), where
C           H is input at the keyboard and R is the object radius.
C           The objects all have the same profiles, defined by
C           specified parameters given below.
C
C           There is an option for correctly adjusting for
C           the effect of the finite bin size used as 'collectors'
C           for the rain of photons from the objects.
C
C           An option exists for mimicking the effect of using a
C           film type of detector, where the bins act as logarith-
C           metic averagers when the film is measured by a micro-
C           densitometer.
C
C           There is an option for adding true Poisson noise both to
C           the objects and to the sky.
C
C           There is an option for
C           storing the positions and actually applied magnitudes
C           (which will not be quite the same as those input, and
C           if there is Poisson noise will not be at all the same) as
C           an output XYlist.
C
C           The final image is made as an I*2 image, that is the
C           data points are integers in the range 0 - 32767.
C
C           The total overall aim has been to make images as like
C           as possible to those obtained in real life.
C
C           The object profiles are Lorentz stars:-
C
C                      I =     1
C                          ------------
C                                 P(1+d2)
C                           1 + d1
C
C
C                                     d1=sqrt((x.x/rx1.rx1)+(y.y/ry1.ry1))
C                                     d2=sqrt((x.x/rx2.rx2)+(y.y/ry2.ry2))
C
C             here the radius for applying the star heights is (rx1+ry1)/2
C
C
C
C
C            or Moffat stars:-
C
C                      V =     1
C                          ------------
C                                  P
C                           (1+d.d)
C                                                    d = mod(r)/radius
C
C
C
C
C            or Galaxy profile:-
C
C
C                     V =       1
C                          -----------
C                                  1/4
C                            1 + d
C
C                                            where  d = mod(r)/radius
C
C
C
C
C
C            WARNING    Galaxies option not fully working properly yet
C            ---------------------------------------------------------
C
C
C
C     USER PARAMETERS
C
C
C            XSIZE     100                  The X size of the image
C
C            YSIZE             XSIZE        The Y size of the image
C
C            IMAGE                          The name of the image
C
C            TITLE    Output from PUTSTAR   The title to be attached to
C                                           image.
C
C            XYLIST                         The XYlist of positions and
C                                           maybe magnitudes.
C
C            NBOX              30           The size of the box around
C                                           each object position to be filled
C                                           with object flux
C
C            TYPE              LORENTZ      The type of object profile
C                                           (Lorentz,Moffat,Galaxy) to be
C                                           used
C
C            BLUR              YES          Flag for blurring Galaxies by
C                                           by 'seeing'.
C                                           Choices are YES,NO
C
C            SEERAD            3.0          Radius of 'seeing' to use, if
C                                           blurring is to be done.
C                                           The galaxy radius is made =
C                                           sqrt(Gr.Gr+Sr.Sr). So a very
C                                           small galaxy still has a galaxy
C                                           profile, whereas it should have
C                                           a star profile.
C
C            RADIUS            3.0          Moffat radius
C
C            POWER             2.0          Moffat power
C
C            RX                3.0          Lorentz X radius
C
C            RY                Rx           Lorentz Y radius
C
C            P                 2.0          Lorentz power
C
C            PRX               7*RX         Lorentz power X radius
C
C            PRY               7*RY         Lorentz power Y radius
C
C
C            HFACT             1.0          The value the object heights
C                                           are to be multiplied by
C                                           before being used.
C
C            BIN          NO                Flag for taking profile as
C                                           true and binning it into used
C                                           pixels.
C
C            DETECTOR      ELECTRONIC       Flag for detector type if
C                                           binning.
C                                           (ELECTRONIC or FILM)
C
C            DFILM                          If film detector, number in image
C                                           corresponding to Density=1
C
C            RANDOM       NO                Flag for Poissonian noise
C                                           applied (YES,NO)
C
C            RANFACT      1.0               If Poissonian noise applied,
C                                           factor to multiply noise by
C                                           if counts are not single counts
C                                           but averaged or manipulated in
C                                           some way
C
C            NRAN      12345                If Poissonian noise applied,
C                                           seed number to start off the
C                                           random number generator
C
C            SKY       100                  The background level to be added
C
C            SAVE         NO                Flag for saving posns and applied
C                                           magnitudes.
C
C            OUTPUT                         The optional Output XY,mag list
C
C            TITLEF  'Output from PUTSTAR'  The title of any output XY list
C
C
C
C     NORMALLY DEFAULTED PARAMETERS:-
C
C             PARFIX      YES               Fixed profile. (No alternative)
C
C             VOLCAL      NO                Profile volume calculation
C                                           (No alternative)
C
C
C
C
C  WRITTEN BY :-
C        A.J. PENNY         RGO                      83-7-29
C-----------------------------------------------------------------
C



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         PROGRAM PUTSTAR
C
C
C CALLS
C             EDRS PACKAGE:
C                     GT2DIW,PTDSCR,GETPAR,IMGSET,
C                     GTXYLW,GETCMD,GTXYLR
C             STARLINK:
C                     FRDATA,RDKEYC,CNPAR,WRUSER
C             THIS FILE:
C                     IMGRVL,PLACE,PRAN,EXTLSA
C
C NOTES
C             USES VAX %VAL FACILITY
C
C -------------------------------------------------------------------
C
C
C
      PROGRAM PUTSTAR
C
C
C
      REAL TEMP(1,1),PROF(5)
      CHARACTER CVAL*1
      CHARACTER TITLE*30
      CHARACTER TEXT*72
      LOGICAL VALID,PARFIX,VOLCAL
C
C  Set continuation flag
C
      VALID = .TRUE.
C
C  Get array size
C
      IF (VALID) THEN
         NPIX = 100
         CALL GETPAR('XSIZE','INTEGER',1,1.0,4096.0,.TRUE.,
     +               NPIX,RVAL,IERRA)
         NLINES = NPIX
         CALL GETPAR('YSIZE','INTEGER',1,1.0,4096.0,.TRUE.,
     +               NLINES,RVAL,IERRB)
         IF (IERRA.NE.0.OR.IERRB.NE.0) VALID = .FALSE.
      ENDIF
C
C  Get Output image
C
      CALL GT2DIW('IMAGE',102,.FALSE.,NPIX,NLINES,IPIM,IERROU)
      IF (IERROU.NE.0) THEN
         VALID = .FALSE.
         CALL WRUSER('CANT GET IMAGE',ISTAT)
      ELSE
         TITLE = 'Output from PUTSTAR'
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL PTDSCR('IMAGE','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +              IERR)
         INVAL = -32767
         SCALE = 1.0
         ZERO = 0.0
         CALL PTDSCR('IMAGE','INVAL','INTEGER',INVAL,RVAL,CVAL,IERR)
         CALL PTDSCR('IMAGE','BSCALE','REAL',IVAL,SCALE,CVAL,IERR)
         CALL PTDSCR('IMAGE','BZERO','REAL',IVAL,ZERO,CVAL,IERR)
      ENDIF
C
C Get the input file for  positions and magnitudes and perhaps Galaxy radii
C
      IF (VALID) THEN
         CALL GTXYLR('XYLIST',.TRUE.,NPAR,NSTAR,IPIN,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C  Get object box size
C
      IF (VALID) THEN
         APIX = REAL(MAX(NPIX,NLINES))
         NBOX = 30
         CALL GETPAR('NBOX','INTEGER',1,1.0,APIX,.TRUE.,
     +               NBOX,RVAL,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C  Get object type
C
      IF (VALID) THEN
    1    KTYPE = 1
         CALL GETCMD('TYPE','LORENTZ,MOFFAT,GALAXY,HELP,?.',.TRUE.,
     +               KTYPE,TEXT,KTEXT,ISTAT)
         IF (KTYPE.EQ.4.OR.KTYPE.EQ.5) THEN
            CALL WRUSER('Choices are :-',ISTAT)
            CALL WRUSER('LORENTZ      Lorentz stars',ISTAT)
            CALL WRUSER('MOFFAT       Moffat stars',ISTAT)
            CALL WRUSER('GALAXY       Hubble galaxies',ISTAT)
            CALL CNPAR('TYPE',ISTAT)
            GO TO 1
         ENDIF
         IF (KTYPE.EQ.3.AND.NPAR.LT.3) THEN
            CALL WRUSER('No radii in XYlist',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  If galaxy, get blurring if any
C
      IF (VALID.AND.KTYPE.EQ.3) THEN
    2    KBLUR = 1
         CALL GETCMD('BLUR','YES,NO,HELP,?.',1,KBLUR,TEXT,KTEXT,
     +               IERR)
         IF (KBLUR.EQ.3.OR.KBLUR.EQ.4) THEN
            CALL WRUSER('Choices are:-',ISTAT)
            CALL WRUSER('YES        Apply bluring to galaxies',ISTAT)
            CALL WRUSER('NO         No blurring to apply',ISTAT)
            CALL CNPAR('BLUR',ISTAT)
            GO TO 2
         ENDIF
         IF (KBLUR.EQ.1) THEN
            SEERAD = 3.0
            CALL GETPAR('SEERAD','REAL',1,0.001,1000.0,.TRUE.,
     +                  NVAL,SEERAD,IERR)
         ENDIF
      ENDIF
C
C  If Moffat star get radius and fall-off power
C
      IF (VALID.AND.KTYPE.EQ.2) THEN
         RADIUS = 3.0
         CALL GETPAR('RADIUS','REAL',1,0.001,APIX,.TRUE.,
     +               NVAL,RADIUS,IERR)
         POWER = 1.0
         CALL GETPAR('POWER','REAL',1,0.001,1000.0,.TRUE.,NVAL,
     +                POWER,IERR)
      ENDIF
C
C  If Lorentz star get profile
C
      IF (VALID.AND.KTYPE.EQ.1) THEN
         CALL GTPROF(PROF,PARFIX,VOLCAL,IERR)
         RADIUS = (PROF(1)+PROF(2))/2.0
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C  Get Height factor to multiply object heights by
C
      IF (VALID) THEN
         HFACT = 1.0
         CALL GETPAR('HFACT','REAL',1,1.0E-10,1.0E10,.TRUE.,NVAL,
     +               HFACT,IERR)
      ENDIF
C
C  Get if Binning
C
      IF (VALID) THEN
    6    KBIN = 1
         CALL GETCMD('BIN','NO,YES,HELP,?.',.TRUE.,KBIN,
     +               TEXT,KTEXT,ISTAT)
         IF (KBIN.EQ.3.OR.KBIN.EQ.4) THEN
            CALL WRUSER('Choices are :-',ISTAT)
            CALL WRUSER('NO   Take profile as given',ISTAT)
            CALL WRUSER('YES   Bin profile into pixel',ISTAT)
            CALL CNPAR('BIN',ISTAT)
            GO TO 6
         ENDIF
      ENDIF
C
C  Get detector type
C
      IF (VALID.AND.KBIN.EQ.2) THEN
    3    KFILM = 2
         CALL GETCMD('DETECTOR','FILM,ELECTRONIC,HELP,?.',.TRUE.,
     +               KFILM,TEXT,KTEXT,ISTAT)
         IF (KFILM.EQ.3.OR.KFILM.EQ.4) THEN
            CALL WRUSER('Choices are :-',ISTAT)
            CALL WRUSER('ELECTRONIC    Averaging in pixel',ISTAT)
            CALL WRUSER('FILM          Density averaging',ISTAT)
            CALL CNPAR('DETECTOR',ISTAT)
            GO TO 3
         ENDIF
      ENDIF
C
C  If film, get image value for density=1
C
      IF (VALID.AND.KFILM.EQ.1.AND.KBIN.EQ.2) THEN
         DFILM = 754.0
         CALL GETPAR('DFILM','REAL',1,0.001,100000.0,.TRUE.,
     +               NVAL,DFILM,IERR)
      ENDIF
C
C  Get if image is to be Poisson noisy
C
      IF (VALID) THEN
    4    KRAN = 1
         CALL GETCMD('RANDOM','NO,YES,HELP,?.',.TRUE.,KRAN,
     +               TEXT,KTEXT,ISTAT)
         IF (KRAN.EQ.3.OR.KRAN.EQ.4) THEN
            CALL WRUSER('Choices are :-',ISTAT)
            CALL WRUSER('NO    Apply no noise',ISTAT)
            CALL WRUSER('YES   Apply Poisson noise',ISTAT)
            CALL CNPAR('RANDOM',ISTAT)
            GO TO 4
         ENDIF
         IF (KRAN.EQ.2) THEN
            RANF = 1.0
            CALL GETPAR('RANFACT','REAL',1,1.0E-8,1.0E8,.TRUE.,NVAL,
     +                  RANF,IERR)
            IF (IERR.NE.0) THEN
               VALID = .FALSE.
            ELSE
               NRAN = 12345
               CALL GETPAR('NRAN','INTEGER',1,-1.0E8,1.0E8,.TRUE.,
     +                     NRAN,RVAL,IERR)
               IF (IERR.NE.0) VALID = .FALSE.
            ENDIF
         ENDIF
      ENDIF
C
C Get the sky level
C
      IF (VALID) THEN
         SKY = 100.0
         CALL GETPAR('SKY','REAL',1,0.0,32767.0,.TRUE.,NVAL,
     +               SKY,IERR)
      ENDIF
C
C  Open a list for storing the Xs,Ys and Magnitudes (in XYformat)
C
      IF (VALID) THEN
    5    KSAVE = 1
         CALL GETCMD('SAVE','NO,YES,HELP,?.',.TRUE.,KSAVE,
     +               TEXT,KTEXT,ISTAT)
         IF (KSAVE.EQ.3.OR.KSAVE.EQ.4) THEN
            CALL WRUSER('Choices are :-',ISTAT)
            CALL WRUSER('NO       Do not save applied mags',ISTAT)
            CALL WRUSER('YES     Save applied mags in a file',ISTAT)
            CALL CNPAR('SAVE',ISTAT)
            GO TO 5
         ENDIF
         IF (KSAVE.EQ.2) THEN
            CALL GTXYLW('OUTPUT',.FALSE.,NPAR,NSTAR,IPOUT,ISTAT)
            CALL PTDSCR('OUTPUT','NITEM','INTEGER',NPAR,RVAL,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',NSTAR,RVAL,
     +                  CVAL,IERR)
            TITLE='X,Y,Mag file from PUTSTAR'
            CALL RDKEYC('TITLEF',.TRUE.,1,TITLE,I,IERR)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',
     +                  IVAL,RVAL,TITLE,IERR)
C
C Copy the input XYlist to the output XYlist, so the magnitudes
C can be overwritten later.
C
            CALL EXTLSA(%VAL(IPIN),NPAR,NSTAR,1,NSTAR,1,NPAR,
     +                  %VAL(IPOUT),NPAR,NSTAR,1,1)
         ENDIF
      ENDIF
C
C
C -------------------------------------------------------------
C
C  Put the sky level in
C
      IF (VALID) THEN
         IF(KRAN.EQ.2) THEN
            CALL IMGRVL(%VAL(IPIM),NPIX,NLINES,SKY,RANF,NRAN)
         ELSE
            NVAL = SKY
            CALL IMGSET(%VAL(IPIM),NPIX,NLINES,NVAL)
         ENDIF
C
C Now put the objects in the image and store the actual magnitudes
C of the noised objects.
C
         DO K = 1,NSTAR
            CALL EXTLSA(%VAL(IPIN),NPAR,NSTAR,K,K,6,6,TEMP,1,1,1,1)
            XPOS = TEMP(1,1)
            CALL EXTLSA(%VAL(IPIN),NPAR,NSTAR,K,K,7,7,TEMP,1,1,1,1)
            YPOS = TEMP(1,1)
            CALL EXTLSA(%VAL(IPIN),NPAR,NSTAR,K,K,8,8,TEMP,1,1,1,1)
            HEIGHT = 10.0**((30.0-TEMP(1,1))/2.5)
            IF (KTYPE.EQ.3) THEN
               CALL EXTLSA(%VAL(IPIN),NPAR,NSTAR,K,K,9,9,TEMP,1,1,1,1)
               RADIUS = TEMP(1,1)
               IF (KBLUR.EQ.1) THEN
                  RADIUS = SQRT(RADIUS**2.0+SEERAD**2.0)
               ENDIF
            ENDIF
            HEIGHT = HEIGHT*HFACT/(RADIUS*RADIUS)
            CALL PLACE(%VAL(IPIM),NPIX,NLINES,XPOS,YPOS,NBOX,
     +                 KTYPE,HEIGHT,RADIUS,POWER,KFILM,DFILM,KRAN,
     +                 RANF,NRAN,FLUX,KBIN,PROF)
            WRITE(TEXT,900)FLUX
  900       FORMAT('  FLUX ',F12.4)
            CALL WRUSER(TEXT,IERR)
            IF (KSAVE.EQ.2) THEN
               IF (FLUX.LT.1.0E-10) FLUX = 1.0E-10
               TEMP(1,1) = 30.0 - 2.5*ALOG10(FLUX)
               CALL EXTLSA(TEMP,1,1,1,1,1,1,%VAL(IPOUT),NPAR,NSTAR,
     +                     K,8)
            ENDIF
         ENDDO
      ENDIF
C
C -----------------------------------------------------------------
C
C
C FREE DATA AREAS AND EXIT
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END
C
C
C



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          SUBROUTINE PLACE
C
C
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C PURPOSE
C          TO PUT A STAR INTO AN ARRAY
C METHOD
C          ADDS A STAR TO A BOX IN THE ARRAY
C
C ARGUMENTS
C
C (OUT)
C          IA
C          INTEGER*2(NPIX,NLINES)
C                  IMAGE ARRAY
C          FLUX
C          REAL
C                  ACTUAL STAR FLUX ADDED
C (IN)
C          NPIX,NLINES
C          INTEGER
C                  DIMENSIONS OF IA
C          XA,YA
C          REAL
C                  POSITION OF STAR CENTRE
C          NBOX
C          INTEGER
C                  SIDE OF BOX OVERWRITTEN
C          KTYPE
C          INTEGER
C                  TYPE OF STAR PROFILE
C          H,RADIUS,POWER
C          REAL
C                  HEIGHT AND RADIUS AND FALLOFF FACTER OF STAR
C          KFILM
C                  FLAG FOR DETECTOR TYPE
C          DFILM
C          REAL
C                  PIXEL VALUE FOR DENSITY = 1
C          KRAN
C          INTEGER
C                  FLAG FOR POISSON NOISE
C          NRAN
C          INTEGER
C                  SEED VALUE FOR RANDOMISING CALCS
C     KBIN   Int     Flag for binning (no=1,yes=2)
C     PROF   Real(5)  Lorentz profile
C
C CALLS
C          PRAN
C
C NOTES
C          USES INTEGER*2 ARRAY
C
C WRITTEN BY
C          A.J. PENNY
C
C ------------------------------------------------------
C
C
C
      SUBROUTINE PLACE(IA,NPIX,NLINES,XA,YA,NBOX,KTYPE,H,RADIUS,POWER,
     +                 KFILM,DFILM,KRAN,RANF,NRAN,FLUX,KBIN,PROF)
C
C
C
      INTEGER*2 IA(NPIX,NLINES)
      REAL PROF(5)
C
C
C
      IF (KTYPE.EQ.1) THEN
         RXTA = PROF(1)*PROF(1)
         RYTA = PROF(2)*PROF(2)
         RXTB = PROF(4)*PROF(4)
         RYTB = PROF(5)*PROF(5)
         PLOR = PROF(3)/2.0
      ENDIF
C
C  Calc spacing for dividing up profile
C
      IF (KBIN.EQ.2) THEN
         NB = INT(10.0/RADIUS + 0.9)
         ANB = REAL(NB)
         ABOX = ANB*ANB
      ENDIF
C
C  Calc the accurate profile detected by the pixels
C
      DMFILM = -1.0*DFILM
      FLUX = 0.0
C
      NXS = INT(XA) - NBOX/2
      NXE = NXS + NBOX - 1
      NYS = INT(YA) - NBOX/2
      NYE = NYS + NBOX - 1
      IF(NXS.LE.NPIX.AND.NXE.GE.1.AND.NYS.LE.NLINES.AND.NYE.GE.1) THEN
         IF(NXS.LT.1)NXS = 1
         IF(NXE.GT.NPIX) NXE = NPIX
         IF(NYS.LT.1) NYS = 1
         IF(NYE.GT.NLINES) NYE = NLINES
         DO K = NYS,NYE
            DO J = NXS,NXE
               IF (KBIN.EQ.2) THEN
                  DX = REAL(J) - XA - 0.5 +0.5/ANB
                  DY = REAL(K) - YA - 0.5 + 0.5/ANB
                  S = 0.0
                  DO LX = 1,NB
                     DXA = DX + REAL(LX-1)/ANB
                     DO LY = 1,NB
                        DYA = DY + REAL(LY-1)/ANB
                        IF (KTYPE.EQ.1) THEN
                           DA = (DXA*DXA/RXTA) + (DYA*DYA/RYTA)
                           DB = SQRT((DXA*DXA/RXTB)+(DYA*DYA/RYTB))
                           R = H/(1.0+DA**(PLOR*(1.0+DB)))
                        ENDIF
                        IF (KTYPE.EQ.2) THEN
                           D = SQRT(DXA*DXA+DYA*DYA)/RADIUS
                           R = H/((1.0+D**2.0)**POWER)
                        ENDIF
                        IF (KTYPE.EQ.3) THEN
                           D = SQRT(DXA*DXA+DYA*DYA)/RADIUS
                           R = H/(1.0+(D**0.25))
                        ENDIF
                        IF(KFILM.EQ.1) R = 10.0**(R/DMFILM)
                        S = S + R
                     ENDDO
                  ENDDO
                  R = S/ABOX
                  IF(KFILM.EQ.1) R = DFILM*ALOG10(1.0/R)
               ELSE
                  DX = REAL(J) - XA
                  DY = REAL(K) - YA
                  IF (KTYPE.EQ.1) THEN
                     DA = (DX*DX/RXTA) + (DY*DY/RYTA)
                     DB = SQRT((DX*DX/RXTB)+(DY*DY/RYTB))
                     R = H/(1.0+DA**(PLOR*(1.0+DB)))
                  ENDIF
                  IF (KTYPE.EQ.2) THEN
                     D = SQRT(DX*DX+DY*DY)/RADIUS
                     R = H/((1.0+D**2.0)**POWER)
                  ENDIF
                  IF (KTYPE.EQ.3) THEN
                     D = SQRT(DX*DX+DY*DY)/RADIUS
                     R = H/(1.0+(D**0.25))
                  ENDIF
               ENDIF
C
C  Add Poisson noise, if desired
C
               IF(KRAN.EQ.1) THEN
                  CALL PRAN(R,RR,NRAN)
                  R = R + RANF*(RR-R)
               ENDIF
C
C  Add object to existing count in pixel, rounding number added
C  to nearest integer
C
               RR = REAL(IA(J,K)) + R
               IF (RR.GT.32766.9) RR = 32766.9
               IA(J,K) = NINT(RR)
C
C  Add number to actual object flux
C
               FLUX = FLUX + R
            ENDDO
         ENDDO
      ENDIF
C
      END
C
C
C



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      SUBROUTINE PRAN
C
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C PURPOSE
C           TO RETURN A POISSON RANDOMISED VERSION OF THE INPUT NUMBER
C
C METHOD
C           FOR NUMBERS LESS THAN TWENTY, CALCULATE AS FOLLOWS:-
C              CHOOSE A RANDOM NUMBER BETWEEN 0 AND 1. THEN CALC
C              THE PROBABILITY P0 OF 0 AS EXP(-S). IF NR IS LESS THAN
C              P0 EXIT WITH ANSWER 0. ELSE CALC PROB P1 OF 1 AS
C              S*EXP(-S)/1. IF NR LESS THAN P0+P1, EXIT WITH ANS 1.
C              ELSE CALC PROB P2 OF 2 AS S*S*EXP(-S)/2.1  IF NR LESS
C              THAN P0+P1+P2, EXIT WITH ANS 2. AND SO ON
C           FOR NUMBERS MORE THAN TWENTY, THE SUM OF SIX UNIFORMLY
C           RANDOM NUMBERS GIVE A VERY NEARLY GAUSSIAN PATTERN,WHICH
C           IS VERY NEARLY A POISSONIAN PATTERN
C
C ARGUMENTS
C           S (IN)
C           REAL
C                INPUT NUMBER
C           SS (OUT)
C           REAL
C                OUTPUT NUMBER (INTEGRAL VALUE)
C           NRAN
C           INTEGER
C                THE SEED NUMBER FOR THE RANDOM NUMBER GENERATOR
C
C CALLS
C           NONE
C
C NOTES
C           THE SEED NUMBER IS CHANGED AFTER EACH CALL, BUT CAN BE SET
C           TO ANY NUMBER TO START THE RANDOM NUMBER PATTERN
C
C WRITTEN BY
C     A.J. PENNY
C
C ----------------------------------------------------------------------
C
C
C
      SUBROUTINE PRAN(S,SS,NRAN)
C
C
C
      IF(S.LT.20.0) THEN
         B = RAN(NRAN)
         K = 0
         R = 0.0
         A = EXP(-1.0*S)
         C = A
   10    CONTINUE
         IF(B.LT.C)GO TO 11
         K = K + 1
         R = R + 1.0
         A = A*S/R
         C = C + A
         GO TO 10
   11    CONTINUE
         SS = REAL(K)
      ELSE
         A = 0.0
         DO K = 1,6
            A = A + RAN(NRAN)
         ENDDO
         A = (A-3.0)/0.709
         SS = A*SQRT(S)
         SS = AINT(S+SS+0.5)
      ENDIF
C
      END
C
C
C



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R IMGRVL *
C      *            *
C      **************
C
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CPURPOSE
C	TO SET AN IMAGE ARRAY TO A CONSTANT VALUE WITH POISSONIAN STATISTICS
C
CMETHOD
C     POISSONIAN VALUE TO BE ASSIGNED
C
CARGUMENTS
C	IA (OUT)
C	INTEGER*2(NPIX,NLINES)
C		IMAGE ARRAY
C	NPIX,NLINES (IN)
C	INTEGER
C		DIMENSIONS OF IA
C     RVAL (IN)
C     REAL
C             MEAN VALUE TO PUT IN IMAGE
C
CCALLS
C     PRAN
C
CNOTES
C	USES INTEGER*2 ARRAYS
C
CWRITTEN BY
C     A.J. PENNY
C ----------------------------------------------------------------------
C
C
C
      SUBROUTINE IMGRVL(IA,NPIX,NLINES,RVAL,RANF,NRAN)
C
C
C
      INTEGER*2 IA(NPIX,NLINES)
C
C SCAN IMAGE, SETTING EACH PIXEL TO VALUE AROUND RVAL
C
      DO J=1,NLINES
	   DO I=1,NPIX
           CALL PRAN(RVAL,RR,NRAN)
           IA(I,J) = RVAL + RANF*(RR-RVAL)
         ENDDO
      ENDDO
C
C
C
      END



