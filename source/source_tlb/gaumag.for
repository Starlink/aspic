C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   GAUMAG *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               GAUMAG
C
C
C          FUNCTION:-
C               It estimates the flux of stars in an image, by taking a
C               small area around the star position and fitting a background
C               and a 2-D Gaussian profile to the data in that area. The
C               star positions are taken from an XYlist that is fed in
C               and the results are typed out and stored available for
C               further work.
C
C               The star profile is in the form:-
C
C                                    2
C                                -(d)
C                         I = exp
C
C
C                  where d = sqrt((X/RX)**2+(Y/RY)**2)
C
C               The magnitudes are calculated as PI.HEIGHT.RX.RY, which
C               is the volume of a Gaussian.
C
C
C               BEWARE
C               Star profiles are not strictly Gaussian, so for stars
C               with differing profiles, there will be a (small)
C               systematic error. This program is mainly intended
C               for use on the stars in one image which may all be
C               supposed to have the same profile.
C               For bright stars it may be OK to have slightly
C               differing profiles.
C               For very faint stars the use of floating profile
C               causes errors.
C
C
C
C          USE:-
C               The image and XYlist are input and the profile parameters
C               are input, and the program then goes through the list
C               doing the work
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               This  is  the  2-D   Starlink
C                                             I*2 image whose stars are to be
C                                             measured.
C
C         XYLIST                              This  is  a  list  of   (x,y)
C                                             co-ordinates stored as the
C                                             first two columns of an XYlist
C
C         OUTPUT                              This  is  the   name   of   a
C                                             Starlink  frame which is used
C                                             to  store  the  results,  and
C                                             other   useful   information,
C                                             obtained. The output is in the
C                                             same format as written by EDRS.
C
C
C         TITLE      Output from GAUMAG       The title to be written to
C                                             the output file.
C
C         PARFIX          NO                  Flag for wether a fixed
C                                             profile is to be used or
C                                             wether the radii are to be
C                                             allowed to vary for each
C                                             star to best fit the data.
C                                             Choices are NO,YES
C                                             If PARFIX is taken as NO,
C                                             then the starting values
C                                             for the iteration of the
C                                             radii are 5,5.
C
C         RX           GAUFIT_RX              The Gaussian X radius to be
C                                             used if a fixed profile is
C                                             employed
C                                             Set = 4 if GAUFIT_RX is not
C                                             set.
C
C         RY           GAUFIT_RY              The Gaussian Y radius to be
C                                             used if a fixed profile is
C                                             employed.
C                                             Set = RX if GAUFIT_RY is not
C                                             set.
C
C         XBOX         6*RX or 30             These are the lengths of the
C                                             sides of the area to be used
C         YBOX         6*RY or XBOX           around each star position.
C                                             (the 6* values are for if
C                                             fixed radii are used.)
C
C
C         A J Penny       RGO                                  29-JUL-82
C
C--------------------------------------------------------------------------

C
C
C   The Starlink master segment was written by K F Hartley
C   and extensively modified by A J Penny
C
C  CALLS
C      EDRS Package
C          GTXYLR,GTXYLW
C      Starlink
C          RDIMAG,WRUSER,RDKEYI,RDKEYR,RDKEYC,CNPAR,FRDATA,CTOI,CTOR,
C          RDDSCR,PTDSCR.
C      Grasp
C          AGAUSS,EXTLSA,GETSYM,CHARLN
C
C
C -----------------------------------------------------------
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER ISIDE(2),KX(2)
      REAL COORDS(2),TEMP(12,1),RADIUS(2)
      CHARACTER*72 TXT,TEXT,ATITLE
      LOGICAL*1 VALID
      CHARACTER CVAL*1
      CHARACTER*30 TITLE,SYMB
C
C
C
      VALID =.TRUE.
C
C
C   First get the data array
C
      CALL GTIMG(IPIM,KX,BSCALE,BZERO,INVAL,ATITLE,IERR)
      IF (IERR.NE.0) THEN
         CALL WRUSER('NOT A VALID IMAGE',ISTAT)
         VALID = .FALSE.
      ENDIF
C
C
C --------------------------------------------------------
C
C
C  Now get the XY list
C
      IF (VALID) THEN
         CALL GTXYLR('XYLIST',.FALSE.,NXYVAL,NSTAR,IPXY,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            WRITE (TEXT,900) NSTAR
  900       FORMAT ('NUMBER OF STARS IN THE LIST IS',I6)
            CALL WRUSER(TEXT,ISTAT)
            CALL RDDSCR('XYLIST','TITLE',1,TITLE,I,ISTAT)
         ELSE
            VALID = .FALSE.
            CALL WRUSER('NOT A VALID XY LIST',ISTAT)
         END IF
      ENDIF
C
C -------------------------------------------------------
C
      IF (VALID) THEN
C
C  Open output XY list
C
         CALL GTXYLW('OUTPUT',.FALSE.,17,NSTAR,IPOUT,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
C
C
C   Store information to show how many values are stored for each star,
C   and how many star records are present.
C
            CALL PTDSCR('OUTPUT','NITEM','INTEGER',17,RVAL,CVAL,IERR)
            CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',NSTAR,RVAL,CVAL,
     +               IERR)
C
C   Now pick up a new title if required.
C
            CALL CHARLN(TITLE,LEN)
            IF (LEN.EQ.0) THEN
               TITLE = 'Output from GAUMAG'
            ENDIF
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,IERR)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,
     +                  TITLE,IERR)
C
C  Transfer identifiers
C
            CALL EXTLSA(%VAL(IPXY),NXYVAL,NSTAR,1,NSTAR,1,5,
     +                  %VAL(IPOUT),17,NSTAR,1,1)
         ELSE
            CALL WRUSER('CANT OPEN OUTPUT FILE',ISTAT)
            VALID = .FALSE.
         ENDIF
C
      ENDIF
C
C
C  ------------------------------------------------------
C
C
      IF (VALID) THEN
C
C  Get the profile parameters
C
         KK = 1
         CALL GETCMD('PARFIX','NO,YES.',1,KK,TEXT,KTEXT,IERR)
         IF (KK.EQ.1.OR.IERR.NE.0) THEN
            KW = 0
            RADIUS(1) = 0.0
            RADIUS(2) = 0.0
         ELSE
            KW = 1
            SYMB = 'GAUFIT_RX'
            CALL GETSYM(SYMB,RADIUS(1),ISTAT)
            IF (ISTAT.NE.0) RADIUS(1) = 4.0
            CALL RDKEYR('RX',.TRUE.,1,RADIUS(1),I,ISTAT)
            SYMB = 'GAUFIT_RY'
            CALL GETSYM(SYMB,RADIUS(2),ISTAT)
            IF (ISTAT.NE.0) RADIUS(2) = RADIUS(1)
            CALL RDKEYR('RY',.TRUE.,1,RADIUS(2),I,ISTAT)
         END IF
C
C   Now define size of box for fitting
C   Default is 6*Radius
C
         IF (KW.EQ.0) THEN
            ISIDE(1) = 30
         ELSE
            ISIDE(1)=6.0*RADIUS(1)
         ENDIF
         CALL RDKEYI('XBOX',.TRUE.,1,ISIDE(1),I,ISTAT)
         IF (ISTAT.GT.ERR_PARNUL) ISIDE(1) = 30
         IF (KW.EQ.0) THEN
            ISIDE(2) = ISIDE(1)
         ELSE
            ISIDE(2) = 6.0*RADIUS(2)
         ENDIF
         CALL RDKEYI('YBOX',.TRUE.,1,ISIDE(2),I,ISTAT)
         IF (ISTAT.GT.ERR_PARNUL) ISIDE(2) = 30
C
C
C
      ENDIF
C
C
C ---------------------------------------------------------
C
C
      IF (VALID) THEN
C
C
C
         WRITE(TEXT,910)
  910    FORMAT(' STAR  MAG INV ITER  RMS  DX   DY  HEIGHT',
     +          ' BASE   XPOS   YPOS  RADX  RADY')
         CALL WRUSER(TEXT,ISTAT)
C
C
C
         DO NS = 1,NSTAR
C
C  Get an XY position from the XY list
C
            CALL EXTLSA(%VAL(IPXY),NXYVAL,NSTAR,NS,NS,6,7,
     +                  TEMP,12,1,1,1)
            COORDS(1) = TEMP(1,1)
            COORDS(2) = TEMP(2,1)
C
C  Fit the Gaussian to the data at that position
C
            CALL AGAUSS (%VAL(IPIM),KX(1),KX(2),
     +                   COORDS(1),COORDS(2),
     +                   ISIDE(1),ISIDE(2),
     +                   KW,RADIUS(1),RADIUS(2),INVAL,20,
     +                   AMAG,HEIGHT,BASE,DXO,DYO,ANX,ANY,
     +                   RX,RY,RMS,ITER,NINVAL)
            AMAG = AMAG - 2.5*ALOG10(BSCALE)
            HEIGHT = HEIGHT*BSCALE
            BASE = BASE*BSCALE + BZERO
            RMS = RMS*BSCALE
C
C  Type the results to the terminal
C
            ARMS = RMS
            IF (ABS(RMS).GT.9999.0) ARMS = SIGN(9999.0,ARMS)
            AH = HEIGHT
            IF (AH.GT.99999.0) AH = 99999.0
            IF (AH.LT.-9999.0) AH = -9999.0
            AB = BASE
            IF (AB.GT.99999.0) AB = 99999.0
            IF (AB.LT.-9999.0) AB = -9999.0
            AANX = ANX
            IF (ABS(AANX).GT.9999.0) AANX = SIGN(9999.0,AANX)
            AANY = ANY
            IF (ABS(AANY).GT.9999.0) AANY = SIGN(9999.0,AANY)
            ARX = RX
            IF (ABS(ARX).GT.99.0) ARX = SIGN(99.0,ARX)
            ARY = RY
            IF (ABS(ARY).GT.99.0) ARY = SIGN(99.0,ARY)
            KINVAL = NINVAL
            IF (KINVAL.GT.999) KINVAL = 999
            WRITE (TEXT,920) NS,AMAG,KINVAL,ITER,ARMS,
     +                       DXO,DYO,AH,AB,AANX,AANY,ARX,ARY
  920       FORMAT (I4,1X,F5.2,1X,I3,2X,I2,1X,F5.0,1X,2(F4.0,1X),
     +              2F6.0,2(F6.1,1X),2(F5.2,1X))
            CALL WRUSER(TEXT,ISTAT)
C
C  Store the results
C
            TEMP(1,1) = ANX
            TEMP(2,1) = ANY
            TEMP(3,1) = AMAG
            TEMP(4,1) = DXO
            TEMP(5,1) = DYO
            TEMP(6,1) = REAL(ITER)
            TEMP(7,1) = RMS
            TEMP(8,1) = REAL(NINVAL)
            TEMP(9,1) = HEIGHT
            TEMP(10,1) = BASE
            TEMP(11,1) = RX
            TEMP(12,1) = RY
            CALL EXTLSA(TEMP,12,1,1,1,1,12,
     +                  %VAL(IPOUT),17,NSTAR,NS,6)
C
C  Return for another star
C
         ENDDO
C
C
      ENDIF
C
C
C ------------------------------------------------------
C
C
C   Finally tidy up and go home
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END
C
C
C



