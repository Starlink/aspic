C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   GAUFIT *
C                     *                  *
C                     ********************
C
C
C
C
C
C          FUNCTION:-
C               It enables you to estimate the Gaussian profile
C               that best fits the stars in an image.
C
C          USE
C                 The image name and a list of XY positions are fed in
C               (positions in an XYlist file). The program fits a
C               simple Gaussian at each position to find the star's
C               exact centre and calculates the radius and types it,
C               and the residuals to the fit, out.
C               Bad stars are rejected.
C               The program then calculates the mean fitted radii
C               and gives you the chance to reject those stars that
C               have too discordant fitted radii and then any others.
C
C               The program then makes a 'grand' star by taking the
C               data around each individual good star centre and adding
C               each pixel value into a total array at its appropriate
C               position.
C               The radii of this mean star are then calculated and
C               stored as Starlink parameters of the program.
C
C               You can then go on and calculate even better radii by
C               doing the summing in a better way, in which the program
C               allows for the fact that the stars are not centred at
C               integral pixel values by interpolating between pixels
C               before adding into the mean array.
C               If this is done the output radii are these new ones.
C
C               You can then display the mean fit and residuals as a
C               radial plot and as a solid body plot.
C
C               The mean fitting can then be redone.
C
C               The star profile is in the Gauss form :-
C
C
C                                     2
C                                 -1.d
C                           I =  e
C
C               Where d = sqrt((X/RX)**2+(Y/RY)**2)
C
C
C               Beware that, as stars are not exactly Gaussian, the fitted
C               profile will vary with box size.
C
C
C
C         USER PARAMETERS:-
C
C          IMAGE                              This is the name of the I*2
C                                             image containing the stars.
C
C          XYLIST                             The star position list (It must
C                                             be in the XYlist format.)
C
C
C          XBOX          30                   The length in X pixels of the
C                                             area around each star to be
C                                             analysed for testing if it is
C                                             acceptable.
C
C          YBOX          30                   The length in Y pixels of
C                                             that area
C
C          RADIILIM                           Limits to radii of trial
C                                             fit to a star for that
C                                             star to be accepted for the
C                                             averaging and summing.
C
C          REJECT        -1                   No in input list of star
C                                             not to be used in making
C                                             summed array. You are
C                                             asked again and again for
C                                             for this, until you type
C                                             -1
C
C          SUMMED        YES                  Flag for summing all boxes
C                                             containing good stars and
C                                             calculating mean profile.
C                                             Choices are YES,NO.
C
C
C          XBOXSUM       6*Xradius            The length in X pixels of the
C                                             area around each star to be
C                                             summed in.
C
C          YBOXSUM       6*Yradius            The length in Y pixels of
C                                             that area
C
C          DEVICE        ARGS                 The display device for the
C                                             summed star profile and showing
C                                             the star itself as a sold body
C
C          DEVSIZE       Various              For the solid body plots,the
C                                             size of the picture.
C
C          DEVLIM        Min,Max              For the solid body plots, the
C                                             range of values to form the
C                                             bottom and top of the plot
C
C          AGAIN         Yes                  Flag for another fit to the
C                                             summed data
C
C
C       STARLINK OUTPUT PARAMETERS
C
C          RX                                 This is the fitted radius
C                                             in the X direction.
C
C          RY                                 The same in the Y direction
C
C
C
C
C
C
C
C           A J Penny   RGO                            83-2-18
C
C
C--------------------------------------------------------------------------



C
C      Starlink Parameters
C        SUMMED
C
C
C      CALLS
C       Starlink
C         CNPAR,FRDATA,GETDYN,WRUSER
C       Edrs
C         GETCMD,GTXYLR
C       Grasp
C         GTIMG
C       This file
C         DOFIND,DOFITG
C
C
C      USES
C        I*2 arrays
C        %Val facility
C        Starlink ERRPAR,FMTPAR
C
C ----------------------------------------------------------------------
C
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER NSIZE(2)
      LOGICAL VALID
      CHARACTER*72 TXT
      CHARACTER TEXT*72,TITLE*72
C
C   Set continuation flag
C
      VALID = .TRUE.
C
C --------------------------------------------------------------
C
C   Get the image data array
C
      CALL GTIMG(IPIN,NSIZE,BS,BZ,INVAL,TITLE,IERR)
      IF (IERR.NE.0) VALID = .FALSE.
C
C ----------------------------------------------------------------
C
C   Now seek a list of (x,y) positions
C
      IF (VALID) THEN
         CALL GTXYLR('XYLIST',.FALSE.,NIPXY,LSTLEN,IPXY,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            WRITE (TEXT,900) LSTLEN
  900       FORMAT ('NUMBER OF STARS IN THE LIST IS',I4)
            CALL WRUSER(TEXT,ISTAT)
         ELSE
            CALL WRUSER('NO VALID XY LIST',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Open working areas
C
      IF (VALID) THEN
         LSTB = LSTLEN*9
         CALL GETDYN('STORE',FMT_R,LSTB,IPST,ISTATA)
         IF (ISTATA.NE.ERR_NORMAL) THEN
            CALL WRUSER('CANT GET TEMPORARY STORAGE',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C -----------------------------------------------------------------
C
C  Find which stars are good enough to take data from and estimate
C  of star radii.
C
      IF (VALID) THEN
         CALL DOFIND(%VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,INVAL,
     +              %VAL(IPXY),NIPXY,LSTLEN,%VAL(IPST),9,RX,RY)
      ENDIF
C
C  Sum the data from good stars and make mean fit, using simple
C  summing
C
      IF (VALID) THEN
         CALL DOFITG(%VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,INVAL,
     +              %VAL(IPST),9,LSTLEN,RX,RY,0)
      ENDIF
C
C  Ask wether to do sum again using interpolation, and if so, do
C
      IF (VALID) THEN
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('SUM ARRAYS WITH INTERPOLATION ?',ISTAT)
         KW = 1
         CALL GETCMD('SUMMED','YES,NO.',1,KW,TEXT,KTEXT,IERR)
         CALL CNPAR('SUMMED',ISTAT)
         IF (KW.EQ.1) THEN
            CALL DOFITG(%VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,INVAL,
     +                 %VAL(IPST),9,LSTLEN,RX,RY,1)
         ENDIF
      ENDIF
C
C --------------------------------------------------------------
C
C  Release the temporary working areas and exit from program
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOFIND *
C      *            *
C      **************
C
C
C   PURPOSE
C                This subroutine fits Gaussians to stars
C                in an XY list and finds the good ones and makes
C                an estimate of the radii.
C
C   ARGUMENTS
C  IN
C    KPT(NPIX,NLINE)  Integer*2      The data array
C    NPIX             Integer        The X length of KPT
C    NLINE            Integer        The Y length of KPT
C    BS               Real           The BSCALE of the data
C    BZ               Real           The BZERO of the data
C    INVAL            Integer        The pixel Invalid value
C    XYLIST(NXY,LSTLEN) Real         The list of star positions
C    NXY              Integer        The no of cols + 5 in XYLIST
C    LSTLEN           Integer        The no of rows in XYLIST
C  OUT
C    RES(NRES,LSTLEN) Real           The results
C    NRES             Integer        The no of parameters in RES
C    RXA              Real           The mean fitted X radius
C    RYA              Real           The mean fitted Y radius
C
C
C   STARLINK PARAMETERS
C      XBOX,YBOX
C
C
C   CALLS
C     Starlink
C       CNPAR,WRUSER
C     Edrs
C       GETPAR
C     Grasp
C       AGAUSS
C     This file
C       STARRJ
C
C   USES
C     I*2 arrays
C
C
C   A.J.PENNY                   RGO                    83-3-17
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE DOFIND(KPT,NPIX,NLINE,BS,BZ,INVAL,
     +                  XYLIST,NXY,LSTLEN,RES,NRES,RXA,RYA)
C
C
C
      INTEGER*2 KPT(NPIX,NLINE)
      REAL XYLIST(NXY,LSTLEN),RES(NRES,LSTLEN),SIZE(2)
      LOGICAL VALID,VALIDA,AGAIN
      CHARACTER TEXT*72,TXT*3
C
C
C  Get the box sizes
C
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER(' INPUT SIDES OF BOX ROUND STAR',ISTAT)
      KX = 30
      CALL GETPAR('XBOX','INTEGER',1,2.0,70.0,.TRUE.,KX,
     +            RVAL,IERR1)
      KY = KX
      CALL GETPAR('YBOX','INTEGER',1,2.0,70.0,.TRUE.,KY,
     +            RVAL,IERR2)
      CALL CNPAR('XBOX',ISTAT)
      CALL CNPAR('YBOX',ISTAT)
      RKX = REAL(KX)/6.0
      RKY = REAL(KY)/6.0
C
C  Loop through the star list
C
      CALL WRUSER(' ',ISTAT)
      WRITE(TEXT,905)
  905 FORMAT(' ',43X,'GAUSS')
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,906)
  906 FORMAT(' ','STAR','  MAG ',' HEIGHT','  DX ','  DY ',
     +       '  RMS ','ITS',' IN ','  RX ','   RY ')
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      DO NSTAR = 1,LSTLEN
         XA = XYLIST(6,NSTAR)
         YA = XYLIST(7,NSTAR)
C
            CALL AGAUSS(KPT,NPIX,NLINE,
     +                   XA,YA,KX,KY,
     +                   0,RKX,RKY,INVAL,20,
     +                   AMAGA,HEIGHT,BASE,DXO,DYO,ANX,ANY,
     +                   RX,RY,RMS,ITER,NINVAL)
         AMAG = AMAGA - 2.5*ALOG10(BS)
         HEIGHT = HEIGHT*BS
         BASE = BASE*BS + BZ
         RMS = RMS*BS
C
C  Store position and radii
C
         RES(1,NSTAR) = ANX
         RES(2,NSTAR) = ANY
         RES(3,NSTAR) = RX
         RES(4,NSTAR) = RY
         RES(5,NSTAR) = ITER
         RES(6,NSTAR) = NINVAL
         RES(8,NSTAR) = AMAGA
C
C  Type out some fit values
C
         AH = HEIGHT
         ADX = DXO
         ADY = DYO
         ARMS = RMS
         ARX = RX
         ARY = RY
         IF (ABS(AH).GT.9999.0) AH = SIGN(9999.0,AH)
         IF (ABS(ADX).GT.99.0) ADX = SIGN(99.0,ADX)
         IF (ABS(ADY).GT.99.0) ADY = SIGN(99.0,ADY)
         IF (ABS(ARMS).GT.999.0) ARMS = SIGN(999.0,ARMS)
         IF (ABS(ARX).GT.99.0) ARX = SIGN(99.0,ARX)
         IF (ABS(ARY).GT.99.0) ARY = SIGN(99.0,ARY)
         NIN = NINVAL
         IF (NIN.GT.99) NIN = 99
         IF (NINVAL.GT.0.OR.ITER.GT.19.OR.AMAGA.GT.49.0) THEN
            TXT = 'REJ'
         ELSE
            TXT = '   '
         ENDIF
         WRITE (TEXT,900) NSTAR,AMAG,AH,ADX,ADY,
     +                    ARMS,ITER,NIN,ARX,ARY,TXT
  900    FORMAT (1H ,I4,F6.2,F7.1,2F5.1,F6.1,I3,I3,2F6.2,2X,A3)
         CALL WRUSER(TEXT,ISTAT)
C
C
      ENDDO
C
C --------------------------------------------------------
C
C  Find which stars are wanted for fit
C
      CALL STARRJ(RES,NRES,LSTLEN)
C
C  Get average profile
C
      RXA = 0.0
      RYA = 0.0
      NRAD = 0
      DO NSTAR = 1,LSTLEN
         IF (RES(9,NSTAR).LT.0.5) THEN
            RXA = RXA + RES(3,NSTAR)
            RYA = RYA + RES(4,NSTAR)
            NRAD = NRAD + 1
         ENDIF
      ENDDO
      IF (NRAD.EQ.0) NRAD = 1
      RXA = RXA/REAL(NRAD)
      RYA = RYA/REAL(NRAD)
      IF (ABS(RXA).GT.99.0) RXA = SIGN(99.0,RXA)
      IF (ABS(RYA).GT.99.0) RYA = SIGN(99.0,RYA)
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER(' MEAN OF RADII ARE',ISTAT)
      WRITE(TEXT,910)RXA,RYA
  910 FORMAT(' ','RX = ',F6.2,'   RY = ',F6.2)
      CALL WRUSER(TEXT,ISTAT)
      SEEING = 1.65*((RXA+RYA)/2.0)
      WRITE(TEXT,911)SEEING
  911 FORMAT(' ','SEEING (FWHM) = ',F6.2)
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOFITG *
C      *            *
C      **************
C
C
C   PURPOSE
C       Sums the data from a number of stars in an image and fits
C       a mean Gaussian to them
C       Method of summing is either taking pixels and adding to nearest
C       pixel, or interpolating to exact pixel posn.
C       Output is typed to terminal and put in the Starlink parameters.
C
C   ARGUMENTS
C  IN
C    KPT(NPIX,NLINE)   Integer*2      The image
C    NPIX              Integer        The X size of the image
C    NLINE             Integer        The Y size of the image
C    BS                Real           The BSCALE of the image
C    BZ                Real           The BZERO of the image
C    INVAL             Integer        The Invalid pixel flag in the image
C    RES(NRES,LSTLEN)  Real           List of star posns and reject flags
C    NRES              Integer        No of params in RES (1,2,9 used only)
C    LSTLEN            Integer        No of stars in list
C    RXA               Real           Input estimate of star Gauss X radius
C    RYA               Real           Input estimate of star Gauss Y radius
C    KTYPE             Integer        Type of summing of stars pixels
C                                       (0=simple;1=interpolated)
C
C   STARLINK PARAMETERS
C      AGAIN,XBOXSUM,YBOXSUM
C
C
C   CALLS
C     Simpleplot
C       PLOTSL
C     Starlink
C       CNPAR,FRDATA,GETDYN,RDUSER,WRERR,WRKEYR,WRUSER
C     Edrs
C       GETCMD,GETPAR
C     Grasp
C       COPYRGO,BGAUSS,DEVCLS,DEVOPN,RCLEAR
C     This file
C       COPYR,FITOG,PLTPRG,RESIDG
C
c
C   USES
C     Integer*2 arrays
C     %Val facility
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-3-17
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE DOFITG(KPT,NPIX,NLINE,BS,BZ,INVAL,
     +                 RES,NRES,LSTLEN,RXA,RYA,KTYPE)
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER*2 KPT(NPIX,NLINE)
      REAL RES(NRES,LSTLEN),SIZE(2)
      LOGICAL VALID,VALIDA,AGAIN
      CHARACTER TEXT*72,TXT*3
C
C  Loop round estimating best profile
C
      VALID = .TRUE.
C
C
C
      AGAIN = .TRUE.
      DO WHILE (AGAIN)
C
C  Get the star analysis box size
C
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER(' INPUT SIDES OF BOX FOR SUMMED ARRAY',ISTAT)
         KX = 6.0*RXA
         IF (KX.GT.70) KX = 70
         CALL GETPAR('XBOXSUM','INTEGER',1,2.0,70.0,.TRUE.,KX,
     +               RVAL,IERR1)
         KY = 6.0*RYA
         IF (KY.GT.70) KY = 70
         CALL GETPAR('YBOXSUM','INTEGER',1,2.0,70.0,.TRUE.,KY,
     +               RVAL,IERR2)
         IF (IERR1.NE.0.OR.IERR2.NE.0) THEN
            VALID = .FALSE.
            CALL WRUSER('BAD ANSWER',ISTAT)
         ENDIF
         CALL CNPAR('XBOXSUM',ISTAT)
         CALL CNPAR('YBOXSUM',ISTAT)
C
C -------------------------------------------------------
C
C      Now get work space for the data, fit and residuals.
C      If work space can be found set VALIDA true
C
         IF (VALID) THEN
            NXY = KX*KY
            CALL GETDYN('TOTAL',FMT_R,NXY,IPTOT,ISTAT)
            CALL GETDYN('WORK',FMT_R,NXY,IPWORK,ISTATA)
            IF (ISTAT.NE.ERR_NORMAL.OR.ISTATA.NE.ERR_NORMAL) THEN
               CALL WRERR('HELLD')
               VALID = .FALSE.
            END IF
            CALL GETDYN('RESID',FMT_R,NXY,IPR,ISTAT)
            IF (ISTAT.NE.ERR_NORMAL) THEN
               CALL WRERR('HELLR')
               VALID = .FALSE.
            END IF
         ENDIF
C
C ---------------------------------------------------------------
C
         IF (VALID) THEN
C
C  Clear Mean array
C
            CALL RCLEAR(%VAL(IPTOT),KX,KY,1,KX,1,KY)
C
C  Add in the wanted stars
C
            DO K = 1,LSTLEN
               IF (RES(9,K).LT.0.5) THEN
                  JX1 = RES(1,K) - REAL((KX/2))
                  JX2 = JX1 + KX - 1
                  JY1 = RES(2,K) - REAL((KY/2))
                  JY2 = JY1 + KY - 1
                  IF (KTYPE.EQ.0) THEN
                     CALL COPYRGO(KPT,NPIX,NLINE,%VAL(IPTOT),KX,KY,
     +                         JX1,JY1,JX2,JY2,BS,BZ,INVAL,NIN,1)
                  ELSE
                     XD = RES(1,K) - INT(RES(1,K))
                     YD = RES(2,K) - INT(RES(2,K))
                     CALL COPYR(KPT,NPIX,NLINE,BS,BZ,%VAL(IPTOT),KX,KY,
     +                          JX1,JY1,XD,YD,%VAL(IPWORK))
                  ENDIF
               ENDIF
            ENDDO
C
C  Solve the star
C
            CALL BGAUSS(%VAL(IPTOT),KX,KY,KX,KY,0,RXA,RYA,20,
     +                   AMAG,HEIGHT,BASE,XA,YA,
     +                   RXB,RYB,RMS,ITER)
            RXA = RXB
            RYA = RYB
C
C  Type out fit
C
            CALL FITOG(HEIGHT,BASE,XA,YA,RMS,RXA,RYA,ITER)
C
C  Load the residuals
C
            CALL RESIDG(%VAL(IPTOT),KX,KY,%VAL(IPR),
     +                 XA,YA,HEIGHT,BASE,RXA,RYA)
C
C  Display the fit as a radial plot
C
            CALL DEVOPN(IDEV,SIZE)
            IF (IDEV.NE.1) THEN
               CALL PLTPRG(%VAL(IPR),KX,KY,XA,YA,
     +                     HEIGHT,RXA,RYA,SIZE)
               CALL DEVCLS(IDEV)
            ENDIF
C
C  Put out image and residuals as a solid body plot
C
            CALL WRUSER('SOLID BODY AND RESIDUALS PLOT',ISTAT)
            CALL DEVOPN(IDEV,SIZE)
            IF (IDEV.NE.1) THEN
               CALL PLOTSL(%VAL(IPTOT),KX,KY,SIZE,IERR)
               CALL WRUSER(' PRESS "RETURN" FOR RESIDUALS',ISTAT)
               CALL RDUSER(TEXT,ISTAT)
               CALL PLOTSL(%VAL(IPR),KX,KY,SIZE,IERR)
               CALL DEVCLS(IDEV)
            ENDIF
         ENDIF
C
C  Store output radii
C
         IF (VALID) THEN
            CALL WRKEYR('RX',RXA,1,IERR)
            CALL WRKEYR('RY',RYA,1,IERR)
         ENDIF
C
C  Release working areas
C
         CALL FRDATA('TOTAL',ISTAT)
         CALL FRDATA('RESID',ISTAT)
         CALL FRDATA('WORK',ISTAT)
C
C  Make another fit ?
C
         K = 1
         CALL GETCMD('AGAIN','YES,NO.',1,K,TEXT,KTEXT,IERR)
         CALL CNPAR('AGAIN',ISTAT)
         IF (K.NE.1) AGAIN = .FALSE.
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R RESIDG*
C      *           *
C      *************
C
C
C    PURPOSE
C      This s/r makes the residuals from the data in an array and
C      the position,height,base and Gaussian profile fitted.
C
C   ARGUMENTS
C  IN
C    DATA(KX,KY)     Real       The array
C    KX              Integer    The X length of the array
C    KY              Integer    The Y length of the array
C    XA              Real       The star X position
C    YA              Real       The star Y position
C    A               Real       The star height
C    B               Real       The base level
C    RX              Real       The star X radius
C    RY              Real       The star Y radius
C  OUT
C    RES(KX,KY)      Real       The difference between data and fit
C
C   CALLS
C    None
C
C
C      A J PENNY                  RGO                      83-3-16
C -----------------------------------------------------------------
C
C
      SUBROUTINE RESIDG(DATA,KX,KY,RES,XA,YA,A,B,RX,RY)
C
C
C
      REAL DATA(KX,KY),RES(KX,KY)
C
C
      DO K = 1,KY
         DO J = 1,KX
            D1 = (ABS(REAL(J)-XA)/RX)**2.0
            D1 = D1 + (ABS(REAL(K)-YA)/RY)**2.0
            IF (D1.GT.50.0) D1 = 50.0
            FIT = B + A*EXP(-1.0*D1)
            RES(J,K) = DATA(J,K) - FIT
         ENDDO
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
C      * S/R PLTPRG *
C      *            *
C      **************
C
C
C     PURPOSE
C       This s/r plots out as a radial plot the Gauss profile
C       and the residuals from the fit.
C       The base level is not shown.
C
C
C   ARGUMENTS
C    IN
C     RESID(LX,LY)    Real       The reiduals to the fit
C     LX              Integer    The X size of the array
C     LY              Integr     The Y size of the array
C     XA              Real       The X position of the profile centre
C     YA              Real       The Y position of the profile centre
C     H               Real       The height of the profile
C     RX              Real       The star X radius
C     RY              Real       The star Y radius
C     SIZE(2)         Real       X and Y size of plot in cm
C
C
C   CALLS
C    Simpleplot
C       BREAK,JBAXES,JOIN PT,MARK PT
C
C      A J PENNY                  RGO                  82-OCT-25
C -----------------------------------------------------------
C
C
C
      SUBROUTINE PLTPRG(RESID,LX,LY,XA,YA,H,
     +                  RX,RY,SIZE)
C
C
C
      REAL RESID(LX,LY),SIZE(2),AX(2),AY(2)
      REAL DIST(200),PDATA(200),PFIT(200)
      INTEGER NUM(200)
C
C  Calc no of radial bins
C
      XMAX = (XA-1.0)/RX
      XMAXA = (REAL(LX) - XA)/RX
      YMAX = (YA - 1.0)/RY
      YMAXA = (REAL(LY) - YA)/RY
      IF (XMAXA.GT.XMAX) XMAX = XMAXA
      IF (YMAXA.GT.YMAX) YMAX = YMAXA
      DRMAX = SQRT(XMAX*XMAX+YMAX*YMAX)
      ANUMPT = 15.0*DRMAX
      IF (ANUMPT.GT.200.0) ANUMPT = 200.0
C
C  Zero radial profile values
C
      DO K = 1,200
         NUM(K) = 0
         DIST(K) = 0.0
         PDATA(K) = 0.0
         PFIT(K) = 0.0
      ENDDO
C
C  Calc radial profile of fit and sum in Residuals to fit to get
C  radial profile of data
C
      DO K = 1,LY
         DO J = 1,LX
            DX = REAL(J) - XA
            DY = REAL(K) - YA
            DR = SQRT((DX/RX)**2.0+(DY/RY)**2.0)
            IF (DR.LE.DRMAX) THEN
               KD = 1 + INT((ANUMPT-1.0)*(DR/DRMAX))
               DIST(KD) = DIST(KD) + DR*((RX+RY)/2.0)
               NUM(KD) = NUM(KD) + 1
               PD = H*EXP(-1.0*DR**2.0)
               PFIT(KD) = PFIT(KD) + PD
               PDATA(KD) = PDATA(KD) + PD + RESID(J,K)
            ENDIF
         ENDDO
      ENDDO
C
C  Bunch up if any points in the radial profile have no data
C
      DO K = 1,200-1
         IF (NUM(K).EQ.0) THEN
            NEXT = K
            KFOUND = 0
            DO J = K+1,200
               IF (KFOUND.EQ.0.AND.NUM(J).NE.0) THEN
                  KFOUND = 1
                  NEXT = J
               ENDIF
            ENDDO
            KGAP = NEXT - K
            NUM(K) = NUM(K+KGAP)
            DIST(K) = DIST(K+KGAP)
            PFIT(K) = PFIT(K+KGAP)
            PDATA(K) = PDATA(K+KGAP)
            NUM(K+KGAP) = 0
         ENDIF
      ENDDO
      NTOT = 0
      DO K = 1,200
         IF (NUM(K).NE.0) NTOT = NTOT + 1
      ENDDO
C
C  Divide by number of points added in in each radial point
C  to get mean value
C
      DO K = 1,NTOT
         ANUM = REAL(NUM(K))
         PDATA(K) = PDATA(K)/ANUM
         DIST(K) = DIST(K)/ANUM
         PFIT(K) = PFIT(K)/ANUM
      ENDDO
C
C  Plot out the points
C
      DATMIN = 0.0
      DO K = 1,NTOT
         IF (PDATA(K).LT.DATMIN) DATMIN = PDATA(K)
      ENDDO
      AX(1) = 0.0
      AX(2) = 0.0
      AY(1) = 0.0
      AY(2) = 0.0
      DO K = 1,NTOT
         IF (DIST(K).GT.AX(2)) AX(2) = DIST(K)
         Y = PDATA(K) - DATMIN
         IF (Y.GT.AY(2)) AY(2) = Y
         Y = PFIT(K) - DATMIN
         IF (Y.GT.AY(2)) AY(2) = Y
      ENDDO
      CALL JBAXES(AX,2,SIZE(1),' ',1,AY,2,SIZE(2),' ',1)
      DO K = 1,NTOT
         X = DIST(K)
         Y = PDATA(K) - DATMIN
         CALL MARK PT(X,Y,3)
      ENDDO
      CALL BREAK
      DO K = 1,NTOT
         X = DIST(K)
         Y = PFIT(K) - DATMIN
         CALL JOIN PT(X,Y)
      ENDDO
C
C
C
      END


C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         *************
C         *           *
C         * S/R COPYR *
C         *           *
C         *************
C
C
C   PURPOSE
C   This subroutine adds part of the I*2 array KWHOLE
C   into the Real array PART. With an interpolated shift of XD,YD.
C   Interpolation is done by cubic running fit over 4 points in X
C   and then the same in Y. The 2 points nearest any edge are taken
C   straight over, not interpolated.
C
C
C   ARGUMENTS
C   IN
C     KWHOLE(MA,NA)   Integer*2    The large array
C     MA              Real         The X size of the array
C     NA              Real         The Y size of the array
C     BS              Real         The BSCALE of the array
C     BZ              Real         The BZERO of the array
C     IX1             Integer      The X start of the area to be atken
C     IY1             Integer      The Y start of the area to be taken
C     XD              Real         The fractional pixel shift to be applied
C     YD              Real         The fractional pixel shift to be applied
C   IN/OUT
C     PART(MA,NA)     Real         The array to add to
C     YY(MA,NA)       Real         Working space
C
C   CALLS
C     Grasp
C       POLFIT
C
C   USES
C     Integer*2 arrays
C
C   A J PENNY            RGO                           83-2-18
C
C
C -------------------------------------------------------------
C
C
C
      SUBROUTINE COPYR(KWHOLE,MA,NA,BS,BZ,PART,M,N,IX1,IY1,XD,
     +                 YD,YY)
C
C
C
      REAL PART(M,N),YY(M,N)
      INTEGER*2 KWHOLE(MA,NA)
      DOUBLE PRECISION SD(4),X(4),Y(4),XA,YA,XAA,YAA,CHI,RMS,A(10)
C
C  Set up interpolation factors
C
      XA = 2.0 + XD
      XAA = XA*XA
      YA = 2.0 + YD
      YAA = YA*YA
      DO K = 1,4
         X(K) = K
         SD(K) = 1.0
      ENDDO
C
C  Copy section of input array into work area, scaling it and
C  finding max value
C
      YM = KWHOLE(IX1,IY1)*BS + BZ
      KA = IY1
      DO K = 1,N
         JA = IX1
         DO J = 1,M
            YZ = KWHOLE(JA,KA)*BS + BZ
            IF (YZ.GT.YM) YM = YZ
            YY(J,K) = YZ
            JA = JA + 1
         ENDDO
         KA = KA + 1
      ENDDO
C
C  Scale work array so max value = 1 (needed so POLFIT works)
C
      DO K = 1,N
         DO J = 1,M
            YY(J,K) = YY(J,K)/YM
         ENDDO
      ENDDO
C
C  Interpolate inside work array in X direction
C
      DO K = 2,N-2
         YZA = YY(1,K)
         DO J = 2,M-2
            Y(1) = YY(J-1,K)
            Y(2) = YY(J,K)
            Y(3) = YY(J+1,K)
            Y(4) = YY(J+2,K)
            CALL POLFIT(X,Y,SD,4,3,0,A,CHI,RMS)
            YZ = A(1) + A(2)*XA + A(3)*XAA
            YY(J-1,K) = YZA
            YZA = YZ
         ENDDO
      ENDDO
C
C  Interpolate inside work array in Y direction
C
      DO J = 2,M-2
         YZA = YY(J,1)
         DO K = 2,N-2
            Y(1) = YY(J,K-1)
            Y(2) = YY(J,K)
            Y(3) = YY(J,K+1)
            Y(4) = YY(J,K+2)
            CALL POLFIT(X,Y,SD,4,3,0,A,CHI,RMS)
            YZ = A(1) + A(2)*YA + A(3)*YAA
            YY(J,K-1) = YZA
            YZA = YZ
         ENDDO
      ENDDO
C
C  Add work array (scaled back from=1) to output array
      DO K = 1,N
         DO J = 1,M
            PART(J,K) = PART(J,K) + YY(J,K)*YM
         ENDDO
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R STARRJ *
C      *            *
C      **************
C
C
C
C   PURPOSE
C    To take a list of star mags,radii,iterations, and invalid points
C    and decide which are of acceptable quality.
C
C   ARGUMENTS
C   IN/OUT
C      RES(N,LSTLEN)   Real     List of star parameters
C      N               Integer  No of parameters
C      LSTLEN          Integer  No of stars
C
C   STARLINK PARAMETERS
C       RADIILIM,REJECT
C
C   CALLS
C     Starlink
C       CNPAR,RDKEKI,RDKEYR,WRUSER
C
C
C   A.J.PENNY                   RGO                    83-2-22
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE STARRJ(RES,N,LSTLEN)
C
C
C
      REAL RES(N,LSTLEN),RADJ(2)
      CHARACTER*72 TEXT
C
C  Set all to be accepted
C
      DO K = 1,LSTLEN
         RES(9,K) = 0.0
      ENDDO
C
C  Reject on Magnitude,Iterations,Invalid
C
      NUM = 0
      DO K = 1,LSTLEN
         KREJ = 0
         IF (RES(8,K).GT.49.0) KREJ = 1
         IF (RES(5,K).GT.19.1) KREJ = 1
         IF (RES(6,K).GT.0.1) KREJ = 1
         IF (KREJ.EQ.1) THEN
            RES(9,K) = 1.0
            NUM = NUM + 1
            RES(7,NUM) = K
         ENDIF
      ENDDO
C
C  Reject on Radii
C
      RAD = 0.0
      RNUM = 0.0
      DO K = 1,LSTLEN
         IF (RES(9,K).LT.0.5) THEN
            RAD = RAD + RES(3,K) + RES(4,K)
            RNUM = RNUM + 2.0
         ENDIF
      ENDDO
      IF (RNUM.GT.0.1) THEN
         RAD = RAD/RNUM
         RADJ(1) = 0.5*RAD
         RADJ(2) = 1.5*RAD
      ELSE
         RADJ(1) = 1.0
         RADJ(2) = 10.0
      ENDIF
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('INPUT ACCEPTABLE RADII LIMITS',ISTAT)
      CALL RDKEYR('RADIILIM',.TRUE.,2,RADJ,K,ISTAT)
      CALL CNPAR('RADIILIM',ISTAT)
      DO K = 1,LSTLEN
         KREJ = 0
         IF(RES(3,K).LT.RADJ(1).OR.RES(3,K).GT.RADJ(2))KREJ=1
         IF(RES(4,K).LT.RADJ(1).OR.RES(4,K).GT.RADJ(2))KREJ=1
         IF (KREJ.EQ.1) THEN
            IF (RES(9,K).LT.0.5) THEN
               NUM = NUM + 1
               RES(7,NUM) = K
            ENDIF
            RES(9,K) = 1.0
         ENDIF
      ENDDO
C
C  Type out rejected stars
C
      IF (NUM.NE.0) THEN
         CALL WRUSER('REJECTED STARS ARE',ISTAT)
         KA = 1
         KB = 8
         IF (NUM.LT.8) KB = NUM
         NUMA = 0
         DO WHILE (NUMA.EQ.0)
            WRITE(TEXT,907)(RES(7,K),K=KA,KB)
  907       FORMAT(' ',8F7.0)
            CALL WRUSER(TEXT,ISTAT)
            IF (NUM.EQ.KB) NUMA = 1
            KA = KB + 1
            KB = KA + 7
            IF (NUM.LT.KB) KB = NUM
         ENDDO
      ENDIF
C
C  Find any other rejected stars
C
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('MORE STAR NOS TO REJECT (-1 TO END)',ISTAT)
      KRW = 0
      DO WHILE (KRW.EQ.0)
         CALL CNPAR('REJECT',ISTAT)
         KR = -1
         CALL RDKEYI('REJECT',.TRUE.,1,KR,K,ISTAT)
         IF(KR.GE.1.AND.KR.LE.LSTLEN) THEN
            RES(9,K) = 1.0
         ELSE
            KRW = 1
         ENDIF
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R FITOG  *
C      *            *
C      **************
C
C
C   PURPOSE
C      Types out fit
C
C   ARGUMENTS
C  IN
C     HEIGHT    Real      Height
C     BASE      Real      Base
C     XA        Real      X displacement
C     YA        Real      Y displacement
C     RMS       Real      RMS
C     RXA       Real      X radius
C     RYA       Real      Y radius
C     ITER      Integer   No of iterations
C
C
C
C   CALLS
C     Starlink
C       WRUSER
C
C
C   A.J.PENNY                   RGO                    83-3-2
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE FITOG(HEIGHT,BASE,XA,YA,RMS,RXA,RYA,ITER)
C
C
C
      CHARACTER*70 TEXT
C
C
C
      AH = HEIGHT
      AB = BASE
      ADX = XA
      ADY = YA
      ARMS = RMS
      ARX = RXA
      ARY = RYA
      IF (ABS(AH).GT.9999999.0) AH = SIGN(9999999.0,AH)
      IF (ABS(AB).GT.99999.9) AB = SIGN(99999.0,AB)
      IF (ABS(ADX).GT.99.0) ADX = SIGN(99.0,ADX)
      IF (ABS(ADY).GT.99.0) ADY = SIGN(99.0,ADY)
      IF (ABS(ARMS).GT.99999.0) ARMS = SIGN(99999.0,ARMS)
      IF (ABS(ARX).GT.99.0) ARX = SIGN(99.0,ARX)
      IF (ABS(ARY).GT.99.0) ARY = SIGN(99.0,ARY)
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('MEAN FIT IS',ISTAT)
      CALL WRUSER('   HEIGHT     BASE    X    Y     RMS   ITER   RX
     +  RY',ISTAT)
      WRITE (TEXT,901) AH,AB,ADX,ADY,
     +                 ARMS,ITER,ARX,ARY
  901 FORMAT (1H ,F10.1,F9.1,2F5.1,F8.1,I3,4X,2F6.2)
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER(' ',ISTAT)
      SEEING = (RXA+RYA)/2.0
      SEEING = 1.65*SEEING
      WRITE(TEXT,902)SEEING
  902 FORMAT(' SEEING (FWHM) = ',F6.2)
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER(' ',ISTAT)
C
C
C
      END



