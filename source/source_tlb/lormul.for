C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program  LORMUL  *
C                     *                  *
C                     ********************
C
C
C
C
C
C          FUNCTION:-
C               It performs photometry of stars in an image. A Lorentz
C               star profile and an XY list of star positions are fed
C               in. The program then estimates the magnitudes and accurate
C               positions of those stars. It can deal to some extent
C               with crowded stars. It does this by applying, to single
C               stars or groups of stars, a proper iterative
C               least squares fitting with a function of a sloping plane
C               background and up to eight Lorentzian fully 2D star profiles.
C               The magnitudes are typed out and the entire fit analysis
C               is stored in  a file for any further work. This may be
C               formatted for printing by using the program PRLORMUL.
C                 The program stores output as it goes along, so a crash
C               is not fatal. You can also rerun the program using the
C               part completed output as input so it doesnt have to do
C               those stars again
C
C               The actual order of fitting is to start with the brightest
C               star, solve it, remove it from the image and go onto
C               the next brightest. At the end the process is repeated on
C               the now depleted image for all the stars where the fit
C               failed before, hoping that the removal of stars will have
C               helped. At the end the depleted image is output for
C               your inspection.
C               You may select to only solve for the N brightest stars
C               and/or to omit the redo loop.
C
C
C          USE:-
C               It may be used to do photometry of crowded fields. The
C               method is straightforward but time consuming. First a very
C               rough estimate of the magnitude of each star is made by
C               looking for the peak value within one radius, then the
C               list is looked at to see if any of the stars have nearby
C               stars which (given their distance in terms of the star
C               profile and distance and relative magnitude) will affect
C               the star. This is done as :-
C
C  Go through the XY list and find for each star its importance
C  according to the formula
C
C             Imp = mag(star) - mag(main star) - 6 + d/(star radius)
C
C  Thus the more important the star is the more -ve Imp is. If the
C  star is important enough to worry about (Imp less than 0 or closer
C  than 3.0 radii if not otherwise Important), then
C  the star is noted with the more
C  important stars first. If more than seven stars are important
C  enough to be noted, only the seven most important are noted
C
C               Then the group of stars are fitted together and the
C               result for the main star noted. It is then removed from
C               the image, and the process repeated for the next star.
C
C
C               The star profile is in the Lorentz form :-
C
C                                    1
C                          I = ---------------
C                                     P(1+d2)
C                               1 + d1
C
C               Where d1 = sqrt((X/RX)**2+(Y/RY)**2)
C               and   d2 = sqrt((X/PRX)**2+(Y/PRY)**2)
C
C
C
C         USER PARAMETERS:-
C
C          IMAGE                              This is the name of the image
C                                             containing the stars. As with
C                                             the other EDRS  programs  the
C                                             I*2 incarnation is used.
C
C          XYLIST                             The name of the star position
C                                             list file. (An EDRS XY format
C                                             file)
C          XYBEFORE                           The name of any previous
C                                             run through of LORMUL on this
C                                             file. If there was one, LORMUL
C                                             starts from where the previous
C                                             run ended. If there wasnt,
C                                             just press 'return'.
C
C
C          OUTPUT                             This is  the  name
C                                             of  the  .BDF  file  used  to
C                                             store the results.
C
C          TITLE      Output from LORMUL      This is the title  to
C                                             be associated with the output
C                                             file.
C
C          RX            LORFIT_RX            This is the 'radius' in the
C                        (4 if undefined)     X direction of the star
C                                             This is used as the input
C                                             to the calculation. If the
C                                             profile is fixed, it is kept
C                                             constant.
C
C          RY            LORFIT_RY            The same in the Y direction
C                        (RX if undefined)
C
C          P             LORFIT_P             This is the power in the star
C                        (2 if undefined)     profile. (fixed or variable)
C
C          PRX           LORFIT_PRX           This is the scale in the X
C                        (7*RX if undefined)  direction for modifying the
C                                             power in the star profile
C                                             function. (fixed or variable)
C
C          PRY           LORFIT_PRY           This is the same in Y
C                        (7*RY if undefined)  profile function
C                                             (fixed or variable)
C
C          NUMBER        All                  The number of stars to try
C                                             in order of brightness.
C                                             (stars more than 1 radius
C                                             outside the image are not
C                                             tried anyway)
C
C          REDO          Yes                  Flag for redoing the failed
C                                             stars.
C
C
C          IMOUT                              Image left after stars have
C                                             been removed
C
C          IMOTIT                             Title of output image
C
C
C       USUALLY DEFAULTED PARAMETERS
C
C          PARFIX          YES                The flag for using fixed or
C                                             varaible profile. Choices are
C                                             only YES for fixed, as putting
C                                             NO still results in yes
C
C          VOLCAL          YES                Flag for calculating the mags
C                                             as height*standard profile
C                                             volume. Putting NO again has no
C                                             effect.
C
C
C
C
C
C
C
C
C
C           A J Penny   RGO                            28-JUL-82
C
C
C--------------------------------------------------------------------------



C     Aspic program    *** LORMUL***
C     The fitting subroutine work was done by John Straede at AAO
C     in the course of the Vela Pulsar work, following a
C     line of a simple program by AJPenny
C     This version written by KFHartley at RGO on 16/10/81
C     Modified extensively by KFH during March/April 82
C     Extensively modified by AJPenny during April/May 82
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      REAL PROF(5),RRVAL(24,1)
      INTEGER NSIZE(2)
      LOGICAL VALID,PARFIX,VOLCAL
      CHARACTER*72 TXT
      CHARACTER TEXT*72,CVAL*1,TITLE*30,ATITLE*72
C
C   Define the function VOLUME
C
      EXTERNAL VOLUME
C
C   Set continuation flag
C
      VALID = .TRUE.
C
C --------------------------------------------------------------
C
C   Get the image data array
C
      CALL GTIMG(IPIN,NSIZE,BS,BZ,INVAL,ATITLE,IERR)
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
            CALL WRERR('HELLXY')
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C   Open working areas as:-
C   IPX,IPY for the star X,Y positions
C   IPMAG,IPH,IPHA,IPHB for estimated star mags and heights
C   IPNST for the companion list for each star and the done flag
C   IPD for the working image
C
      IF (VALID) THEN
         NXNY = NSIZE(1)*NSIZE(2)
         CALL GETDYN('AREA',FMT_SW,NXNY,IPD,ISTATD)
         CALL GETDYN('XPOSN',FMT_R,LSTLEN,IPX,ISTATX)
         CALL GETDYN('YPOSN',FMT_R,LSTLEN,IPY,ISTATY)
         CALL GETDYN('MAGS',FMT_R,LSTLEN,IPMAG,ISTATM)
         CALL GETDYN('HTS',FMT_R,LSTLEN,IPH,ISTATH)
         CALL GETDYN('HTSA',FMT_R,LSTLEN,IPHA,ISTATI)
         CALL GETDYN('HTSB',FMT_SL,LSTLEN,IPHB,ISTATJ)
         LSTA = LSTLEN*9
         CALL GETDYN('NSTS',FMT_SL,LSTA,IPNST,ISTATN)
         IF ((ISTATD.NE.ERR_NORMAL).OR.
     +       (ISTATX.NE.ERR_NORMAL).OR.
     +       (ISTATY.NE.ERR_NORMAL).OR.
     +       (ISTATM.NE.ERR_NORMAL).OR.
     +       (ISTATH.NE.ERR_NORMAL).OR.
     +       (ISTATI.NE.ERR_NORMAL).OR.
     +       (ISTATJ.NE.ERR_NORMAL).OR.
     +       (ISTATN.NE.ERR_NORMAL)) THEN
            CALL WRERR('HELLW')
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C -----------------------------------------------------------------
C
C  Now open a file in the EDRS XY list format to store the results in
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,29,LSTLEN,IPOUT,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
C
C  Transfer the identifiers
C
            CALL EXTLSA(%VAL(IPXY),NIPXY,LSTLEN,1,LSTLEN,1,5,
     +                  %VAL(IPOUT),29,LSTLEN,1,1)
C
C Now store the file descriptors in that file
C
            CALL PTDSCR('OUTPUT','NITEM','INTEGER',29,RVAL,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,
     +                  CVAL,IERR)
            TITLE='Output from LORMUL'
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,I,IERR)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',
     +                  IVAL,RVAL,TITLE,IERR)
         ELSE
C
C  If cannot open file, write error message
C
            CALL WRUSER('OUTPUT FILE INVALID',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C --------------------------------------------------------------
C
C  Bring in any previous measures.
C  If no previous run, set up a false flagged magnitude one.
C
      IF (VALID) THEN
         CALL GTXYLR('XYBEFORE',.TRUE.,NITEMA,LSTLNA,IPBEF,ISTAT)
         IF (ISTAT.EQ.1) THEN
            CALL GETDYN('BEF',FMT_R,LSTLEN*29,IPBEF,ISTATB)
            IF (ISTATB.NE.ERR_NORMAL) THEN
               VALID = .FALSE.
            ELSE
               RRVAL(1,1) = 9.0E20
               DO K = 2,24
                  RRVAL(K,1) = 0.0
               ENDDO
               DO K = 1,LSTLEN
                  CALL EXTLSA(RRVAL,24,1,1,1,1,24,
     +                        %VAL(IPOUT),29,LSTLEN,K,6)
                  CALL EXTLSA(RRVAL,24,1,1,1,1,24,
     +                        %VAL(IPBEF),29,LSTLEN,K,6)
               ENDDO
            ENDIF
         ELSE
            IF (LSTLNA.NE.LSTLEN.OR.NITEMA.NE.29) THEN
               CALL WRUSER('INVALID BEFORE LIST',IERR)
               VALID = .FALSE.
            ELSE
               CALL EXTLSA(%VAL(IPBEF),29,LSTLEN,1,LSTLEN,1,29,
     +                     %VAL(IPOUT),29,LSTLEN,1,1)
            ENDIF
         ENDIF
      ENDIF
C
C  Get the star profile parameters
C
      IF (VALID) THEN
         CALL GTPROF(PROF,PARFIX,VOLCAL,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C  Get number to do
C
      IF (VALID) THEN
         NUMBER = LSTLEN
         ALST = REAL(LSTLEN) + 0.5
         CALL GETPAR('NUMBER','INTEGER',1,0.5,ALST,.TRUE.,NUMBER,
     +               RVAL,ISTAT)
         IF (ISTAT.NE.0) VALID = .FALSE.
      ENDIF
C
C  Get flag for redoing
C
      IF (VALID) THEN
    1    KREDO = 1
         CALL GETCMD('REDO','YES,NO,HELP,?.',.TRUE.,KREDO,
     +               TEXT,KTEXT,ISTAT)
         IF (KREDO.EQ.3.OR.KREDO.EQ.4) THEN
            CALL WRUSER('Choices are :-',ISTAT)
            CALL WRUSER('YES      Redo failed fit stars',ISTAT)
            CALL WRUSER('NO       Do not redo',ISTAT)
            CALL CNPAR('REDO',ISTAT)
            GO TO 1
         ENDIF
      ENDIF
C
C  Get Output star subtracted image
C
      IF (VALID) THEN
         CALL GT2DIW('IMOUT',102,.FALSE.,NSIZE(1),NSIZE(2),IPIM,IERR)
         IF (IERR.NE.0) THEN
            VALID = .FALSE.
            CALL WRUSER('CANT GET IMAGE',ISTAT)
         ELSE
            TITLE = 'Output from LORMUL'
            CALL RDKEYC('IMOTIT',.TRUE.,1,TITLE,NVAL,ISTAT)
            CALL PTDSCR('IMOUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +                 IERR)
            CALL PTDSCR('IMOUT','INVAL','INTEGER',INVAL,RVAL,CVAL,IERR)
            CALL PTDSCR('IMOUT','BSCALE','REAL',IVAL,BS,CVAL,IERR)
            CALL PTDSCR('IMOUT','BZERO','REAL',IVAL,BZ,CVAL,IERR)
         ENDIF
      ENDIF
C
C -------------------------------------------------------------
C
      IF (VALID) THEN
C
C  Estimate rough magnitudes  for the stars by a single Gaussian
C  iteration at the XYLIST position. Transfer the X,Y positions
C  and magnitudes to the IPX,IPY,IPMAG areas
C
         CALL ESTMAG(%VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,INVAL,PROF,
     +               GAURX,GAURY,%VAL(IPXY),NIPXY,LSTLEN,%VAL(IPX),
     +               %VAL(IPY),%VAL(IPMAG),%VAL(IPH),%VAL(IPHA),
     +               %VAL(IPHB))
C
C  Sort through the star list finding any close, bright companions to
C  each star. Transfer these to the IPNST area
C
         CALL DOLIST(%VAL(IPX),%VAL(IPY),%VAL(IPMAG),GAURX,GAURY,LSTLEN,
     +              %VAL(IPNST))
C
C  Copy image into working area
C
         CALL TRANS(%VAL(IPIN),NSIZE(1),NSIZE(2),%VAL(IPD))
C
C  Now do the actual work of fitting Lorentzians to the stars, then
C  storing the results in the Output file
C
         CALL DOMAGS(%VAL(IPD),NSIZE(1),NSIZE(2),BS,BZ,INVAL,PROF,
     +               GAURX,GAURY,%VAL(IPX),%VAL(IPY),%VAL(IPNST),
     +               %VAL(IPH),LSTLEN,%VAL(IPOUT),29,
     +               %VAL(IPHB),%VAL(IPBEF),NUMBER,KREDO)
C
C  Store star subtracted image
C
         CALL TRANS(%VAL(IPD),NSIZE(1),NSIZE(2),%VAL(IPIM))
C
C
C
      ENDIF
C
C --------------------------------------------------------------
C
C  Release the temporary working areas and exit from program
C
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        ****************
C        *              *
C        * S/R  ESTMAG  *
C        *              *
C        ****************
C
C  This subroutine goes through an XY list of star positions
C  and finds rough magnitudes of stars in that list in the
C  data array input
C
      SUBROUTINE ESTMAG(KPT,NPIX,NLINE,BS,BZ,INVAL,PROF,GAURX,
     +                  GAURY,XYLIST,NXY,LSTLEN,XA,YA,BMAG,HTS,
     +                  HTSA,KHT)
C
C
C
      INTEGER*2 KPT(NPIX,NLINE),KP(30,30)
      REAL XYLIST(NXY,LSTLEN),XA(LSTLEN),YA(LSTLEN),BMAG(LSTLEN)
      REAL HTS(LSTLEN),HTSA(LSTLEN)
      REAL A(4,4),B(4)
      INTEGER KHT(LSTLEN)
      REAL PROF(5)
      CHARACTER TEXT*72
C
C ----------------------------------------------------------
C
C  Take the standard profile and find the X,Y radii of the best
C  fitting Gaussian
C  This also gives the size of the box around each star needed
C
C
C  Make a false star from the standard profile
C
      DO K = 1,30
         DY = REAL(K) - 15.5
         AY = DY/PROF(2)
         BY = DY/PROF(5)
         DO J = 1,30
            DX = REAL(J) - 15.5
            AX = DX/PROF(1)
            BX = DX/PROF(4)
            AR = SQRT(AX*AX+AY*AY)
            BR = SQRT(BX*BX+BY*BY)
            F = 1.0 + AR**(PROF(3)*(1.0+BR))
            KP(J,K) = 100 + INT(100.0/F)
         ENDDO
      ENDDO
C
C  Fit a Gaussian to that
C
      CALL AGAUSS(KP,30,30,16.0,16.0,30,30,0,A1,A2,INVAL,20,
     +            A3,A4,A5,A6,A7,A8,A9,GAURX,GAURY,A10,K1,K2)
C
C  Type result and calculate size of box needed
C
      CALL WRUSER(' ',ISTAT)
      WRITE(TEXT,901)GAURX,GAURY
  901 FORMAT(1H ,'GAUSSIAN RADII USED IN FINDING COMPANIONS =  ',2F7.2)
      CALL WRUSER(TEXT,IERR)
C
C ----------------------------------------------------------------
C
C  Transfer positions to work lists
C
      DO L = 1,LSTLEN
         XA(L) = XYLIST(6,L)
         YA(L) = XYLIST(7,L)
      ENDDO
C
C  Get rough mags and heights by finding max near posn
C  above sky.
C
      KRX = GAURX
      KRY = GAURY
      LX = 2.0*GAURX + 1
      LY = 2.0*GAURY + 1
      CALL IMASPA(KPT,NPIX,NLINE,1,NPIX,1,NLINE,INVAL,SKY,STD,
     +            KN)
      KSKY = SKY
      DO L = 1,LSTLEN
         JA = XYLIST(6,L)
         KA = XYLIST(7,L)
         NVAL = 0
         MAX = 0
         DO K = 1,LX
            DO J = 1,LY
               JB = JA - KRX + J - 1
               KB = KA - KRY + K - 1
               IF ((JB.GE.1.AND.JB.LE.NPIX).AND.
     +             (KB.GE.1.AND.KB.LE.NLINE)) THEN
                  IF(KPT(JB,KB).NE.INVAL) THEN
                     NVAL = NVAL + 1
                     KD = KPT(JB,KB) - KSKY
                     IF (KD.GT.MAX) MAX = KD
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         IF (NVAL.GT.6) THEN
            IF (MAX.GT.1) THEN
               AMAG = 20.0 - 2.5*ALOG10(REAL(MAX))
            ELSE
               AMAG = 50.0
               MAX = 2
            ENDIF
         ELSE
            AMAG = 60.0
            MAX = 2
         ENDIF
         BMAG(L) = AMAG
         HTS(L) = MAX
      ENDDO
C
C -------------------------------------------------------------------
C
C  Find the mean magnitude of the stars successfully fitted, and then
C  reset any magnitudes which are more than 5 mags away from the mean
C  to those limits.
C  And do same for heights
C  Also if the  fit has failed, set the magnitudes to the
C  faintest magnitude (ie mean + 5)
C  If all points INVALID set to brightest, as may be v bright star.
C  If outside edge, set to faintest
C  at edge instead doesnt matter.
C
      S = 0.0
      SH = 0.0
      NS = 0
      DO K = 1,LSTLEN
         IF(BMAG(K).LT.49.0) THEN
            S = S + BMAG(K)
            SH = SH + HTS(K)
            NS = NS + 1
         ENDIF
      ENDDO
      IF (NS.EQ.0) THEN
         AMEAN = 20.0
         HMEAN = 500.0
         CALL WRUSER('NO ROUGH MAGNITUDES CAN BE MEASURED',IERR)
      ELSE
         AMEAN = S/REAL(NS)
         HMEAN = SH/REAL(NS)
      ENDIF
      AMAX = AMEAN + 5.0
      AMIN = AMEAN - 10.0
      HMIN = HMEAN/100.0
      HMAX = HMEAN*10000.0
C
      DO K = 1,LSTLEN
C
         AMAG = BMAG(K)
         AH = HTS(K)
C
         IF (BMAG(K).GT.49.0) THEN
            AMAG = AMAX
            AH = HMIN
         ENDIF
         IF (BMAG(K).GT.59.0) THEN
            AMAG = AMIN
            AH = HMAX
         ENDIF
C
         IF (AMAG.GT.AMAX) THEN
            AMAG = AMAX
            AH = HMIN
         ENDIF
         IF (AMAG.LT.AMIN) THEN
            AMAG = AMIN
            AH = HMAX
         ENDIF
C
         IF (XA(K).LT.1.OR.XA(K).GT.NPIX.OR.
     +       YA(K).LT.1.OR.YA(K).GT.NLINE) THEN
            AMAG = AMAX
            AH = HMIN
         ENDIF
C
         BMAG(K) = AMAG
         HTS(K) = AH
C
      ENDDO
C
C    Sort stars in brightest to faintest
C
      CALL SORTL(HTS,HTSA,LSTLEN,KHT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          **************
C          *            *
C          * S/R DOLIST *
C          *            *
C          **************
C
C
C              This subroutine reads through a list of star XY positions
C              and magnitudes, together with the star profile, and
C              works out which stars have companions close enough to
C              affect photometry. It makes for each star a list of those
C              stars which are close enough.
C
C
      SUBROUTINE DOLIST(XA,YA,BMAG,GAURX,GAURY,LSTLEN,NSTS)
C
C
C
      REAL XA(LSTLEN),YA(LSTLEN),BMAG(LSTLEN)
      INTEGER NSTS(LSTLEN,9)
      REAL DMES(8)
C
C  Do for each star
C
      DO K = 1,LSTLEN
C
C  Put the star itself as the first one and clear the other 7 spaces
C  in the list. (The ninth space is for the done flag)
C
         NSTS(K,1) = K
         DMES(1) = 0.0
         DO LA = 2,8
            NSTS(K,LA) = 0
            DMES(LA) = 1.0
         ENDDO
C
C  Go through the XY list and find for each star its importance
C  according to the formula
C
C             Imp = mag(star) - mag(main star) - 6 + d/(star radius)
C
C  Thus the more important the star is the more -ve Imp is. If the
C  star is important enough to worry about (Imp less than 0 or closer
C  than 3.0 radii if not otherwise Important), then
C  the star is inserted in the main star list with the more
C  important stars first. If more than seven stars are important
C  enough to get on the list, only the seven most important are saved
C
         DO LB = 1,LSTLEN
            IF(LB.NE.K) THEN
C               Calculate its importance
               DM = BMAG(LB) - BMAG(K)
               DX = (XA(LB) - XA(K))/GAURX
               DY = (YA(LB) - YA(K))/GAURY
               R = SQRT(DX*DX+DY*DY)
               DMCRIT = 6.0 - 1.0*R
               DME = DM - DMCRIT
               IF (DME.GE.0.0.AND.R.LT.3.0) DME = -0.01
C
C                   If it is important, put it in the main star
C                   list at the right place
C
               IF (DME.LT.0.0) THEN
                  KD = 0
                  DO LC = 2,8
                     IF(KD.EQ.0.AND.DME.LT.DMES(LC)) THEN
                        IF(LC.NE.8) THEN
                           DO LD=8,LC+1,-1
                              DMES(LD) = DMES(LD-1)
                              NSTS(K,LD) = NSTS(K,LD-1)
                           ENDDO
                        ENDIF
                        DMES(LC) = DME
                        NSTS(K,LC) = LB
                        KD = 1
                     ENDIF
                  ENDDO
               ENDIF
C
C
C
            ENDIF
         ENDDO
C
      ENDDO
C
C
C
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         **************
C         *            *
C         * S/R DOMAGS *
C         *            *
C         **************
C
C                This subroutine fits Lorentzians to stars
C                in an XY list. If the stars are flagged as having
C                close companions, the whole group is done together
C                and if in the course of doing this any of those
C                companions have themselves no further close
C                companions, and have thus been fully done, their
C                magnitudes are recorded and the stars are flagged
C                as being done, so they do not have to be done
C                again.
C                The calcs start from some beginning star, as the
C                input list has measures for the stars up to
C                that.
C                The results are stored star by star to the ouput
C                list.
C
      SUBROUTINE DOMAGS(KPT,NPIX,NLINE,BS,BZ,INVAL,PROF,GAURX,GAURY,
     +                  XA,YA,NSTS,HTS,LSTLEN,DATOUT,NITEMD,
     +                  KHT,DATIN,NUMBER,KREDO)
C
C
C
      INTEGER*2 KPT(NPIX,NLINE)
      INTEGER NSTS(LSTLEN,9),NINSTS(8),KHT(LSTLEN)
      REAL PROF(5),DATOUT(NITEMD,LSTLEN),XA(LSTLEN),YA(LSTLEN)
      REAL DATIN(NITEMD,LSTLEN)
      REAL HTS(LSTLEN)
      REAL RMS(8),CC(32)
      CHARACTER TEXT*72
C
C  Calculate the volume of the profile
C
      DO K = 1,5
         CC(K+3) = PROF(K)
      ENDDO
      VOL = VOLUME(CC)
C
C  Set up DONE flags as not done, done, or done and failed
C
      DO K = 1,LSTLEN
         NSTS(K,9) = 0
         IF (DATIN(6,K).LT.8.0E20) THEN
            NSTS(K,9) = 1
            IF (DATIN(8,K).LT.51.0.AND.DATIN(8,K).GT.49.0) THEN
               NSTS(K,9) = 2
            ENDIF
         ENDIF
      ENDDO
C
C  Type header to terminal output
C
      CALL WRUSER(' ',ISTAT)
      WRITE(TEXT,900)
  900 FORMAT(' ',' STAR','   MAG  ',' HEIGHT ','  DX  ','  DY  ',
     +       ' RMS ',' ITS ','INVAL','   COMPANION STARS')
      CALL WRUSER(TEXT,ISTAT)
C
C  Go through stars doing undone ones, fitting and removing from image
C
      DO LK = 1,NUMBER
         K = KHT(LK)
         IF (NSTS(K,9).EQ.0) THEN
            CALL LORDO(KPT,NPIX,NLINE,BS,BZ,INVAL,PROF,GAURX,GAURY,
     +                 XA,YA,NSTS,HTS,LSTLEN,K,RMS,ITER,PERMS,IX1,
     +                 IY1,CC,NINSTS)
            CALL RESULT(CC,RMS,1,DATOUT,NITEMD,LSTLEN,K,
     +                  ITER,PERMS,IX1,IY1,NSTS,VOL,XA,
     +                  YA,NINSTS)
          ELSE
C
C       Get star paramters from previous run
C
            CC(9) = DATIN(6,K)
            CC(10) = DATIN(7,K)
            CC(11) = DATIN(14,K)
         ENDIF
C
C  Remove star from image, and from companion star lists, if it
C  has been solved OK or it was done OK in previous run.
C  And flag as done or done and failed.
C
         IF ((CC(11)*VOL).GT.1.0E-8) THEN
            NSTS(K,9) = 1
            CALL REMOVE(KPT,NPIX,NLINE,BS,BZ,INVAL,CC,IX1,IY1)
            DO KK = 1,LSTLEN
               NIN = 0
               DO KA = 2,8
                  IF (NSTS(KK,KA).EQ.K) NIN = KA
               ENDDO
               IF (NIN.NE.0) THEN
                  DO KA = NIN,7
                     NSTS(KK,KA) = NSTS(KK,KA+1)
                  ENDDO
                  NSTS(KK,8) = 0
               ENDIF
            ENDDO
         ELSE
           NSTS(K,9) = 2
         ENDIF
C
C
C
      ENDDO
C
C  If wanted, redo the ones that were done and failed, as they may be
C  redoable after the removal of the OK ones.
C
      IF (KREDO.EQ.1) THEN
C  Type new header to terminal output
C
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('       REDO OF FAILED FITS',ISTAT)
C
C  Go through stars doing failed ones, fitting and removing from image
C
         DO LK = 1,LSTLEN
            K = KHT(LK)
            IF (NSTS(K,9).EQ.2) THEN
               CALL LORDO(KPT,NPIX,NLINE,BS,BZ,INVAL,PROF,GAURX,GAURY,
     +                    XA,YA,NSTS,HTS,LSTLEN,K,RMS,ITER,PERMS,IX1,
     +                    IY1,CC,NINSTS)
               CALL RESULT(CC,RMS,1,DATOUT,NITEMD,LSTLEN,K,
     +                     ITER,PERMS,IX1,IY1,NSTS,VOL,XA,
     +                     YA,NINSTS)
C
C  Remove star from image, and from companion star lists, if it
C  has been solved OK.
C
               IF ((CC(11)*VOL).GT.1.0E-8) THEN
                  CALL REMOVE(KPT,NPIX,NLINE,BS,BZ,INVAL,CC,IX1,IY1)
                  DO KK = 1,LSTLEN
                     NIN = 0
                     DO KA = 2,8
                        IF (NSTS(KK,KA).EQ.K) NIN = KA
                     ENDDO
                     IF (NIN.NE.0) THEN
                        DO KA = NIN,7
                           NSTS(KK,KA) = NSTS(KK,KA+1)
                        ENDDO
                        NSTS(KK,8) = 0
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
C
C  If only doing part of list (NUMBER less than LSTLEN), then
C  clear X posn flags of undone stars.
C
      IF (NUMBER.LT.LSTLEN) THEN
         DO K = 1,LSTLEN
            IF (NSTS(K,9).EQ.0) DATOUT(6,K) = 0.0
         ENDDO
      ENDIF
C
C
C
         ENDDO
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          *************
C          *           *
C          * S/R LORDO *
C          *           *
C          *************
C
C          This subroutine is given up to eight stars from an XY list
C          which are somewhere in an image, copies the appropriate
C          part of the frame into a working area, sets up the
C          parameters for the standard Lorentz fitting subroutine,
C          calls that subroutine, and calls the calculation for
C          each star of the goodness of fit of the result.
C
      SUBROUTINE LORDO(KPT,NPIX,NLINE,BS,BZ,INVAL,PROF,GAURX,GAURY,
     +                 XA,YA,NSTS,HTS,LSTLEN,NSTAR,RMS,ITER,PERMS,
     +                 IX1,IY1,CC,NINSTS)
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER*2 KPT(NPIX,NLINE)
      REAL XA(LSTLEN),YA(LSTLEN),HTS(LSTLEN)
      REAL CC(32),RMS(8),XP(8),YP(8),HTP(8),PROF(5)
      INTEGER NSTS(LSTLEN,9),JFIT(32),NINSTS(8)
      LOGICAL VALID,VALIDA
C
C ------------------------------------------------------------
C
C  Choose area needed for these stars and set VALID true if
C  an area can be found
C
      IX1 = NPIX
      IX2 = 1
      IY1 = NLINE
      IY2 = 1
      DX = GAURX*3.0
      DY = GAURY*3.0
      NST = 0
      DO K = 1,8
         LSTAR = NSTS(NSTAR,K)
         IF (LSTAR.NE.0) THEN
            NST = NST + 1
            KX1 = XA(LSTAR) - DX
            KX2 = XA(LSTAR) + DX
            KY1 = YA(LSTAR) - DY
            KY2 = YA(LSTAR) + DY
            IF (KX1.LT.IX1) IX1 = KX1
            IF (KX2.GT.IX2) IX2 = KX2
            IF (KY1.LT.IY1) IY1 = KY1
            IF (KY2.GT.IY2) IY2 = KY2
         ENDIF
      ENDDO
      IF (IX1.LT.1) IX1 = 1
      IF (IY1.LT.1) IY1 = 1
      IF (IX2.GT.NPIX) IX2 = NPIX
      IF (IY2.GT.NLINE) IY2 = NLINE
C
C  Check to see if box in main image
C
      VALID = .TRUE.
      IF (IX2.LE.IX1.OR.IY2.LE.IY1) VALID = .FALSE.
      IF (IX1.GE.NPIX.OR.IX2.LE.1) VALID = .FALSE.
      IF (IY1.GE.NLINE.OR.IY2.LE.1) VALID = .FALSE.
C
C
C -------------------------------------------------------
C
      IF (VALID) THEN
C      Now get work space for the data, fit and residuals.
C      If work space can be found set VALIDA true
C
         VALIDA = .TRUE.
         LX = IX2 - IX1 + 1
         LY = IY2 - IY1 + 1
         NX = LX*LY
         CALL GETDYN('DATA',FMT_R,NX,IPD,ISTAT)
         IF (ISTAT.NE.ERR_NORMAL) THEN
            CALL WRERR('HELLD')
            VALIDA = .FALSE.
         END IF
         CALL GETDYN('KDO',FMT_SW,NX,IPKDO,ISTAT)
      ENDIF
C
C -----------------------------------------------------------
C
C  Get the star positions and heights in terms of this area
C
      DO K = 1,NST
         L = NSTS(NSTAR,K)
         XP(K) = XA(L) - IX1 + 1
         YP(K) = YA(L) - IY1 + 1
         HTP(K) = HTS(L)
      ENDDO
C
C ----------------------------------------------------------
C
      IF (VALID.AND.VALIDA) THEN
C
C  Now copy the selected region into working area
C
         CALL COPYI(KPT,NPIX,NLINE,%VAL(IPD),LX,LY,
     +             IX1,IY1,IX2,IY2,BS,BZ,INVAL,NINVAL,0)
C
C  Set up CC for a fixed profile and up to 8 stars.
C
         CALL CCFILLA(%VAL(IPD),LX,LY,.TRUE.,PROF,XP,YP,HTP,NST,CC,
     +                JFIT)
C
C  Do the fit
C
         ALOW = REAL(INVAL) + 2.0
         CALL FITFST(%VAL(IPD),%VAL(IPKDO),LX,LY,CC,JFIT,NST,ITER,
     +               GAURX,GAURY,ALOW,40)
C
C  Now calculate the rms errors
C
         CALL RMSDO(%VAL(IPD),LX,LY,CC,PROF(1),PROF(2),NST,RMS,
     +               KPT,NPIX,NLINE,IX1,IY1,INVAL,NINSTS)
      ENDIF
C
C --------------------------------------------------------
C
      IF (VALID) THEN
C
C   Free the work space and return for another loop.
C
         CALL FRDATA('DATA',ISTAT)
         CALL FRDATA('KDO',ISTAT)
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R RESULT *
C      *            *
C      **************
C
C
C ------------------------------------------------------------------
C
C
C
      SUBROUTINE RESULT(CC,RMS,LK,DATOUT,NITEMD,LSTLEN,NSTAR,
     +                  ITER,PERMS,IX1,IY1,NSTS,VOL,XA,
     +                  YA,NINVAL)
C
C
C
      REAL CC(32),RMS(8),RES(24),DATOUT(NITEMD,LSTLEN)
      REAL XA(LSTLEN),YA(LSTLEN)
      INTEGER NSTS(LSTLEN,9),NINVAL(8),NSOUT(7)
      CHARACTER TEXT*72
C
C
C
      L = 9 + 3*(LK-1)
      AMAG = CC(L+2)*VOL
      IF (AMAG.GT.1.0E-8) THEN
         AMAG = 30.0-2.5*ALOG10(AMAG)
      ELSE
         AMAG = 50.0
      ENDIF
      RES(1) = CC(L) + REAL(IX1) - 1.0
      RES(2) = CC(L+1) + REAL(IY1) - 1.0
      RES(3) = AMAG
      RES(4) = RES(1) - XA(NSTAR)
      RES(5) = RES(2) - YA(NSTAR)
      RES(6) = REAL(ITER)
      RES(7) = RMS(LK)
      RES(8) = NINVAL(LK)
      RES(9) = CC(L+2)
      RES(10) = CC(1)+CC(2)*CC(L)+CC(3)*CC(L+1)
      RES(11) = PERMS
      RES(12) = CC(4)
      RES(13) = CC(5)
      RES(14) = CC(6)
      RES(15) = CC(7)
      RES(16) = CC(8)
      KK = 0
      NSTA = 0
      DO K = 1,7
         NSOUT(K) = NSTS(NSTAR,K+1)
         IF (NSOUT(K).NE.0) NSTA = NSTA + 1
      ENDDO
      RES(17) = REAL(NSTA)
      DO K = 1,7
         RES(17+K) = REAL(NSOUT(K))
      ENDDO
C
C  If failed fit, set null params for posn,rms
C
      IF (AMAG.GT.49.9.OR.RMS(LK).LT.1.0E-8) THEN
         RES(1) = XA(NSTAR)
         RES(2) = YA(NSTAR)
         RES(4) = 0.0
         RES(5) = 0.0
         RES(7) = 0.0
      ENDIF
C
C  Prevent any overflow problems later
C
      DO K = 1,24
         AR = RES(K)
         IF (ABS(AR).GT.999999.0) AR = SIGN(999999.0,AR)
         RES(K) = AR
      ENDDO
C
C  Store the result
C
      DO K = 1,24
         KA = K + 5
         DATOUT(KA,NSTAR) = RES(K)
      ENDDO
C
C  Type out some fit values
C
      AH = CC(L+2)
      ADX = RES(4)
      ADY = RES(5)
      ARMS = RES(7)
      IF (ABS(AH).GT.99999.0) AH = SIGN(99999.0,AH)
      IF (ABS(ADX).GT.999.0) ADX = SIGN(999.0,ADX)
      IF (ABS(ADY).GT.999.0) ADY = SIGN(999.0,ADY)
      IF (ARMS.GT.9999.0) ARMS = 9999.0
      IF (NSTA.GE.5) NSTA = 5
      IF (NSTA.EQ.0) THEN
         WRITE (TEXT,900) NSTAR,AMAG,AH,ADX,ADY,
     +                    ARMS,ITER,NINVAL(LK)
      ELSE
         WRITE (TEXT,900) NSTAR,AMAG,AH,ADX,ADY,
     +                    ARMS,ITER,NINVAL(LK),
     +                    (NSOUT(K),K=1,NSTA)
      ENDIF
  900 FORMAT (1H ,I5,F7.2,F8.1,2F6.1,F6.1,I3,I5,5I5)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        ***************
C        *             *
C        * S/R RMSDO   *
C        *             *
C        ***************
C
C ----------------------------------------------------------------
C
C
C
      SUBROUTINE RMSDO(DATA,NX,NY,CC,XHALF,YHALF,NST,RMS,
     +                  KPT,NPIX,NLINE,IX1,IY1,INVAL,NINSTS)
C
C
C
      REAL DATA(NX,NY),CC(32),RMS(8)
      DOUBLE PRECISION SUM
      INTEGER*2 KPT(NPIX,NLINE)
      INTEGER NINSTS(8)
C
C
C
      PP = CC(6)/2.0
      GGX = 1.0/(CC(4)*CC(4))
      GGY = 1.0/(CC(5)*CC(5))
      HHX = 1.0/(CC(7)*CC(7))
      HHY = 1.0/(CC(8)*CC(8))
C
C
C
      DX = ABS(2.0*XHALF)
      IF (DX.LT.1.0) DX = 1.0
      DY = ABS(2.0*YHALF)
      IF (DY.LT.1.0) DY = 1.0
      DO K = 1,NST
         I = 9 + 3*(K-1)
         X = CC(I)
         Y = CC(I+1)
         AX1 = X - DX
         AX2 = X + DX
         AY1 = Y - DY
         AY2 = Y + DY
         KX1 = 1
         IF (AX1.GT.REAL(NX)) KX1 = NX
         IF (AX1.GE.1.0.AND.AX1.LE.REAL(NX)) KX1 = AX1
         KX2 = 1
         IF (AX2.GT.REAL(NX)) KX2 = NX
         IF (AX2.GE.1.0.AND.AX2.LE.REAL(NX)) KX2 = AX2
         KY1 = 1
         IF (AY1.GT.REAL(NY)) KY1 = NY
         IF (AY1.GE.1.0.AND.AY1.LE.REAL(NY)) KY1 = AY1
         KY2 = 1
         IF (AY2.GT.REAL(NY)) KY2 = NY
         IF (AY2.GE.1.0.AND.AY2.LE.REAL(NY)) KY2 = AY2
         IF (KX2.GT.KX1.AND.KY2.GT.KY1) THEN
            SUM = 0.0
            NIN = 0.0
            DO LY = KY1,KY2
               DO LX = KX1,KX2
                  Z = CC(1) + X*CC(2) + Y*CC(3)
                  DO J = 1,NST
                     JJ = 6 + 3*J
                     DCX = REAL(LX) - CC(JJ)
                     DCY = REAL(LY) - CC(JJ+1)
                     DDX = DCX*DCX
                     DDY = DCY*DCY
                     RG = DDX*GGX + DDY*GGY
                     RH = DDX*HHX + DDY*HHY
                     IF (RG.LT.0.000001) RG = 0.000001
                     IF (RH.LT.0.000001) RH = 0.000001
                     RH = SQRT(RH)
                     V = CC(JJ+2)/(1.0+(RG)**(PP*(1.0+RH)))
                     Z = Z + V
                  ENDDO
                  RES = DATA(LX,LY) - Z
                  SUM = SUM + RES*RES
                  LXA = LX + IX1 - 1
                  LYA = LY + IY1 - 1
                  IF (KPT(LXA,LYA).EQ.INVAL) NIN = NIN + 1
               ENDDO
            ENDDO
            NP = (KX2-KX1+1)*(KY2-KY1+1)
            RMS(K) = SNGL(SQRT(SUM/DBLE(NP-1)))
            NINSTS(K) = NIN
         ELSE
            RMS(K) = 0.0
            NINSTS(K) = 0
         ENDIF
      ENDDO
C
C
C
      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               ***************
C               *             *
C               * S/R CCFILLA *
C               *             *
C               ***************
C
C
C  This takes the profile parameters and star (x,y,height) parameters
C  and loads them into CC and JFIT for use in the Lorentz 2-D profile
C  fitting subroutines.
C
C   Parameters
C      INPUT
C        DATA    This array contains part of the full array
C        NX      This is the first dimension of DATA
C        NY      This the second dimension of DATA
C        PARFIX  This flags wether the profile parameters are fixed
C                or floating
C        PROF    This contains the star profile
C        X       The X coords of the stars in the DATA area
C        Y       The Y coords of the stars in the DATA area
C        HT      The approx heights of the stars
C
C   INPUT/OUTPUT
C
C        NSTAR   The number of stars (1 to 8; if outside that a single
C                star at the centre is assumed and NSTAR returned as 1)
C
C     OUTPUT
C        CC      This array will contain the initial values for
C                background (1,2,3)
C                star profile (4,5,6,7,8)
C                and star posns and heights (9,10,...,32)
C        JFIT    This holds flags as to wether the parameters in CC
C                are fixed (-1)
C                not being used (0)
C                are allowed to vary (1)
C
C   A.J.Penny                                          83-7-29
C --------------------------------------------------------------
C
C
C
      SUBROUTINE CCFILLA(DATA,NX,NY,PARFIX,PROF,X,Y,HT,NSTAR,CC,JFIT)
C
C
C
      REAL PROF(5),CC(32),DATA(NX,NY),X(8),Y(8),HT(8)
      INTEGER JFIT(32)
      LOGICAL PARFIX
C
C   Estimate the background parameters from the edge of the array
C
      S = 0.0
      DO K = 1,3
         DO J = 1,3
            S = S + DATA(J,K)
         ENDDO
      ENDDO
      CC(1) = S/9.0
      S = 0.0
      DO K = 1,3
         DO J = NX-2,NX
            S = S + DATA(J,K)
         ENDDO
      ENDDO
      CC(2) = ((S/9.0)-CC(1))/REAL(NX-1)
      S = 0.0
      DO K = NY-2,NY
         DO J = 1,3
            S = S + DATA(J,K)
         ENDDO
      ENDDO
      CC(3) = ((S/9.0)-CC(1))/REAL(NY-1)
      JFIT(1)=1
      JFIT(2)=1
      JFIT(3)=1
C
C  Set the profile parameters to be fixed or to be variable
C  If variable set at input starting values
C
      IF (PARFIX) THEN
         DO K = 4,8
            CC(K) = PROF(K-3)
            JFIT(K) = -1
         ENDDO
      ELSE
         DO K = 4,8
            CC(K) = PROF(K-3)
            JFIT(K) = 1
         ENDDO
      ENDIF
C
C  If NSTAR outside range 1 to 8, assume a single star at centre
C
      IF (NSTAR.LT.1.OR.NSTAR.GT.8) THEN
         NSTAR = 1
         X(1) = REAL(NX)/2.0 + 0.5
         Y(1) = REAL(NY)/2.0 + 0.5
      ENDIF
C
C  Put in star positions and rough heights and flag these to be fitted
C
      DO L = 1,NSTAR
         LD = (L-1)*3
         CC(9+LD) = X(L)
         CC(10+LD) = Y(L)
         CC(11+LD) = HT(L)
         JFIT(9+LD) = 1
         JFIT(10+LD) = 1
         JFIT(11+LD) = 1
      ENDDO
C
C  Set other stars to be not done
C
      IF (NSTAR.LT.8) THEN
         DO K = 12+LD,32
            CC(K) = 0.0
            JFIT(K) = 0
         ENDDO
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R TRANS  *
C      *            *
C      **************
C
C
C   PURPOSE
C      Copy a 2D I*2 image to another
C
C   ARGUMENTS
C  IN
C    IN   Int*2(NX,NY)   Input image
C    NX   Int            X size
C    NY   Int            Y size
C  OUT
C    KOUT Int*2(NX,NY)   Output image
C
C   STARLINK PARAMETERS
C
C   CALLS
C
C   USES
C     I*2 arrays
C
C
C   A.J.PENNY                   RGO                    83-7-31
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE TRANS(IN,NX,NY,KOUT)
C
C
C
      INTEGER*2 IN(NX,NY),KOUT(NX,NY)
C
C
C
      DO K = 1,NY
         DO J = 1,NX
            KOUT(J,K) = IN(J,K)
         ENDDO
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R SORTL  *
C      *            *
C      **************
C
C
C    A J PENNY               RGO                      82-11-4
C ------------------------------------------------------------
C
C
C
      SUBROUTINE SORTL(VALIN,VAL,LSTLEN,KHT)
C
C
C
      REAL VALIN(LSTLEN),VAL(LSTLEN)
      INTEGER KHT(LSTLEN)
C
C
C
      DO K = 1,LSTLEN
         KHT(K) = K
         VAL(K) = VALIN(K)
      ENDDO
C
C
C
      DO I = LSTLEN,2,-1
         DO J = 1,I-1
            JG = J + 1
            IF (VAL(J).LT.VAL(JG)) THEN
               VK = VAL(J)
               VAL(J) = VAL(JG)
               VAL(JG) = VK
               K = KHT(J)
               KHT(J) = KHT(JG)
               KHT(JG) = K
            ENDIF
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
C      * S/R REMOVE *
C      *            *
C      **************
C
C
C   PURPOSE
C    Removes a Lorentz star from an I*2 image
C
C   ARGUMENTS
C  IN
C  IN/OUT
C  OUT
C
C   STARLINK PARAMETERS
C
C
C
C   CALLS
C     Starlink
C     Aspic
C     Edrs
C     Grasp
C     This file
C
C   USES
C     I*2 arrays
C     %VAL facility
C     Byte arrays
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-7-31
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE REMOVE(IM,NX,NY,BS,BZ,INVAL,CC,KX,KY)
C
C
C
      INTEGER*2 IM(NX,NY)
      REAL CC(32)
C
C
C
      H = CC(11)/BS
      AX = 0.0
      HA = H
      DO WHILE (HA.GT.0.2)
         AX = AX + 1.0
         HA = H/(1.0+((AX/CC(4))**(CC(6)*(1.0+(AX/CC(7))))))
      ENDDO
      AY = 0.0
      HA = H
      DO WHILE (HA.GT.0.2)
         AY = AY + 1
         HA = H/(1.0+((AY/CC(5))**(CC(6)*(1.0+(AY/CC(8))))))
      ENDDO
C
C
C
      LXS = REAL(KX) + CC(9) - AX - 1.0
      LXE = LXS + 2*AX + 1.0
      LYS = REAL(KY) + CC(10) - AY - 1.0
      LYE = LYS + 2*AY + 1.0
      IF (LXS.LT.1) LXS = 1
      IF (LXS.GT.NX) LXS = NX
      IF (LXE.LT.1) LXE = 1
      IF (LXE.GT.NX) LXE = NX
      IF (LYS.LT.1) LYS = 1
      IF (LYS.GT.NY) LYS = NY
      IF (LYE.LT.1) LYE = 1
      IF (LYE.GT.NY) LYE = NY
C
C
C
      P = CC(6)/2.0
      RXA = CC(4)*CC(4)
      RXB = CC(7)*CC(7)
      RYA = CC(5)*CC(5)
      RYB = CC(8)*CC(8)
      DO K = LYS,LYE
         DO J = LXS,LXE
            DX = REAL(J) - (CC(9)+REAL(KX)-1.0)
            DY = REAL(K) - (CC(10)+REAL(KY)-1.0)
            DXA = DX*DX/RXA
            DXB = DX*DX/RXB
            DYA = DY*DY/RYA
            DYB = DY*DY/RYB
            DA = DXA + DYA
            DB = SQRT(DXB+DYB)
            HA = H/(1.0+(DA**(P*(1.0+DB))))
            IF (HA.GT.32767.0) IM(J,K) = INVAL
            IF (IM(J,K).NE.INVAL) THEN
               T = REAL(IM(J,K)) - HA
               IF (T.GT.32767.0.OR.T.LT.-32767.0) THEN
                  IM(J,K) = INVAL
               ELSE
                  IM(J,K) = T
               ENDIF
            ENDIF
         ENDDO
      ENDDO
C
C
C
      END



