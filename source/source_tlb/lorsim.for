C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   LORSIM *
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
C               positions of these stars. If either takes a fixed star
C               profile from the environment, or it calculates a profile
C               for each star.
C               The magnitudes are typed out and the entire fit analysis
C               is stored in  a file for any further work. This may be
C               formatted for printing by using the program PRLORSIM (qv).
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
C          XYLIST                             The star position list (It must
C                                             be in the EDRS XYLIST format.
C
C          OUTPUT                             This is  the  name
C                                             of  the  .BDF  file  used  to
C                                             store the results.
C
C          TITLE      Output from LORSIM      This is the title  to
C                                             be associated with the output
C                                             file.
C
C
C          PARFIX        NO                   The flag for fixed or variable
C                                             profiles. Choices are NO,YES
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
C          VOLCAL        YES                  The flag for the magnitudes
C                                             calculated to be multiplied by
C                                             the profile volume. (This is
C                                             useful for comparing stars
C                                             fitted with varying profiles or
C                                             stars on different exposures)
C                                             But it is very time consuming,
C                                             so omit it if you dont need it.
C                                             Choices are NO,YES
C
C          XBOX          8*RX                 The length in X pixels of the
C                                             area around each star to be
C                                             used in the analysis of that
C                                             star
C
C          YBOX          8*RY                 The length in Y pixels of
C                                             that area
C
C
C
C
C
C
C
C
C           A J Penny   RGO                            4-NOV-82
C
C
C--------------------------------------------------------------------------



C     Aspic program    *** LORSIM ***
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
      REAL PROF(5)
      INTEGER NSIZE(2)
      LOGICAL VALID,PARFIX,VOLCAL
      CHARACTER*72 TXT
      CHARACTER TEXT*72,CVAL*1,TITLE(1)*30
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
      CALL GTIMAG(IPIN,NSIZE,BS,BZ,INVAL,IERR)
      IF (IERR.NE.0) VALID = .FALSE.
C
C ----------------------------------------------------------------
C
C   Now seek a list of (x,y) positions and open working areas as :-
C   IPST for the temporary storage of the results
C
      IF (VALID) THEN
         CALL GTXYLR('XYLIST',.FALSE.,NIPXY,LSTLEN,IPXY,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            WRITE (TEXT,900) LSTLEN
  900       FORMAT ('NUMBER OF STARS IN THE LIST IS',I4)
            CALL WRUSER(TEXT,ISTAT)
            LSTB = LSTLEN*16
            CALL GETDYN('STORE',FMT_R,LSTB,IPST,ISTATA)
            IF (ISTATA.NE.ERR_NORMAL) THEN
               CALL WRUSER('CANT GET TEMPORARY STORAGE',ISTAT)
               VALID = .FALSE.
            ENDIF
         ELSE
            CALL WRUSER('NO VALID XY LIST',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C -------------------------------------------------------------
C
C
C  Now open a file in the EDRS XY list format to store the results in
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,21,LSTLEN,IPOUT,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
C
C  Transfer the identifiers
C
            CALL EXTLSA(%VAL(IPXY),NIPXY,LSTLEN,1,LSTLEN,1,5,
     +                  %VAL(IPOUT),21,LSTLEN,1,1)
C
C Now store the file descriptors in that file
C
            CALL PTDSCR('OUTPUT','NITEM','INTEGER',21,RVAL,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,
     +                   CVAL,IERR)
            TITLE(1)='Output from LORSIM'
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,I,IERR)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',
     +                  IVAL,RVAL,TITLE(1),IERR)
         ELSE
C
C  If cannot open file, write error message
C
            CALL WRUSER('OUTPUT FILE INVALID',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C -----------------------------------------------------------------
C
C  Get the profile parameters
C
      IF (VALID) THEN
         CALL GTPROF(PROF,PARFIX,VOLCAL,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C --------------------------------------------------------------
C
C  Get the star analysis box size
C
      IF (VALID) THEN
         AX = NSIZE(1)
         NXBOX = 8.0*PROF(1)
         CALL GETPAR('XBOX','INTEGER',1,1.0,AX,.TRUE.,NXBOX,
     +               RVAL,IERR1)
         AY = NSIZE(2)
         NYBOX = 8.0*PROF(2)
         CALL GETPAR('YBOX','INTEGER',1,1.0,AY,.TRUE.,NYBOX,
     +               RVAL,IERR2)
         IF (IERR1.NE.0.OR.IERR2.NE.0) THEN
            VALID = .FALSE.
            CALL WRUSER('BAD ANSWER',ISTAT)
         ENDIF
      ENDIF
C
C -----------------------------------------------------------------
C
C  Now do the actual work of fitting Lorentzians to the stars, then
C  putting the results in the IPST area
C
      IF (VALID) THEN
         CALL DOLOR(%VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,INVAL,PARFIX,
     +              PROF,NXBOX,NYBOX,%VAL(IPXY),NIPXY,LSTLEN,%VAL(IPST),
     +              VOLCAL)
      ENDIF
C
C --------------------------------------------------------------
C
C  Store the results in the output XY list
C
      IF (VALID) THEN
         CALL EXTLSA(%VAL(IPST),16,LSTLEN,1,LSTLEN,1,16,
     +               %VAL(IPOUT),21,LSTLEN,1,6)
      ENDIF
C
C --------------------------------------------------------------
C
C
C  Release the temporary working areas and exit from program
C
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         *************
C         *           *
C         * S/R DOLOR *
C         *           *
C         *************
C
C                This subroutine fits Lorentzians to stars
C                in an XY list.
C
      SUBROUTINE DOLOR(KPT,NPIX,NLINE,BS,BZ,INVAL,PARFIX,PROF,
     +                  NXBOX,NYBOX,XYLIST,NXY,LSTLEN,RES,VOLCAL)
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER*2 KPT(NPIX,NLINE)
      REAL XYLIST(NXY,LSTLEN),PROF(5),RES(16,LSTLEN),SIZE(2)
      INTEGER JFIT(32),NINSTS(8)
      REAL CC(32),XP(8),YP(8),RMSA(8)
      LOGICAL VALID,VALIDA,PARFIX,VOLCAL,AGAIN
      CHARACTER TEXT*72
C
C  Calculate profile volume if wanted and can be done before looping
C
      IF (VOLCAL) THEN
         IF (PARFIX) THEN
            DO K = 1,5
               CC(K+3) = PROF(K)
            ENDDO
            VOL = VOLUME(CC)
         ENDIF
      ENDIF
C
C  Loop through the star list
C
      CALL WRUSER(' ',ISTAT)
      WRITE(TEXT,906)
  906 FORMAT(' ','STAR','  MAG ',' HEIGHT','  DX ','  DY ',
     +       '  RMS ','ITS',' IN','  RX ','   RY ','    P  ',
     +       '   PRX ','   PRY ')
      CALL WRUSER(TEXT,ISTAT)
      DO NSTAR = 1,LSTLEN
C
C ------------------------------------------------------------
C
C  Choose area needed for this star
C
         XA = XYLIST(6,NSTAR)
         YA = XYLIST(7,NSTAR)
         IX1 = XA - REAL(NXBOX)/2.0
         IX2 = IX1 + NXBOX - 1
         IY1 = YA - REAL(NYBOX)/2.0
         IY2 = IY1 + NYBOX - 1
C
C  Check to see if box in main image
C
         VALID = .TRUE.
         IF (IX1.LT.1.OR.IX2.GT.NPIX) VALID = .FALSE.
         IF (IY1.LT.1.OR.IY2.GT.NLINE) VALID = .FALSE.
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
            CALL GETDYN('FIT',FMT_R,NX,IPF,ISTAT)
            IF (ISTAT.NE.ERR_NORMAL) THEN
               CALL WRERR('HELLF')
               VALIDA = .FALSE.
            END IF
            CALL GETDYN('RESID',FMT_R,NX,IPR,ISTAT)
            IF (ISTAT.NE.ERR_NORMAL) THEN
               CALL WRERR('HELLR')
               VALIDA = .FALSE.
            END IF
         ENDIF
C
C ----------------------------------------------------------
C
         IF (VALID.AND.VALIDA) THEN
C
C  Now copy the selected region into working area
C
            CALL COPYRGO(KPT,NPIX,NLINE,%VAL(IPD),LX,LY,
     +                IX1,IY1,IX2,IY2,BS,BZ,INVAL,NINVAL,0)
C
C  Set up for a single star at centre of the box
C
            NSTDEF = 0
            CALL CCFILL(%VAL(IPD),LX,LY,PARFIX,PROF,XP,YP,NSTDEF,
     +                  CC,JFIT)
C
C  Now do the real work
C
            NST = 1
            CALL FITLOR(%VAL(IPD),%VAL(IPF),%VAL(IPR),LX,LY,
     +                      CC,JFIT,NST,ITER,PERMS,20,0,PARFIX)
C
C  Now calculate the rms errors
C
            CALL CALRMS(%VAL(IPR),LX,LY,CC,PROF(1),PROF(2),1,RMSA,
     +                  KPT,NPIX,NLINE,IX1,IY1,INVAL,NINSTS)
            RMS = RMSA(1)
C
C  Now store the result
C
            CALL RESA(CC,RMS,RES,LSTLEN,NSTAR,ITER,PERMS,IX1,IY1,
     +                XA,YA,VOLCAL,VOL,PARFIX,NINVAL)
C
C  If no fit tried, store that
C
         ELSE
            CALL NORES(RES,LSTLEN,NSTAR,XA,YA)
         ENDIF
C
C --------------------------------------------------------
C
         IF (VALID) THEN
C
C   Free the work space and return for another loop.
C
            CALL FRDATA('DATA',ISTAT)
            CALL FRDATA('FIT',ISTAT)
            CALL FRDATA('RESID',ISTAT)
         ENDIF
C
C --------------------------------------------------------
C
      ENDDO
C
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ************
C      *          *
C      * S/R RESA *
C      *          *
C      ************
C
C  This subroutine transfers the output of the fitting s/r into the
C  storage array RES, in a useful form. It also types out some of
C  the results
C
C
C
C
      SUBROUTINE RESA(CC,RMS,RES,LSTLEN,NSTAR,ITER,PERMS,IX1,IY1,
     +                XA,YA,VOLCAL,VOL,PARFIX,NINVAL)
C
C
C
      REAL CC(32),RMS,RES(16,LSTLEN)
      LOGICAL VOLCAL,PARFIX
      CHARACTER TEXT*72
C
C
C
      IF (VOLCAL) THEN
         IF (PARFIX) THEN
            V = VOL
         ELSE
            V = VOLUME(CC)
         ENDIF
      ELSE
         V = 1.0
      ENDIF
      AMAG = CC(11)*V
      IF (AMAG.GT.1.0E-8) THEN
         AMAG = 30.0-2.5*ALOG10(AMAG)
      ELSE
         AMAG = 50.0
      ENDIF
      RES(1,NSTAR) = CC(9) + REAL(IX1) - 1.0
      RES(2,NSTAR) = CC(10) + REAL(IY1) - 1.0
      RES(3,NSTAR) = AMAG
      DX = RES(1,NSTAR) - XA
      DY = RES(2,NSTAR) - YA
      RES(4,NSTAR) = DX
      RES(5,NSTAR) = DY
      RES(6,NSTAR) = REAL(ITER)
      RES(7,NSTAR) = RMS
      RES(8,NSTAR) = NINVAL
      RES(9,NSTAR) = CC(11)
      RES(10,NSTAR) = CC(1)+CC(2)*CC(9)+CC(3)*CC(10)
      RES(11,NSTAR) = PERMS
      RES(12,NSTAR) = CC(4)
      RES(13,NSTAR) = CC(5)
      RES(14,NSTAR) = CC(6)
      RES(15,NSTAR) = CC(7)
      RES(16,NSTAR) = CC(8)
C
C  Stop any overflow problems later
C
      DO K = 1,16
         AR = RES(K,NSTAR)
         IF (ABS(AR).GT.999999.0) AR = SIGN(999999.0,AR)
         RES(K,NSTAR) = AR
      ENDDO
C
C  Type out some fit values
C
      AH = CC(11)
      ADX = DX
      ADY = DY
      ARMS = RMS
      IF (ABS(AH).GT.9999.0) AH = SIGN(9999.0,AH)
      IF (ABS(ADX).GT.99.0) ADX = SIGN(99.0,ADX)
      IF (ABS(ADY).GT.99.0) ADY = SIGN(99.0,ADY)
      IF (ABS(ARMS).GT.999.0) ARMS = SIGN(999.0,ARMS)
      NIN = NINVAL
      IF (NIN.GT.99) NIN = 99
      WRITE (TEXT,900) NSTAR,AMAG,AH,ADX,ADY,
     +                 ARMS,ITER,NIN,CC(4),CC(5),CC(6),CC(7),CC(8)
  900 FORMAT (1H ,I4,F6.2,F7.1,2F5.1,F6.1,I3,I2,2F6.2,F7.3,2F7.1)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R NORES *
C      *           *
C      *************
C
C  This subroutine loads a dummy result when a solution is not
C  made.
C
C
C
      SUBROUTINE NORES(RES,LSTLEN,NSTAR,XA,YA)
C
C
C
      REAL RES(16,LSTLEN)
      CHARACTER TEXT*72
C
C
C
      RES(1,NSTAR) = XA
      RES(2,NSTAR) = YA
      RES(3,NSTAR) = 50.0
      DO K = 4,16
         RES(K,NSTAR) = 0.0
      ENDDO
C
C
C
      FA = 50.0
      F = 0.0
      I = 0
      WRITE (TEXT,900)NSTAR,FA,F,F,F,F,I,I,F,F,F,F,F
  900 FORMAT (1H ,I4,F6.2,F7.1,2F5.1,F6.1,I3,I2,2F6.2,F7.3,2F7.1)
      CALL WRUSER(TEXT,KSTAT)
C
C
C
      END



