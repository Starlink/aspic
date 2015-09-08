C
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   LORCUR *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               LORCUR
C
C
C          FUNCTION:-
C               It performs photometry of cursor selected stars  within  an
C               cursor selected area of an image which has been displayed
C               on an ARGS. The fit is done by a full
C               2-D iterative least squares fit , using a sloping
C               background and a 2-D profile to model a star. The background,
C               the star positions and heights, and if wanted the profile
C               are all found together in the iterations.
C               The program estimates the magnitudes and accurate
C               positions of the stars. It either takes a fixed star
C               profile , or it calculates a profile from the stars.
C               The magnitudes are typed out and the entire fit analysis
C               is stored in  a file for any further work. This may be
C               formatted for printing by using the program PRLORCUR (qv).
C               At any stage, the data in the analysed area or the fit
C               can be displayed as a picture or a graph.
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
C
C          USE:-
C               After choosing the star profile parameters, the user
C               defines the area and approximate positions of the
C               stars in the area by means of the cursor (up to 8 stars
C               can be done). The computer then fits the data, and
C               types out the result, together with an estimate of the
C               RMS error of the fit near each star, and the number of
C               invalid pixels near each star. It also gives the
C               number of iterations it has done (it stops after 20).
C                 The user can then do another area or exit
C
C
C
C         USER PARAMETERS:-
C
C          IMAGE                              This is the name of the I*2
C                                             image.
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
C          OPTION        NEXT                 Flag for choice of action.
C                                             Choices are NEXT,PROFILE,
C                                             RESIDUALS,SOLID,PLTPRF,EXIT,
C                                             HELP,?.
C                                             If NEXT is chosen, then the
C                                             trackerball is used to define
C                                             an area and give the positions
C                                             of the stars.
C                                             If PROFILE is chosen, the image
C                                             profile and type of fit can be
C                                             changed.
C                                             If RESIDUALS is chosen, then
C                                             the residuals from the fit are
C                                             displayed on the ARGS bottom lh
C                                             corner.
C                                             If SOLID is chosen,then the data
C                                             and the residuals are displayed
C                                             in solid body form.
C                                             If PLTPRF is chosen, then the
C                                             residuals are folded in annuli
C                                             round the first star in the
C                                             fit and plotted out against the
C                                             profile.
C                                             If EXIT is chosen, the program
C                                             stops requesting more areas
C                                             and ends, storing all the
C                                             results.
C                                             HELP or ? give a listing of
C                                             the options.
C
C          DEVICE        ARGS                 If the data are plotted out
C                                             by choosing SOLID, this is
C                                             the flag for which device to
C                                             plot out on.
C          DEVSIZE       24,24                The size of the output plot.
C          DEVLIMS       Min,Max              The Min and Max values to be
C                                             plotted out.
C
C          XYWOUT                             This is  the  name
C                                             of  the  .BDF  file  used  to
C                                             store the results.
C
C          XYWTIT        Output               This is the title  to
C                                             be associated with the output
C                                             file.
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     This causes the screen magnification to revert to 1
C                     and the cursor to go to the centre of the image.
C
C         WHITE 2     This causes the screen magnification to be divided
C                     by two. If this would cause it to be less than 1, there
C                     is no effect.
C
C         WHITE 3     This causes the screen magnification to be multiplied
C                     by two. If this would make it too large, there is no
C                     effect.
C
C         RED   4     This is first used to define the lower left hand corner
C                     of the desired area (a cross is drawn there), and next
C                     to define the upper right hand corner (when a
C                     rectangle is drawn round the area). Next the button is
C                     used to define the starting positions of the stars
C                     wanted to be fitted (crosses are drawn at the places)
C                     Entry of star positions is stopped by repeating a
C                     star position exactly (the repeat is not included in
C                     the list of stars to be fitted), or automatically
C                     after the eighth star.
C
C
C
C
C
C
C         A J Penny                RGO                            83-6-14
C
C
C--------------------------------------------------------------------------



C      ASPIC PROGRAM   *** LORCUR ***
C  This version made by A.J.Penny   82-7-19 AND 83-6-14
C      FROM A VERSION BY K F HARTLEY AT RGO ON 16/10/81
C      MODIFIED EXTENSIVELY DURING MARCH/APRIL 1982
C
C
C ---------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      REAL PROF(5),RES(24,2000),XP(8),YP(8),SIZE(2),CC(32)
      INTEGER NSIZE(2)
      LOGICAL VALID,EXIT,LOOP,PARFIX,VOLCAL
      CHARACTER TEXT*72
C
C   Define the function VOLUME
C
      EXTERNAL VOLUME
C
C
C
C   Set continuation flag
C
      VALID = .TRUE.
C
C --------------------------------------------------------------
C
C   Get the image data array
      CALL GTIMAG(IPIN,NSIZE,BS,BZ,INVAL,IERR)
      IF (IERR.NE.0) VALID = .FALSE.
C
C ----------------------------------------------------------------
C
C  Get the ARGS and its display factors
C
      IF (VALID) THEN
         ISTAT=0
         CALL SRINIT(0,.FALSE.,ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL WRUSER('CANT GET ARGS',ISTATA)
            VALID = .FALSE.
         ELSE
            CALL ARGS_NUMIM(ID)
            CALL ARGS_RDIM(IXPOS,IYPOS,ISX,ISY,I,I,ISTAT)
            IXOR = IXPOS - (ISX/2)
            IYOR = IYPOS - (ISY/2)
            CALL ARGS_RDPAR('COMPRE',1,TEXT,NVALS,ISTAT)
            IF (ISTAT.EQ.0) THEN
               READ(TEXT,900)KXB,KXE,KYB,KYE,KCOMP
  900          FORMAT(5I10)
               COMFAC = REAL(KCOMP)
               DX = REAL(KXB)
               DY = REAL(KYB)
            ELSE
               COMFAC = 1.0
               DX = 1.0
               DY = 1.0
            ENDIF
         ENDIF
      ENDIF
C
C -----------------------------------------------------------------
C
C
C  Get wether profile is fixed or not; profile params; volume calc or not
C
      IF (VALID) THEN
         CALL GTPROF(PROF,PARFIX,VOLCAL,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C -----------------------------------------------------------
C -----------------------------------------------------------
C
      IF (VALID) THEN
         NTOTAL = 0
         EXIT = .FALSE.
C
C Loop around, doing region after region
C
         DO WHILE (EXIT.EQ.FALSE)
            LOOP = .TRUE.
C
C
C ------------------------------------------------------------------
C
C  Choose Option for action
C
            CALL WRUSER(' ',ISTAT)
            KOPT = 1
            CALL GETCMD('OPTION','NEXT,PROFILE,RESIDUALS,SOLID,EXIT, M
     +                  M,PLTPRF,HELP,?.',1,KOPT,TEXT,KTEXT,ISTAT)
            CALL CNPAR('OPTION',ISTAT1)
C
C  Give user Help ?
C
            IF (LOOP.AND.(KOPT.EQ.8.OR.KOPT.EQ.9)) THEN
               CALL WRUSER('Choices are:-',ISTAT)
               CALL WRUSER('  NEXT       Use trackerball to define',
     +                     ISTAT)
               CALL WRUSER('             area, rough star positions ',
     +                     ISTAT)
               CALL WRUSER('             and then fit',ISTAT)
               CALL WRUSER('  PROFILE    Change profile to use',ISTAT)
               CALL WRUSER('  RESIDUALS  Draw residuals as solid',ISTAT)
               CALL WRUSER('  SOLID      Draw data as solid picture',
     +                     ISTAT)
               CALL WRUSER('  EXIT       Leave program',ISTAT)
               CALL WRUSER('  PLTPRF     Plot mean radial profile',
     +                     ISTAT)
            ENDIF
C
C  Exit from program ?
C
            IF (KOPT.EQ.5.OR.ISTAT.NE.0) THEN
               LOOP = .FALSE.
               EXIT = .TRUE.
            ENDIF
C
C  Get the new area with its star(s) and analyze it ?
C
            IF (LOOP.AND.(KOPT.EQ.1)) THEN
               CALL GETSRS(IX1,IY1,IX2,IY2,XP,YP,NSTARS,ID,COMFAC,DX,DY,
     +                     %VAL(IPIN),NSIZE(1),NSIZE(2))
               NTEMP = NTOTAL + NSTARS
               IF (NTEMP.GT.2000) THEN
                  CALL WRUSER('OVER 2000 STARS',ISTAT)
                  LOOP = .FALSE.
                  EXIT = .TRUE.
               ENDIF
C
C              Open work spaces
C
               IF (LOOP) THEN
                  LX = IX2 - IX1 + 1
                  LY = IY2 - IY1 + 1
                  NX = LX*LY
                  CALL FRDATA('DATA',ISTAT)
                  CALL GETDYN('DATA',FMT_R,NX,IPD,ISTAT)
                  IF (ISTAT.NE.ERR_NORMAL) THEN
                     CALL WRUSER('CANT GET SPACE',ISTAT1)
                     LOOP = .FALSE.
                  ENDIF
                  CALL FRDATA('FIT',ISTAT)
                  CALL GETDYN('FIT',FMT_R,NX,IPF,ISTAT)
                  IF (ISTAT.NE.ERR_NORMAL) THEN
                     CALL WRUSER('CANT GET SPACE',ISTAT1)
                     LOOP = .FALSE.
                  ENDIF
                  CALL FRDATA('RESID',ISTAT)
                  CALL GETDYN('RESID',FMT_R,NX,IPR,ISTAT)
                  IF (ISTAT.NE.ERR_NORMAL) THEN
                     CALL WRUSER('CANT GET SPACE',ISTAT1)
                     LOOP = .FALSE.
                  ENDIF
               ENDIF
C
C              Analyze the area
C
               IF (LOOP) THEN
                  CALL DOSTAR(%VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,INVAL,
     +                        ID,COMFAC,DX,DY,PROF,PARFIX,VOLCAL,RES,
     +                        NTOTAL,%VAL(IPD),%VAL(IPR),%VAL(IPF),IX1,
     +                        IY1,IX2,IY2,LX,LY,XP,YP,NSTARS,CC,IERR)
                  IF (IERR.NE.0) LOOP = .FALSE.
               ENDIF
            ENDIF
C
C  Change the profile ?
C
            IF (LOOP.AND.(KOPT.EQ.2)) THEN
               CALL GTPROF(PROF,PARFIX,VOLCAL,IERR)
               IF (IERR.NE.0) LOOP = .FALSE.
            ENDIF
C
C  Display the residuals in corner of ARGS image ?
C
            IF (LOOP.AND.(KOPT.EQ.3)) THEN
               NX = LX*LY
               CALL GETDYN('PLOT',FMT_SW,NX,IPP,ISTAT)
               IF (ISTAT.NE.ERR_NORMAL) THEN
                  CALL WRUSER('CANT GET SPACE',ISTAT)
                  LOOP = .FALSE.
               ELSE
                  CALL PLOTRS(%VAL(IPR),LX,LY,%VAL(IPP),IERR)
                  IF (IERR.NE.0) LOOP = .FALSE.
               ENDIF
               CALL FRDATA('PLOT',ISTAT)
            ENDIF
C
C  Display the data and residuals as solid body picture ?
C
            IF (LOOP.AND.(KOPT.EQ.4)) THEN
               CALL DEVOPN(IDEV,SIZE)
               CALL PLOTSL(%VAL(IPD),LX,LY,SIZE,IERR)
               CALL PLOTSL(%VAL(IPR),LX,LY,SIZE,IERR)
               CALL DEVCLS(IDEV)
               IF (IERR.NE.0) LOOP = .FALSE.
            ENDIF
C
C  Display the fitted profile as residuals around first star ?
C
            IF (LOOP.AND.KOPT.EQ.7) THEN
               CALL DEVOPN(IDEV,SIZE)
               CALL PLTPRF(%VAL(IPR),LX,LY,CC,SIZE)
               CALL DEVCLS(IDEV)
            ENDIF
C
C ----------------------------------------------------------
C
         ENDDO
      ENDIF
C
C -------------------------------------------------------------
C --------------------------------------------------------------
C
C  Write results to an output file
C
      IF (VALID) THEN
         CALL XYLWRI(RES,24,2000,1,NTOTAL,1,24,IERR)
      ENDIF
C
C ---------------------------------------------------------
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ***************
C      *             *
C      * S/R GETSRS *
C      *             *
C      ***************
C
C
C ---------------------------------------------------
C
C
C
      SUBROUTINE GETSRS(IX1,IY1,IX2,IY2,XP,YP,NSTARS,ID,COMFAC,DX,DY,
     +                  KD,NSX,NSY)
C
C
C
      REAL XP(8),YP(8),UX(2),UY(2)
      INTEGER KX(2),KY(2)
      INTEGER*2 KD(NSX,NSY)
C
C  Initialise cursors and clear overlays
C
      CALL ARGS_CUROP('14','G')
      CALL ARGS_OVOP(8,'G')
      CALL ARGS_OVOP(9,'R')
C
C  Get the area to be used
C
      CALL ASP_PAN(IX,IY,VX,VY)
      KX(1) = COMFAC*VX + 1.0 + DX - 1.0
      KY(1) = COMFAC*VY + 1.0 + DY - 1.0
      CALL ARGS_OVOP(8,'G')
      CALL CROSS(ID,VX,VY)
      UX(1) = VX
      UY(1) = VY
C
      CALL ASP_PAN(IX,IY,VX,VY)
      KX(2) = COMFAC*VX + 1.0 + DX - 1.0
      KY(2) = COMFAC*VY + 1.0 + DY - 1.0
      UX(2) = VX
      UY(2) = VY
C
      CALL ARGS_OVCL(8,.FALSE.)
      CALL ARGS_OVOP(8,'G')
      CALL RECTAN(ID,UX,UY)
C
C   Now unscramble the bottom left hand corner and
C   the top right hand one.
C
      IX1 = MIN(KX(1),KX(2))
      IX2 = MAX(KX(1),KX(2))
      IY1 = MIN(KY(1),KY(2))
      IY2 = MAX(KY(1),KY(2))
C
C  Now mark the positions of up to eight stars
C
      NSTARS = 0
      KDONE = 0
    1 CONTINUE
         CALL ASP_PAN(IX,IY,VX,VY)
         IF (NSTARS.EQ.0) THEN
            VXA = VX
            VYA = VY
         ELSE
            IF (VX.EQ.VXA.AND.VY.EQ.VYA) THEN
               KDONE = 1
            ELSE
               VXA = VX
               VYA = VY
            ENDIF
         ENDIF
         IF (KDONE.EQ.0) THEN
            NSTARS = NSTARS + 1
            XP(NSTARS) = COMFAC*VX + DX - REAL(IX1) + 1.0
            YP(NSTARS) = COMFAC*VY + DY - REAL(IY1) + 1.0
            CALL CROSS(ID,VX,VY)
         ENDIF
      IF (KDONE.EQ.0.AND.NSTARS.LT.8) GO TO 1
      CALL ARGS_OVCL(8,.FALSE.)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R RECTAN *
C      *            *
C      **************
C
C S/R TO DRAW A RECTANGLE ON THE ARGS
C
C
C ----------------------------------------------------
C
C
C
      SUBROUTINE RECTAN(ID,X,Y)
C
C
C
      REAL X(2),Y(2)
      REAL XP(5),YP(5)
C
C
C
      XP(1)=X(1)
      YP(1)=Y(1)
      XP(2)=X(2)
      YP(2)=Y(1)
      XP(3)=X(2)
      YP(3)=Y(2)
      XP(4)=X(1)
      YP(4)=Y(2)
      XP(5)=XP(1)
      YP(5)=YP(1)
      CALL ARGS_POLYL(ID,5,XP,YP,KSTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R CROSS *
C      *           *
C      *************
C
C S/R TO DRAW CROSS ON ARGS AT POSITION (USER UNITS) (X,Y) OF WIDTH 5 PIXELS
C   (ID IS ID OF IMAGE)
C
C --------------------------------------------------------
C
C
C
      SUBROUTINE CROSS(ID,X,Y)
C
C
C
      REAL XV(2),YV(2)
C
C
C
      CALL ARGS_UTOP (ID,X,Y,KX,KY,ISTAT)
      CALL ARGS_PTOU (ID,KX+2,KY+2,UX,UY,ISTAT)
C
      DX = UX - X
      DY = UY - Y
      XV(1) = X - DX
      YV(1) = Y
      XV(2) = X + DX
      YV(2) = Y
      CALL ARGS_POLYL (ID,2,XV,YV,ISTAT)
C
      XV(1) = X
      YV(1) = Y - DY
      XV(2) = X
      YV(2) = Y + DY
      CALL ARGS_POLYL (ID,2,XV,YV,ISTAT)
C
C
C
      END
C




C



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ***************
C      *             *
C      * S/R PLOTRS  *
C      *             *
C      ***************
C
C
C
C   An array is scaled and displayed at the bottom left hand corner
C   of the ARGS.
C
C   It is used to display the residuals from the fit.
C
C   Parameters
C      INPUT
C         RESID   This is the real input array to be displayed.
C         N       This is the first dimension of RESID
C         M       This is the second dimension of RESID
C         KPLOT   Used as storage space for plot
C         IERR    The error flag. Set = 0 for no faults, = 1 for fault.
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE PLOTRS(RESID,N,M,KPLOT,IERR)
C
C
C
      INTEGER*2 IDUMMY(1),KPLOT(N,M)
      REAL RESID(N,M)
C
C
C
      IERR = 0
C
C   First find the maximum and minimum values of RESID
C
      RMIN=RESID(1,1)
      RMAX=RMIN
      DO J=1,M
         DO I=1,N
            IF (RESID(I,J).GT.RMAX) RMAX=RESID(I,J)
            IF (RESID(I,J).LT.RMIN) RMIN=RESID(I,J)
         ENDDO
      ENDDO
C
C   Then scale and integerize RESID into Plot space
C
      FACTOR=255.0/(RMAX-RMIN)
      DO J=1,M
         DO I=1,N
            KPLOT(I,J)=IFIX((RESID(I,J)-RMIN)*FACTOR)
         ENDDO
      ENDDO
C
C   Finally output to the ARGS, after enabling output to
C   all planes.
C
      CALL ARGS_OVCL(8,.FALSE.)
      CALL SRPXI2(KPLOT,N,N,M,0,0,16,.FALSE.,IDUMMY,1)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ************
C      *          *
C      * S/R RESC *
C      *          *
C      ************
C
C -------------------------------------------------------------
C
C
C
      SUBROUTINE RESC(CC,RMS,RES,NTOTAL,NSTARS,ITER,PERMS,
     +                IX1,IY1,IX2,IY2,VOLCAL,NINSTS,XP,YP)
C
C
C
      REAL CC(32),RMS(8),RES(24,2000),XP(8),YP(8)
      INTEGER NINSTS(8),NSOUT(8)
      CHARACTER TEXT*72
      LOGICAL VOLCAL
C
C
C
      WRITE(TEXT,904)IX1,IY1,IX2,IY2
  904 FORMAT(' ','BLH CORNER AT ',2I5,'  TRH CORNER AT ',2I5)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,900)
  900 FORMAT(' ',' ITER','  RX  ','  RY  ','   P   ','    PRX  ',
     +       '    PRY  ')
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,901)ITER,CC(4),CC(5),CC(6),CC(7),CC(8)
  901 FORMAT(' ',I5,2F6.2,F7.3,2F9.2)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,902)
  902 FORMAT(' ',' STAR','   MAG ','  HEIGHT','     X    ','    Y   ',
     +       '   DX  ','    DY ','       RMS ',' INVAL ')
      CALL WRUSER(TEXT,ISTAT)
      IF (VOLCAL) THEN
         V = VOLUME(CC)
      ELSE
         V = 1.0
      ENDIF
      DO K=1,NSTARS
         L = NTOTAL + K
         LK = 9 + 3*(K-1)
         AMAG = CC(LK+2)*V
         IF (AMAG.GT.1.0E-8) THEN
            AMAG=30.0-2.5*ALOG10(AMAG)
         ELSE
            AMAG = 50.0
         ENDIF
         RES(1,L) = CC(LK) + REAL(IX1) - 1.0
         RES(2,L) = CC(LK+1) + REAL(IY1) - 1.0
         RES(3,L) = AMAG
         RES(4,L) = RES(1,L) - (XP(K)+REAL(IX1)-1.0)
         RES(5,L) = RES(2,L) - (YP(K)+REAL(IY1)-1.0)
         RES(6,L) = REAL(ITER)
         RES(7,L) = RMS(K)
         RES(8,L) = NINSTS(K)
         RES(9,L) = CC(LK+2)
         RES(10,L) = CC(1)+CC(2)*CC(LK)+CC(3)*CC(LK+1)
         RES(11,L) = PERMS
         RES(12,L) = CC(4)
         RES(13,L) = CC(5)
         RES(14,L) = CC(6)
         RES(15,L) = CC(7)
         RES(16,L) = CC(8)
         NSTA = NSTARS - 1
         RES(17,L) = NSTA
         DO KK = 1,8
            NSOUT(KK) = 0
         ENDDO
         KL = 0
         DO KK = 1,8
            IF (KK.LT.NSTARS) THEN
               KL = KL + 1
               IF (KK.EQ.K) KL = KL + 1
               NSOUT(KK) = NTOTAL + KL
            ENDIF
         ENDDO
         DO KK = 1,7
            RES(17+KK,L) = REAL(NSOUT(KK))
         ENDDO
C
C  Type out some of the result for this star
C
         AH = RES(9,L)
         AX = RES(1,L)
         AY = RES(2,L)
         ADX = RES(4,L)
         ADY = RES(5,L)
         ARMS = RES(7,L)
         IF (ABS(AH).GT.99999.0) AH = SIGN(99999.0,AH)
         IF (ABS(AX).GT.99999.0) AX = SIGN(99999.0,AX)
         IF (ABS(AY).GT.99999.0) AY = SIGN(99999.0,AY)
         IF (ABS(ADX).GT.99999.0) ADX = SIGN(99999.0,ADX)
         IF (ABS(ADY).GT.99999.0) ADY = SIGN(99999.0,ADY)
         IF (ABS(ARMS).GT.99999.0) ARMS = SIGN(99999.0,ARMS)
         WRITE(TEXT,903)L,AMAG,AH,AX,AY,ADX,
     +                  ADY,ARMS,NINSTS(K)
  903    FORMAT(' ',I5,F7.2,2F8.1,3F8.1,F11.1,I5)
         CALL WRUSER(TEXT,ISTAT)
C
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ************
C      *          *
C      * S/R MARK *
C      *          *
C      ************
C
C ----------------------------------------------------------------
C
      SUBROUTINE MARK(CC,NSTARS,IX1,IY1,ID,COMFAC,DX,DY)
C
C
C
      REAL CC(32)
C
C
C
      CALL ARGS_OVOP(9,'R')
      DO K = 1,NSTARS
         X = CC(9+(K-1)*3) + REAL(IX1) - 1.0
         Y = CC(10+(K-1)*3) + REAL(IY1) - 1.0
         VX = (X-DX)/COMFAC
         VY = (Y-DY)/COMFAC
         CALL CROSS(ID,VX,VY)
      ENDDO
      CALL ARGS_OVCL(9,.FALSE.)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOSTAR *
C      *            *
C      **************
C
C
C ---------------------------------------------------------------------
C
C
C
      SUBROUTINE DOSTAR(IM,NX,NY,BS,BZ,INVAL,ID,COMFAC,DX,DY,
     +                  PROF,PARFIX,VOLCAL,RES,NTOTAL,DATA,RESID,FIT,
     +                  IX1,IY1,IX2,IY2,LX,LY,XP,YP,NSTARS,CC,IERR)
C
C
C
      REAL RES(24,2000),XP(8),YP(8),CC(32),RMS(8),PROF(5)
      REAL DATA(LX,LY),RESID(LX,LY),FIT(LX,LY)
      INTEGER*2 IM(NX,NY)
      INTEGER NINSTS(8),JFIT(32)
      LOGICAL PARFIX,VOLCAL
C
C
C
      IERR = 0
C
C  Now copy the selected region into DATA
C
       CALL COPYRGO(IM,NX,NY,DATA,LX,LY,
     +           IX1,IY1,IX2,IY2,BS,BZ,INVAL,NINVAL,0)
C
C  Load the star profile and positions into CC and JFIT for the
C  fitting program.
C
       CALL CCFILL(DATA,LX,LY,PARFIX,PROF,XP,YP,
     +             NSTARS,CC,JFIT)
C
C  Do the fit
C
       CALL FITLOR(DATA,FIT,RESID,LX,LY,CC,JFIT,NSTARS,
     +             ITER,PERMS,20,1,PARFIX)
C
C  Calculate the RMS errors
C
       CALL CALRMS(RESID,LX,LY,CC,PROF(1),PROF(2),
     +             NSTARS,RMS,
     +             IM,NX,NY,IX1,IY1,INVAL,
     +             NINSTS)
C
C  Store the results
C
       CALL RESC(CC,RMS,RES,NTOTAL,NSTARS,ITER,PERMS,
     +           IX1,IY1,IX2,IY2,VOLCAL,NINSTS,XP,YP)
       NTOTAL = NTOTAL + NSTARS
C
C  Mark the fitted positions with crosses
C
       CALL MARK(CC,NSTARS,IX1,IY1,ID,COMFAC,DX,DY)
C
C
C
      END
