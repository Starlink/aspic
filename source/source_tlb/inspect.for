C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C           ********************
C           *                  *
C           * Program  INSPECT *
C           *                  *
C           ********************
C
C
C
C          CALLING SEQUENCE:-
C              INSPECT
C
C
C          FUNCTION:-
C            This program can be used for inspecting areas of an image.
C            It is designed for Integer*2 images.
C
C            You choose an image and are then asked to define an
C            area by Cursor or Keyboard definition.
C            You can then choose an option from this list
C        Command            Function                    Short Command
C
C     1) HISTOGRAM  Display its histogram                       HI
C     2) MEAN       Calculate its mean and std dev              M
C     3) 3MEAN      Do 2) for points within 3sigma of the mean  3
C     4) VALUES     Type out a 5x5 grid of values in the area   V
C     5) SOLID      Display the area as a solid body plot       S
C     6) AREA       Choose a new area                           A
C     7) HELP       List the options                            HE
C     8) EXIT       Exit from the program                       E
C
C            After an option is completed the choice is repeated
C
C            The program can deal with the ICDISP displayed images
C            which can be compressed and/or windowed.
C
C
C
C
C
C          USER PARAMETERS:-
C
C
C          IMAGE                              The input image.
C
C          CURSOR        Yes                  Flag for defining area by
C                                             cursor or keyboard. Choices
C                                             are YES,NO.
C
C          XAREA         All                  X limits of area to be chosen
C
C          YAREA         All                  Y limits of area to be chosen
C
C          OPTION        Histogram            Flag for next course of
C                                             action. Choices are
C                                             as described above.
C
C          HIGH          Top                  Generally top of range to
C                                             be treated.
C
C          LOW           Bottom               Generally bottom of range to
C                                             be treated
C
C          STEP          Appropriate          Size of grouping of points
C                                             in histogram. Default gives
C                                             1000 across output device.
C
C          DEVICE        ARGS                 If the data are plotted out
C                                             this is
C                                             the flag for which device to
C                                             plot out on.
C                                             Choices are ARGS,TEKTRONIX,
C                                             GOC,VERSATEC,CALCOMP,CC81.
C
C          DEVSIZE       Various                The size of the output plot.
C
C          DEVLIM        Min,Max              The Min and Max values to be
C                                             plotted out.
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
C         RED   4     This is used to define a corner of the area, and then
C                     define the opposing corner. A rectangle is then
C                     painted round the area
C
C
C
C
C
C        AJ PENNY                  RGO         82-SEP-10
C
C
C-----------------------------------------------------------------



	PROGRAM INSPECT
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER NAXIS(2)
      INTEGER NPIX(3),NRAN(2),LX(2),LY(2),NS(2)
      REAL DEVSIZ(2)
      CHARACTER*72 TEXT,TITLE
      CHARACTER*72 TXT
C
C  Get the input image
C
    1 CONTINUE
      CALL WRUSER(' ',ISTAT)
      CALL GTIMG(IPIM,NAXIS,BSCALE,BZERO,INVAL,TITLE,IERR)
      IF (IERR.NE.0) GO TO 1
      CALL WRUSER('TITLE IS',ISTAT)
      CALL WRUSER(TITLE,ISTAT)
      WRITE(TEXT,940)BSCALE
  940 FORMAT(' ','BSCALE= ',G14.4)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,941)BZERO
  941 FORMAT(' ','BZERO= ',G14.4)
      CALL WRUSER(TEXT,ISTAT)
      KX = NAXIS(1)
      KY = NAXIS(2)
      WRITE(TEXT,942)KX,KY
  942 FORMAT(' ','X SIZE = ',I6,'  Y SIZE = ',I6)
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER(' ',ISTAT)
C
C  Get XY limits of area from cursor or keyboard and find
C  range of values. Make a reference histogram for use in the
C  calculations.
C
  100 CONTINUE
      LX(1) = 1
      LX(2) = KX
      LY(1) = 1
      LY(2) = KY
      CALL GETXY(LX,LY,KX,KY)
      CALL RANGE(%VAL(IPIM),KX,KY,LX,LY,KMIN,KMAX,INVAL,IERR)
      IF (IERR.EQ.1) THEN
         CALL WRUSER('NO VALID PIXELS IN THIS AREA,TRY AGAIN',JSTAT)
         GO TO 100
      ENDIF
      AMIN = REAL(KMIN)*BSCALE + BZERO
      AMAX = REAL(KMAX)*BSCALE + BZERO
      WRITE(TEXT,900)AMIN,AMAX
  900 FORMAT('  MIN = ',F12.4,'  MAX = ',F12.4)
      CALL WRUSER(TEXT,ISTAT)
      NUMBIN = KMAX - KMIN + 1
      CALL GETDYN('HIST',FMT_SL,NUMBIN,IPHIS,KSTAT)
      CALL HGRAM(%VAL(IPIM),KX,KY,LX,LY,KMIN,%VAL(IPHIS),NUMBIN,INVAL)
C
C
C ----------------------------------------------------
C
C    Loop round inspecting image
C    ***************************
C ---------------------------------------------------------
C
C
C --------------------------------------------------------------
C
C  Choose option
C
  101 CONTINUE
      KOPT = 1
      CALL GETCMD('OPTION','HISTOGRAM,MEAN,3MEAN,VALUES,SOLID,XX
     +XX,AREA,HELP,EXIT.',1,KOPT,TEXT,KTEXT,ISTAT)
      CALL CNPAR('OPTION',JSTAT)
C
C ---------------------------------------------------
C
C  Put out histogram if wanted
C
      IF (KOPT.EQ.1) THEN
         ALO = AMIN
         CALL GETPAR('LOW','REAL',1,AMIN,AMAX,.TRUE.,IVAL,ALO,IERR)
         CALL CNPAR('LOW',ISTAT)
         AHI = AMAX
         CALL GETPAR('HIGH','REAL',1,ALO,AMAX,.TRUE.,IVAL,AHI,IERR)
         CALL CNPAR('HIGH',ISTAT)
         STMIN = (AHI-ALO+1.0)/1000.0
         STMINA = STMIN/BSCALE
         IF (STMINA.LT.1.0) STMIN = BSCALE
         STMAX = AHI - ALO
         STEP = STMIN
         CALL GETPAR('STEP','REAL',1,STMIN,STMAX,.TRUE.,IVAL,STEP,IERR)
         CALL CNPAR('STEP',ISTAT)
         CALL DEVOPN(IDEV,DEVSIZ)
         IF (IDEV.NE.1) THEN
            CALL PLOTLI(%VAL(IPHIS),NUMBIN,KMIN,ALO,AHI,STEP,
     +                  BSCALE,BZERO,DEVSIZ)
            CALL DEVCLS(IDEV)
         ENDIF
      ENDIF
C
C -------------------------------------------------------
C
C  Display area as a solid body plot
C
      IF (KOPT.EQ.5) THEN
         CALL DEVOPN(IDEV,DEVSIZ)
         IF (IDEV.NE.1) THEN
            CALL PLTSLI(%VAL(IPIM),KX,KY,LX,LY,BSCALE,BZERO,
     +                  DEVSIZ,IERR)
            CALL DEVCLS(IDEV)
         ENDIF
      ENDIF
C
C ------------------------------------------------------
C
C  Type on terminal the values in the area, if wanted
C
      IF (KOPT.EQ.4) THEN
         CALL WRUSER(' ',ISTAT)
         WRITE(TEXT,901)BSCALE,BZERO
  901    FORMAT(' MULTIPLY THESE BY ',F12.4,' AND ADD ',F12.4)
         CALL WRUSER(TEXT,ISTAT)
         CALL WRUSER(' ',ISTAT)
         CALL PRAREA(%VAL(IPIM),KX,KY,LX,LY)
      ENDIF
C
C ----------------------------------------------------
C
C  Put out mean and std deviation if wanted
C
      IF (KOPT.EQ.2.OR.KOPT.EQ.3) THEN
         ALO = AMIN
         CALL GETPAR('LOW','REAL',1,AMIN,AMAX,.TRUE.,IVAL,ALO,IERR)
         CALL CNPAR('LOW',ISTAT)
         AHI = AMAX
         CALL GETPAR('HIGH','REAL',1,ALO,AMAX,.TRUE.,IVAL,AHI,IERR)
         CALL CNPAR('HIGH',ISTAT)
         MIN = INT((ALO-BZERO)/BSCALE)
         MAX = INT((AHI-BZERO)/BSCALE)
         NRAN(1) = MIN - KMIN + 1
         NRAN(2) = MAX - KMIN + 1
         JN = 1
         IF (KOPT.EQ.3) JN = 3
         CALL MSTDEV(%VAL(IPHIS),NUMBIN,NRAN,JN,S,STD,SNUM)
         S = S + KMIN - 1.0
         S = BSCALE*S + BZERO
         STD = BSCALE*STD
         WRITE(TEXT,902)S,STD,SNUM
  902    FORMAT('  MEAN = ',F12.4,'  STD DEV = ',F12.4,
     +          '  NO = ',F12.1)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C----------------------------------------------------------
C
C  Choose a new area if wanted, getting histogram for the other calcs
C
      IF (KOPT.EQ.7) THEN
         CALL FRDATA('HIST',ISTAT)
  102    CONTINUE
         LX(1) = 1
         LX(2) = KX
         LY(1) = 1
         LY(2) = KY
         CALL GETXY(LX,LY,KX,KY)
         CALL RANGE(%VAL(IPIM),KX,KY,LX,LY,KMIN,KMAX,INVAL,IERRR)
         IF (IERR.EQ.1) THEN
            CALL WRUSER('NO VALID PIXELS IN THIS AREA,TRY AGAIN',JSTAT)
            GO TO 102
         ENDIF
         AMIN = REAL(KMIN)*BSCALE + BZERO
         AMAX = REAL(KMAX)*BSCALE + BZERO
         WRITE(TEXT,903)AMIN,AMAX
  903    FORMAT('  MIN = ',F12.4,'  MAX = ',F12.4)
         CALL WRUSER(TEXT,ISTAT)
         NUMBIN = KMAX - KMIN + 1
         CALL GETDYN('HIST',FMT_SL,NUMBIN,IPHIS,KSTAT)
         CALL HGRAM(%VAL(IPIM),KX,KY,LX,LY,KMIN,%VAL(IPHIS),NUMBIN,
     +              INVAL)
      ENDIF
C
C--------------------------------------------------------
C
C  Type list of options if wanted
C
      IF (KOPT.EQ.8) THEN
      CALL WRUSER('Command            Function',ISTAT)
      CALL WRUSER('HISTOGRAM  Display its histogram',ISTAT)
      CALL WRUSER('MEAN       Calculate its mean and std dev',ISTAT)
      CALL WRUSER('3MEAN      Do 2) for points within 3sigma',ISTAT)
      CALL WRUSER('VALUES     Type out of values in the area',ISTAT)
      CALL WRUSER('SOLID      Display as a solid body plot',ISTAT)
      CALL WRUSER('AREA       Choose a new area',ISTAT)
      CALL WRUSER('HELP       List the options',ISTAT)
      CALL WRUSER('EXIT       Exit from the program',ISTAT)
      ENDIF
C
C -----------------------------------------------------
C
C  Go back for new option, unless exit wanted
C
      IF (KOPT.NE.9) THEN
         GO TO 101
      ENDIF
C
C ------------------------------------------------------
C
C  Tidy up and go home
C
      CALL FRDATA(' ',KSTAT)
      CALL END PLT
C
C
C
      END


C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PLOTLI *
C      *            *
C      **************
C
C S/R TO PLOT OUT GRAPH
C
C -------------------------------------------------------
C
C
C
      SUBROUTINE PLOTLI(LINE,NX,KMIN,ALO,AHI,STEP,BSCALE,BZERO,
     +                  SIZE)
C
C
C
      INTEGER LINE(NX)
      REAL AX(2),AY(2),SIZE(2)
      CHARACTER TEXT*72,DEVICE*4
C
C
C
      KSTEP = STEP/BSCALE
      IF (KSTEP.LT.1) KSTEP = 1
      ASTEP = BSCALE*REAL(KSTEP)
C
C  Get min and max of values
C
      LMIN = INT((ALO-BZERO)/BSCALE) - KMIN + 1
      LMAX = INT((AHI-BZERO)/BSCALE) - KMIN + 1
      AMIN = LINE(LMIN)
      AMAX = LINE(LMIN)
      DO K = LMIN,LMAX,KSTEP
         S = 0.0
         JA = K
         JB = JA + KSTEP - 1
         DO J = JA,JB
            IF (J.LE.LMAX) THEN
               S = S + FLOAT(LINE(J))
            ENDIF
         ENDDO
         IF (S.GT.AMAX) AMAX = S
         IF (S.LT.AMIN) AMIN = S
      ENDDO
      MIN = AMIN
      MAX = AMAX
      AY(1) = AMIN
      IF (MAX.GT.MIN) THEN
         AY(2) = AMAX + 0.2*(AMAX-AMIN)
      ELSE
         AY(2) = AY(1) + 1.0
      ENDIF
      AX(1) = ALO - ASTEP
      AX(2) = AHI + ASTEP
C
C  Draw Simpleplot axes
C
      CALL JBAXES(AX,2,SIZE(1),'VALUE',5,AY,2,SIZE(2),'NUMBER',6)
C
C  Plot the data
C
      AV = (REAL(LMIN+KMIN-1)+REAL(KSTEP)/2.0)*BSCALE + BZERO
      AV = AV - ASTEP
      CALL JOIN PT(AV,AY(1))
      DO K = LMIN,LMAX,KSTEP
         JA = K
         JB = JA + KSTEP - 1
         S = 0.0
         DO J = JA,JB
            IF (J.LE.LMAX) THEN
               S = S + FLOAT(LINE(J))
            ENDIF
         ENDDO
         CALL JOIN PT(AV,S)
         AV = AV + ASTEP
         CALL JOIN PT(AV,S)
      ENDDO
      CALL JOIN PT(AV,AY(1))
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PRAREA *
C      *            *
C      **************
C
C THIS S/R PUTS AN AREA OF THE ARRAY OUT ON THE TERMINAL
C
C ------------------------------------------------------
C
C
C
      SUBROUTINE PRAREA(KDATA,KX,KY,LX,LY)
C
C
      INTEGER*2 KDATA(KX,KY)
      INTEGER LX(2),LY(2),KA(10)
      CHARACTER*72 TEXT
      CHARACTER YFLAG,TXT*69
C
C  Calculate steps between values to be typed
C
      NX = 1 + (LX(2)-LX(1))/10
      NY = 1 + (LY(2)-LY(1))/10
      NY = -1.0*NY
C
C  Calc where to put 'Y' marker
C
      KFLAG = LY(2) + NY*(((LY(1)-LY(2))/NY)/2)
C
C  Put out array
C
      DO K = LY(2),LY(1),NY
         IF (K.EQ.KFLAG) THEN
            YFLAG = 'Y'
         ELSE
            YFLAG = ' '
         ENDIF
         WRITE(TEXT,900)YFLAG,K,(KDATA(J,K),J=LX(1),LX(2),NX)
  900    FORMAT(' ',A1,I5,3X,10I6)
         CALL WRUSER(TEXT,ISTAT)
         CALL WRUSER(' ',ISTAT)
      ENDDO
C
C
C  Put out 'X' axis markers
      NXOUT = (LX(2)-LX(1))/NX + 1
      DO K = 1,NXOUT
         KA(K) = LX(1) + NX*(K-1)
      ENDDO
      WRITE(TEXT,901)(KA(K),K=1,NXOUT)
  901 FORMAT(' ',8X,10I6)
      CALL WRUSER(TEXT,ISTAT)
      DO K = 1,69
         TXT(K:K) = ' '
      ENDDO
      K = 11 + 6*(NXOUT-1)/2
      TXT(K:K) = 'X'
      WRITE(TEXT,902)(TXT(K:K),K=1,69)
  902 FORMAT(' ',69A1)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      END




C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R MSTDEV *
C      *            *
C      **************
C
C THIS S/R FINDS THE MEAN AND STD DEV FROM A HISTOGRAM WHICH
C IS STORED IN LINE AND WANTED FROM THE DATA IN RANGE NRAN(1)-NRAN(2)
C IF PARAMETER 'JN' IS NOT ONE, THE S/R WILL ITERATE THROWING OUT
C DATA MORE THAN THREE STD DEV FROM THE MEAN, DOING THIS 'JN' TIMES.
C
C -----------------------------------------------------
C
C
C
      SUBROUTINE MSTDEV(LINE,NX,NRAN,JN,AM,STD,SNUM)
      INTEGER LINE(NX)
      INTEGER NRAN(2)
      DOUBLE PRECISION S,SS,SN
C
C
C
      KL = NRAN(1)
      KH = NRAN(2)
      DO J = 1,JN
         S = 0.0
         SN = 0.0
         SS = 0.0
         IF (KH.GE.KL) THEN
            DO K = KL,KH
               AN = DBLE(LINE(K))
               V = DBLE(K)
               SN = SN + AN
               S = S + AN*V
               SS = SS + AN*V*V
            ENDDO
         ENDIF
         IF (SN.GT.0.0) THEN
            AM = S/SN
         ELSE
            AM = 0.0
         ENDIF
         IF (SN.GT.1.0) THEN
            SS = (SS-S*S/SN)/(SN-1.0)
            IF (SS.GT.1.0E-20) THEN
               STD = SQRT(SS)
            ELSE
               STD = 0.0
            ENDIF
         ELSE
            STD = 0.0
         ENDIF
         KLA = IFIX(AM-3.0*STD)
         KHA = IFIX(AM+3.0*STD)
         IF(KLA.GT.NRAN(1)) KL = KLA
         IF(KHA.LT.NRAN(2)) KH = KHA
      ENDDO
      SNUM = SN
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R HGRAM *
C      *           *
C      *************
C
C SUBROUTINE TO GET HISTOGRAM
C
C ---------------------------------------------------------
C
C
      SUBROUTINE HGRAM(KDATA,KX,KY,LX,LY,KMIN,KGRAM,NUMBIN,INVAL)
      INTEGER LX(2),LY(2)
      INTEGER*2 KDATA(KX,KY)
      INTEGER KGRAM(NUMBIN)
C
      DO L = 1,NUMBIN
         KGRAM(L) = 0
      ENDDO
      DO J = LX(1),LX(2)
         DO K = LY(1),LY(2)
            IF(KDATA(J,K).NE.INVAL) THEN
               L = KDATA(J,K) - KMIN + 1
               IF(L.GE.1.AND.L.LE.NUMBIN)KGRAM(L) = KGRAM(L) + 1
            ENDIF
         ENDDO
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R RANGE *
C      *           *
C      *************
C
C SUBROUTINE RANGE GETS MIN AND MAX VALUES IN A PART OF A 2-D ARRAY
C
C -------------------------------------------------------------
C
C
      SUBROUTINE RANGE(KDATA,KX,KY,LX,LY,MIN,MAX,INVAL,IERR)
      INTEGER*2 KDATA(KX,KY)
      INTEGER LX(2),LY(2)
C
      MIN = 32767
      MAX = -32768
C
      DO J = LX(1),LX(2)
         DO K = LY(1),LY(2)
            L = KDATA(J,K)
            IF(L.NE.INVAL) THEN
               IF(L.LT.MIN) MIN = L
               IF(L.GT.MAX) MAX = L
            ENDIF
         ENDDO
      ENDDO
C
      IERR = 0
      IF (MIN.EQ.32767) IERR = 1
C
      END





C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R GETXY *
C      *           *
C      *************
C
C  This subroutine gets the corners of a rectangle
C  from the ARGS Cursor or from the keyboard
C  If from ARGS cursor, it deals with an ICDISP displayed image
C  where the image may be compressed by a factor COMFAC and may
C  only be a window on the whole image (lhb corner pixel is
C  realy pixel DX,DY in main image.
C
C
C ---------------------------------------------------
C
C
      SUBROUTINE GETXY(KXPOS,KYPOS,KXS,KYS)
C
C
C
      INTEGER KXPOS(2),KYPOS(2)
      REAL UX(2),UY(2)
      INTEGER KX(2),KY(2)
      CHARACTER*72 TEXT,TEXTAR
C
C
      INCLUDE 'INTERIM(ERRPAR)'
C
C
C  Get if ARGS wanted and display factors
C
      KCUR = 1
      CALL GETCMD('CURSOR','YES,NO.',1,KCUR,TEXT,KTEXT,ISTAT)
      IF (KCUR.EQ.1) THEN
         CALL ARGS_NUMIM(ID)
         CALL SRINIT(0,.FALSE.,KASTAT)
         CALL ARGS_RDPAR('COMPRE',1,TEXTAR,NVALS,ISTAT)
         IF (ISTAT.EQ.0) THEN
            READ(TEXTAR,987)KXB,KXE,KYB,KYE,KCOMP
  987       FORMAT(5I10)
            COMFAC = REAL(KCOMP)
            DX = REAL(KXB)
            DY = REAL(KYB)
         ELSE
            COMFAC = 1.0
            DX = 1.0
            DY = 1.0
         ENDIF
      ELSE
         KASTAT = 1
      ENDIF
      CALL CNPAR('CURSOR',ISTAT)
C
C If ARGS is wanted, use cursor to get corners of area
C
      IF (KASTAT.EQ.0) THEN
C
         CALL ARGS_CUROP('14','G')
         CALL ASP_PAN(IX,IY,UX(1),UY(1))
         KX(1) = COMFAC*UX(1) + 1.0 + DX - 1.0
         KY(1) = COMFAC*UY(1) + 1.0 + DY - 1.0
         CALL ARGS_OVOP(8,'G')
         CALL CROSS(ID,UX(1),UY(1))
C
         CALL ASP_PAN(IX,IY,UX(2),UY(2))
         KX(2) = COMFAC*UX(2) + 1.0 + DX - 1.0
         KY(2) = COMFAC*UY(2) + 1.0 + DY - 1.0
         CALL ARGS_CURCL
C
         CALL ARGS_OVCL(8,.FALSE.)
         CALL ARGS_OVOP(8,'G')
         CALL RECTAN(ID,UX,UY)
         CALL ARGS_OVCL(8,.FALSE.)
C
      ELSE
C
C If keyboard wanted, ask it
C
         KX(1) = 1
         KX(2) = KXS
         CALL RDKEYI('XAREA',.TRUE.,2,KX,NVAL,IERR)
         CALL CNPAR('XAREA',ISTAT)
         KY(1) = 1
         KY(2) = KYS
         CALL RDKEYI('YAREA',.TRUE.,2,KY,NVAL,IERR)
         CALL CNPAR('YAREA',ISTAT)
      END IF
C
C  Check for out of range
C
      IF (KX(1).LT.1) KX(1) = 1
      IF (KX(2).LT.1) KX(2) = 1
      IF (KY(1).LT.1) KY(1) = 1
      IF (KY(2).LT.1) KY(2) = 1
      IF (KX(1).GT.KXS) KX(1) = KXS
      IF (KX(2).GT.KXS) KX(2) = KXS
      IF (KY(1).GT.KYS) KY(1) = KYS
      IF (KY(2).GT.KYS) KY(2) = KYS
C
C   Now unscramble the bottom left hand corner and
C   the top right hand one.
C
      KXPOS(1)=MIN(KX(1),KX(2))
      KXPOS(2)=MAX(KX(1),KX(2))
      KYPOS(1)=MIN(KY(1),KY(2))
      KYPOS(2)=MAX(KY(1),KY(2))
C
C  If cursor used type out posn
C
      IF (KASTAT.EQ.0) THEN
         WRITE(TEXT,900)KXPOS
  900    FORMAT(' ','X RANGE = ',2I6)
         CALL WRUSER(TEXT,ISTAT)
         WRITE(TEXT,901)KYPOS
  901    FORMAT(' ','Y RANGE = ',2I6)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PLTSLI *
C      *            *
C      **************
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE PLTSLI(KDATA,KX,KY,LX,LY,BS,BZ,SIZE,IERR)
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER LX(2),LY(2)
      INTEGER*2 KDATA(KX,KY)
      REAL SIZE(2),DEVLIM(2)
      CHARACTER TEXT*72
C
C
C
      IERR = 0
C
C
C
      NN = (LX(2)-LX(1)+1)*(LY(2)-LY(1)+1)
      CALL GETDYN('DIS',FMT_R,NN,IPDIS,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRUSER('CANT GET DISPLAY SPACE',ISTATA)
      ELSE
         MAX = KDATA(LX(1),LY(1))
         MIN = MAX
         DO K = LY(1),LY(2)
            DO J = LX(1),LX(2)
               IF (KDATA(J,K).LT.MIN) MIN = KDATA(J,K)
               IF (KDATA(J,K).GT.MAX) MAX = KDATA(J,K)
            ENDDO
         ENDDO
         AMIN = REAL(MIN)*BS + BZ
         AMAX = REAL(MAX)*BS + BZ
         DEVLIM(1) = AMIN
         DEVLIM(2) = AMAX
         CALL RDKEYR('DEVLIM',.TRUE.,2,DEVLIM,NVAL,ISTATA)
         BOT = DEVLIM(1)
         TOP = DEVLIM(2)
         CALL PAGE(SIZE(1),SIZE(2))
         CALL LIM3D(BOT,TOP)
         ASIZE = 0.9*SIZE(1)
         KXA = LX(2) - LX(1) - 1
         KYA = LY(2) - LY(1) - 1
         CALL TRANS(KDATA,KX,KY,LX(1),LY(1),%VAL(IPDIS),KXA,
     +              KYA,BS,BZ)
         CALL SOLID(%VAL(IPDIS),KXA,KYA,ASIZE,15.0)
         CALL CNPAR('DEVLIM',ISTATA)
         CALL FRDATA('DIS',ISTATA)
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R TRANS *
C      *           *
C      *************
C
C
C --------------------------------------------------------
C
C
C
      SUBROUTINE TRANS(KD,KX,KY,LX,LY,DATA,KXA,KYA,BS,BZ)
C
C
C
      REAL DATA(KXA,KYA)
      INTEGER*2 KD(KX,KY)
C
C
C
      DO K = 1,KYA
         DO J = 1,KXA
            JA = J + LX - 1
            KA = K + LY - 1
            DATA(J,K) = REAL(KD(JA,KA))*BS + BZ
         ENDDO
      ENDDO
C
C
C
      END



