	SUBROUTINE CRB_INSPECT
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
	CALL CNPAR('DEVICE',ISTAT)
	CALL CNPAR('DEVSIZE',ISTAT)
	CALL CNPAR('DEVLIM',ISTAT)
	CALL CNPAR('CURSOR',ISTAT)
	CALL CNPAR('OPTION',ISTAT)
	CALL CNPAR('XAREA',ISTAT)
	CALL CNPAR('YAREA',ISTAT)
	CALL CNPAR('LOW',ISTAT)
	CALL CNPAR('HIGH',ISTAT)
	CALL CNPAR('STEP',ISTAT)
	CALL ARGS_OVCLR(9)
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
