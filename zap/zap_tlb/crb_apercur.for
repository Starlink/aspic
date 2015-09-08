	SUBROUTINE CRB_APERCUR
C
C   Altered from I*2 to real, after J.E. Sinclair discovered
C   that apparent non-linearity arose within this program
C   (not apparent within any other program)
C
C   I've also added a version of GETCMD which str$upcases the
C   input commands
C
C   31 July 1987 changed call for KOPT=10 from DISIMG to
C   RDIMAG and made it call REAL
C
	include 'interim(errpar)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      REAL RES(17,2000),VAL(1)
      INTEGER NSIZE(2)
      LOGICAL*1 VALID,EXIT,LOOP
      CHARACTER*1 CURCOL,ICOL(2)
	logical*1 loopsky
      CHARACTER*72 TEXT,TITLE
	character*80 string
C
C
C   Set continuation flag
C
	call cnpar('IMAGE',istat)
      VALID = .TRUE.
C
C --------------------------------------------------------------
C
C   Get or display the image data array
      KW = 1
	CALL RDIMAG('IMAGE',204,2,NSIZE,NDIM,IPIN,ISTAT)
	BS=1
	BZ=0
	INVAL=-32767
	IERR=0
	TITLE='THIS IS AN IMAGE'
      IF (IERR.NE.0) VALID = .FALSE.
C
C -----------------------------------------------------------------
C
C  Open ARGS for writing on and reading from
C
      IF (VALID) THEN
         CALL OPARGS(ID,IXPOS,IYPOS,ISX,ISY,IXOR,IYOR,COMFAC,
     +               DISPX,DISPY,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C -----------------------------------------------------------
C -----------------------------------------------------------
C
      IF (VALID) THEN
C
C  Set up values and staring values
C
         CURCOL = 'B'
         ICOL(1) = 'G'
         ICOL(2) = 'R'
         NTOT = 0
         STARF = 1.0
         NSTAR = 1
         X = 0.0
         Y = 0.0
         DX = 0.0
         DY = 0.0
         NINVAL = 0
         SKYF = 0.0
         SKYL = 0.0
         XSKY = 0.0
         YSKY = 0.0
         NSKY = 1
         APDIA = 20.0
         RMSCOR = 1.0
         ZERO = 0.0
         EXPTIM = 1.0
         KRAD = 10
         KXPOS = DISPX + (REAL(ISX)*COMFAC)/2.0
         KYPOS = DISPY + (REAL(ISY)*COMFAC)/2.0
         EXIT = .FALSE.
C
C Loop around, doing region after region
C
	kopt=1
         DO WHILE (EXIT.EQ.FALSE)
            LOOP = .TRUE.
C
C
C ------------------------------------------------------------------
C
C  Choose Option for action
C
            CALL WRUSER(' ',ISTAT)
            CALL GETCMD('OPTION','APERTURE,STORE,HELP,EXIT,
     +APMEANSKY,ZEROMAG,MAGNITUDE,ZEROTIME,RMS,IMAGE.',
     +                  1,KOPT,EXT,KTEXT,ISTAT)
            CALL CNPAR('OPTION',ISTAT1)
C
C  Exit from program ?
C
            IF (KOPT.EQ.4.OR.ISTAT.NE.0) THEN
               LOOP = .FALSE.
               EXIT = .TRUE.
            ENDIF
C
C  Use aperture to measure area of image
C
         IF (KOPT.EQ.1.AND.LOOP) THEN
	kopt=1
            CALL BCURS(KRAD,KXPOS,KYPOS,KFLAG,CURCOL,ICOL,ISX,ISY,
     +                 IXOR,IYOR,COMFAC,DISPX,DISPY)
            XPOS = REAL(KXPOS)
            YPOS = REAL(KYPOS)
            RADIUS = REAL(KRAD)
            CALL DOCIRC(XPOS,YPOS,RADIUS,FLUX,NUM,KINVAL,INVAL,
     +                  %VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,ARMS)
            IF (KFLAG.EQ.1) THEN
               X = XPOS
               Y = YPOS
               APDIA = 2.0*RADIUS
               STARF = FLUX
               NSTAR = NUM
               NINVAL = KINVAL
               WRITE(TEXT,901)STARF,X,Y
  901          FORMAT(' ','FLUX IN APERTURE= ',G14.4,' X,Y= ',2F6.1)
               CALL WRUSER(TEXT,ISTAT)
               WRITE(TEXT,905)APDIA,NSTAR,NINVAL
  905          FORMAT(' ',' DIA= ',F6.2,'  NPIX= ',I5,' INVALID PIX= ',
     +                I5)
               CALL WRUSER(TEXT,ISTAT)
               CALL MAGNIT(STARF,NSTAR,SKYF,NSKY,SKYL,AMAG,RMS,STARR,
     +                     ZERO,RMSCOR)
               CSKYL = SKYL
               CXSKY = XSKY
               CYSKY = YSKY
               CSKYDI = SKYDIA
               CZERO = ZERO
               CRMS = RMS
               CRMSC = RMSCOR
            ELSE
               SKYF = FLUX
               NSKY = NUM
               IF (NUM.GE.1) THEN
                  SKYL = SKYF/REAL(NUM)
               ELSE
                  SKYL = 0.0
               ENDIF
               SKYDIA = 2.0*RADIUS
               XSKY = XPOS
               YSKY = YPOS
               IF(NUM.GT.2.AND.ARMS.GT.1.0E-8.AND.SKYL.GT.1.0E-8)THEN
                  RMSNOR = SQRT(SKYL/REAL(NUM))
                  ESTCOR = (RMSNOR/ARMS)**2.0
               ELSE
                  ESTCOR = 0.0
               ENDIF
               WRITE(TEXT,902)SKYL,XSKY,YSKY
  902          FORMAT(' ','SKY LEVEL= ',G14.4,' XY=',2F7.1)
               CALL WRUSER(TEXT,ISTAT)
               WRITE (TEXT,908)ARMS,ESTCOR
  908          FORMAT(' ','ERROR= ',G14.4,'   RMS CORR EST= ',G14.4)
               CALL WRUSER(TEXT,ISTAT)
               WRITE(TEXT,906)SKYDIA,NSKY,KINVAL
  906          FORMAT(' ',' DIA= ',F6.1,'  NPIX = ',I5,' INVALID PIX= ',
     +                I5)
               CALL WRUSER(TEXT,ISTAT)
            ENDIF
	endif
C
C   APMEANSKY calculates a running mean of sky and
C   exits once the object-measure button is pressed
C
C
         IF (KOPT.EQ.5.AND.LOOP) THEN
	kopt=5
	print *,'Do skies first (don''t change aperture), then object'
	skymean=0
	nloopsky=0
	skyl=0
	loopsky=.true.
	do while (loopsky)
            CALL BCURS(KRAD,KXPOS,KYPOS,KFLAG,CURCOL,ICOL,ISX,ISY,
     +                 IXOR,IYOR,COMFAC,DISPX,DISPY)
            XPOS = REAL(KXPOS)
            YPOS = REAL(KYPOS)
            RADIUS = REAL(KRAD)
            CALL DOCIRC(XPOS,YPOS,RADIUS,FLUX,NUM,KINVAL,INVAL,
     +                  %VAL(IPIN),NSIZE(1),NSIZE(2),BS,BZ,ARMS)
            IF (KFLAG.EQ.1) THEN
		loopsky=.false.
               X = XPOS
               Y = YPOS
               APDIA = 2.0*RADIUS
               STARF = FLUX
               NSTAR = NUM
               NINVAL = KINVAL
               WRITE(TEXT,901)STARF,X,Y
               CALL WRUSER(TEXT,ISTAT)
               WRITE(TEXT,905)APDIA,NSTAR,NINVAL
               CALL WRUSER(TEXT,ISTAT)
               CALL MAGNIT(STARF,NSTAR,SKYF,NSKY,SKYL,AMAG,RMS,STARR,
     +                     ZERO,RMSCOR)
               CSKYL = SKYL
               CXSKY = XSKY
               CYSKY = YSKY
               CSKYDI = SKYDIA
               CZERO = ZERO
               CRMS = RMS
               CRMSC = RMSCOR
            ELSE
		nloopsky=nloopsky+1
               SKYF = FLUX
               NSKY = NUM
               IF (NUM.GE.1) THEN
                  SKYL = SKYF/REAL(NUM)
               ELSE
                  SKYL = 0.0
               ENDIF
		skymean=skymean*(nloopsky-1)+skyl
		skymean=skymean/real(nloopsky)
		skyl=skymean
               SKYDIA = 2.0*RADIUS
               XSKY = XPOS
               YSKY = YPOS
               IF(NUM.GT.2.AND.ARMS.GT.1.0E-8.AND.SKYL.GT.1.0E-8)THEN
                  RMSNOR = SQRT(SKYL/REAL(NUM))
                  ESTCOR = (RMSNOR/ARMS)**2.0
               ELSE
                  ESTCOR = 0.0
               ENDIF
               WRITE(TEXT,902)SKYL,XSKY,YSKY
               CALL WRUSER(TEXT,ISTAT)
               WRITE (TEXT,908)ARMS,ESTCOR
               CALL WRUSER(TEXT,ISTAT)
               WRITE(TEXT,906)SKYDIA,NSKY,KINVAL
               CALL WRUSER(TEXT,ISTAT)
            ENDIF
		enddo
         ENDIF
C
C  Store last star result
C
            IF (KOPT.EQ.2.AND.LOOP) THEN
               IF (NTOT.LT.2000) THEN
                  NTOT = NTOT + 1
                  WRITE (TEXT,922)NTOT
  922             FORMAT(' ','STAR NO = ',I5)
                  CALL WRUSER(TEXT,ISTAT)
                  CALL STORES(NTOT,X,Y,AMAG,STARR,DX,DY,NINVAL,
     +                        NSTAR,CRMS,CSKYL,APDIA,CXSKY,CYSKY,
     +                        CSKYDI,CZERO,CRMSC,RES,17,2000)
               ELSE
                  CALL WRUSER('NOT STORED, OVER 2000 DONE',ISTAT)
               ENDIF
            ENDIF
C
C  Change Zero point
C
            IF (KOPT.EQ.6.AND.LOOP) THEN
               VAL(1) = ZERO
               CALL RDKEYR('ZEROM',.TRUE.,1,VAL,NVALS,ISTAT)
               CALL CNPAR('ZEROM',ISTATA)
               IF (ISTAT.EQ.1) CALL WRUSER('ZERO UNCHANGED',ISTATB)
               IF (ISTAT.NE.0.AND.ISTAT.NE.1) THEN
                  CALL WRUSER('BAD RESPONSE, ZERO UNCHANGED',ISTATC)
               ENDIF
               IF (ISTAT.EQ.0) ZERO = VAL(1)
            ENDIF
C
C  Change Zero point by exposure time
C
            IF (KOPT.EQ.8.AND.LOOP) THEN
               VAL(1) = EXPTIM
               CALL RDKEYR('EXPTIM',.TRUE.,1,VAL,NVALS,ISTAT)
               CALL CNPAR('EXPTIM',ISTATA)
               IF (ISTAT.EQ.1) CALL WRUSER('EXP TIME UNCHANGED',ISTATB)
               IF (ISTAT.NE.0.AND.ISTAT.NE.1) THEN
                  CALL WRUSER('BAD RESPONSE, EXP TIME UNCHANGED',ISTATC)
               ENDIF
               IF (ISTAT.EQ.0) THEN
                  IF (VAL(1).GT.1.0E-8.AND.VAL(1).LT.1.0E8) THEN
                     ZERO = 2.5*ALOG10(VAL(1))
                     EXPTIM = VAL(1)
                     WRITE (TEXT,934)ZERO
  934                FORMAT(' ','NEW ZERO= ',F7.2)
                     CALL WRUSER(TEXT,ISTAT)
                  ELSE
                     CALL WRUSER('FORBIDDEN TIME, ZERO UNCHANGED',ISTAT)
                  ENDIF
               ENDIF
            ENDIF
C
C  Recalculate Magnitude from last star and sky
C
            IF (KOPT.EQ.7.AND.LOOP) THEN
               WRITE(TEXT,911)STARF,X,Y
  911          FORMAT(' ','FLUX IN APERTURE= ',G14.4,' X,Y= ',2F6.1)
               CALL WRUSER(TEXT,ISTAT)
               WRITE(TEXT,915)APDIA,NSTAR,NINVAL
  915          FORMAT(' ',' DIA= ',F6.2,'  NPIX= ',I5,' INVALID PIX= ',
     +                I5)
               CALL WRUSER(TEXT,ISTAT)
               CALL MAGNIT(STARF,NSTAR,SKYF,NSKY,SKYL,AMAG,RMS,STARR,
     +                     ZERO,RMSCOR)
               CSKYL = SKYL
               CXSKY = XSKY
               CYSKY = YSKY
               CSKYDI = SKYDIA
               CZERO = ZERO
               CRMS = RMS
               CRMSC = RMSCOR
            ENDIF
C
C  Type list of options
C
            IF (KOPT.EQ.3.AND.LOOP) THEN
               CALL WRUSER(' ',ISTAT)
               CALL WRUSER(' COMMAND            FUNCTION',ISTAT)
               CALL WRUSER(' ',ISTAT)
               CALL WRUSER(' APERTURE    Use aper to pick star or sky',
     +                     ISTAT)
		CALL WRUSER(
	1   'APMEANSKY   do skies first, then star',istat)
               CALL WRUSER(' STORE       Store last mag result',
     +                     ISTAT)
               CALL WRUSER(' MAGNITUDE   Recalc mag from last star,sky',
     +                     ISTAT)
               CALL WRUSER(' ZEROMAG     Change Zeropoint applied',
     +                     ISTAT)
               CALL WRUSER(' ZEROTIME    Change Zeropoint by exp time',
     +                     ISTAT)
               CALL WRUSER(' HELP        List options',ISTAT)
               CALL WRUSER(' IMAGE       Display and use new image',
     +                     ISTAT)
               CALL WRUSER(' EXIT        Leave program',ISTAT)
               CALL WRUSER(' ',ISTAT)
               CALL WRUSER('BUTTONS FROM LEFT 1)PICK SKY  2)DEC APER',
     +                     ISTAT)
               CALL WRUSER('                  3)INC APER  4)PICK STAR',
     +                     ISTAT)
               CALL WRUSER(' ',ISTAT)
            ENDIF
C
C  Change RMS correction value if wanted
C
            IF (KOPT.EQ.9.AND.LOOP) THEN
               CALL WRUSER('INPUT RMS SCALING FACTOR',ISTAT)
               VAL(1) = RMSCOR
               CALL RDKEYR('RMSCOR',.TRUE.,1,VAL,NVALS,ISTAT)
               CALL CNPAR('RMSCOR',ISTAT)
               RMSCOR = VAL(1)
            ENDIF
C
C  Get and display a new image and open ARGS for use
C
            IF (KOPT.EQ.10.AND.LOOP) THEN
		CALL CNPAR('IMAGE',ISTAT)
		call rdimag('IMAGE',204,2,nsize,ndim,ipin,istat)
		call crb_areset
		call crb_adisp
               CALL CNPAR('XRANGE',ISTAT)
               CALL CNPAR('YRANGE',ISTAT)
               CALL CNPAR('PVLO',ISTAT)
               CALL CNPAR('PVHI',ISTAT)
               IF (IERR.NE.0) THEN
                  CALL WRUSER('CANT GET IMAGE',ISTAT)
                  LOOP = .FALSE.
               ENDIF
               CALL OPARGS(ID,IXPOS,IYPOS,ISX,ISY,IXOR,IYOR,
     +                     COMFAC,DISPX,DISPY,IERR)
               KXPOS = DISPX + (REAL(ISX)*COMFAC)/2.0
               KYPOS = DISPY + (REAL(ISY)*COMFAC)/2.0
               IF (IERR.NE.0) THEN
                  CALL WRUSER('CANT GET IMAGE',ISTAT)
                  LOOP = .FALSE.
               ENDIF
            ENDIF
C
C  Loop round again ?
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
         CALL XYLWRI(RES,17,2000,1,NTOT,1,22,IERR)
      ENDIF
C
C ---------------------------------------------------------
C
C  Clear overlay planes and data
C
      CALL ARGS_OVCL(8,.FALSE.)
      CALL ARGS_OVCL(10,.FALSE.)
      CALL ARGS_OVCL(9,.FALSE.)
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOCIRC *
C      *            *
C      **************
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
      SUBROUTINE DOCIRC(X,Y,RADIUS,FLUX,NUM,NINVAL,INVAL,
     +                  KDATA,NX,NY,BS,BZ,RMS)
C
C
C
      real*4 KDATA(NX,NY)
      DOUBLE PRECISION SUM,SUMSQ
C
C
C
      KBX = X - RADIUS - 1.0
      KEX = X + RADIUS + 1.0
      KBY = Y - RADIUS - 1.0
      KEY = Y + RADIUS + 1.0
      IF (KBX.LT.1) KBX = 1
      IF (KBX.GT.NX) KBX = NX
      IF (KEX.LT.1) KEX = 1
      IF (KEX.GT.NX) KEX = NX
      IF (KBY.LT.1) KBY = 1
      IF (KBY.GT.NY) KEY = NY
      IF (KEY.LT.1) KEY = 1
      IF (KEY.GT.NY) KEY = NY
C
C
C
      DMAX = RADIUS*RADIUS
      NS = 0
      NINVAL = 0
      SUM = 0.0
      SUMSQ = 0.0
      DO K = KBY,KEY
         DO J = KBX,KEX
            XD = REAL(J) - X
            YD = REAL(K) - Y
            DIST = XD*XD + YD*YD
            IF (DIST.LE.DMAX) THEN
               KVAL = KDATA(J,K)
               IF (KVAL.EQ.INVAL) THEN
                  NINVAL = NINVAL + 1
               ELSE
                  NS = NS + 1
                  SUM = SUM + DBLE(KVAL)
                  SUMSQ = SUMSQ + DBLE(KVAL)*DBLE(KVAL)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
C
C
C
      FLUX = SUM*BS + REAL(NS)*BZ
      NUM = NS
C
C  CALC STD DEV OF POINTS IN CIRCLE
C
      IF (NS.GT.2) THEN
         RMS = (SUMSQ-(SUM*SUM/DBLE(NS)))/DBLE((NS-1)*(NS-2))
         IF (RMS.LT.1.0E-10) RMS = 1.0E-10
         RMS = SQRT(RMS)*BS
      ELSE
         RMS = 0.0
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R STORES *
C      *            *
C      **************
C
C
C
C   PURPOSE
C      Store the results in the output array
C
C   ARGUMENTS
C    IN
C      X,Y     Position
C      AMAG,STAR  Star magnitude and flux
C      DX,DY   Posn - input position
C      NIN     No of invalid pixels
C      KPIX    No of pixels in circle round star
C      K       Star number
C      RMS     Star flux error
C      SKY     Sky level
C      NITEM,LSTLEN  Size of output array
C    OUT
C      DATA    Output array
C
C
C
C   A.J.PENNY                   RGO                    82-NOV
C ---------------------------------------------------------------
C
C
C
      SUBROUTINE STORES(K,X,Y,AMAG,STAR,DX,DY,NIN,KPIX,RMS,SKY,
     +                  APDIA,XSKY,YSKY,SKYDIA,ZERO,RMSCOR,
     +                  DATA,NITEM,LSTLEN)
C
C
C
      REAL DATA(NITEM,LSTLEN)
C
C
C
      DATA(1,K) = X
      DATA(2,K) = Y
      DATA(3,K) = AMAG
      DATA(4,K) = DX
      DATA(5,K) = DY
      DATA(6,K) = 0.0
      DATA(7,K) = RMS
      DATA(8,K) = REAL(NIN)
      DATA(9,K) = STAR
      DATA(10,K) = SKY
      DATA(11,K) = REAL(KPIX)
      DATA(12,K) = APDIA
      DATA(13,K) = XSKY
      DATA(14,K) = YSKY
      DATA(15,K) = SKYDIA
      DATA(16,K) = ZERO
      DATA(17,K) = RMSCOR
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R MAGNIT *
C      *            *
C      **************
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
      SUBROUTINE MAGNIT(STARF,NSTAR,SKYF,NSKY,SKYL,AMAG,RMS,STARR,
     +                  ZERO,RMSCOR)
C
C
C
      CHARACTER*72 TEXT
C
C
C
      STARR = STARF - SKYL*REAL(NSTAR)
      IF (STARR.GT.1.0E-8) THEN
         AMAG = 30.0 + ZERO - 2.5*ALOG10(STARR)
      ELSE
         AMAG = 50.0
      ENDIF
      IF (NSTAR.NE.0.AND.NSKY.NE.0.AND.STARR.GT.1.0E-8) THEN
         ST = STARR + (SKYF*REAL(NSTAR)/REAL(NSKY))
         IF (ST.LT.1.0E-8) ST = 1.0E-8
         RMS = SQRT(ST)/STARR
         RMS = RMS/SQRT(RMSCOR)
      ELSE
         RMS = 0.0
      ENDIF
C
C
C  Type out some of the result for this star
C
      BMAG = AMAG
      IF (ABS(BMAG).GT.99.0) BMAG = SIGN(99.0,BMAG)
      BRMS = RMS
      IF (ABS(BRMS).GT.999.0) BRMS = SIGN(999.0,BRMS)
      WRITE(TEXT,903)BMAG,BRMS,X,Y,STARR
  903 FORMAT(' ','  MAG=',F6.3,' RMS= ',F6.2,' XY= ',2F7.1,
     +       ' STAR FLUX= ',G14.4)
      CALL WRUSER(TEXT,ISTAT)
      WRITE (TEXT,904)SKYL
  904 FORMAT(' ','  SKY LEVEL USED= ',G14.4)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      *************
C      *           *
C      * S/R BCURS *
C      *           *
C      *************
C
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE BCURS(KRAD,KXPOS,KYPOS,KRET,CURCOL,INDCOL,ISX,ISY,
     +                 IXOR,IYOR,COMFAC,DISPX,DISPY)
C
C
C
      CHARACTER*1 CURCOL,INDCOL(2)
      INTEGER*2 KDATA(9)
      INTEGER*2 KOUT(769)
C
C
C
      LRAD = REAL(KRAD)/COMFAC
      IF (LRAD.LE.0) LRAD=10
      LXPOS = (REAL(KXPOS)-(DISPX-1.0))/COMFAC + IXOR - 1
      LYPOS = (REAL(KYPOS)-(DISPY-1.0))/COMFAC + IYOR - 1
      KTYPE = 1
      NPOS = 1
      KDATA(1)=KTYPE
      KDATA(2)=NPOS
      KDATA(3)=LRAD
      KDATA(4)=LXPOS
      KDATA(5)=LYPOS
      KDATA(6)=IXOR
      KDATA(7)=IYOR
      KDATA(8)=ISX
      KDATA(9)=ISY
C
C
C
      CALL ARGS_OVOP(9,CURCOL)
      CALL ARGS_OVC(1,INDCOL(1))
      CALL ARGS_OVC(4,INDCOL(2))
      CALL LOAD_UCURA
      CALL WRITE_UCURA(KDATA)
      CALL RUN_UCURA
      CALL READ_UCURA(KOUT)
      CALL ARGS_OVCL(9,.FALSE.)
C
C
C
      KRAD = REAL(KOUT(1))*COMFAC
      KXPOS = KOUT(2) - IXOR + 1
      KXPOS = REAL(KXPOS)*COMFAC + DISPX - 1.0
      KYPOS = KOUT(3) - IYOR + 1
      KYPOS = REAL(KYPOS)*COMFAC + DISPY - 1.0
      KRET=KOUT(769)
C
C
C
      END


C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R OPARGS *
C      *            *
C      **************
C
C
C    AJPENNY                            RGO                29-12-82
C ------------------------------------------------------------------
C
C
C
      SUBROUTINE OPARGS(ID,IXPOS,IYPOS,ISX,ISY,IXOR,IYOR,COMFAC,
     +                  DISPX,DISPY,IERR)
C
C
C
      CHARACTER*72 TEXT
C
C
C
      IERR = 0
C
C
C
      CALL SRINIT(0,.FALSE.,ISTAT)
      CALL ARGS_NUMIM(ID)
      IF (ISTAT.NE.0) THEN
         CALL WRUSER('CANT GET ARGS',ISTAT)
         IERR = 1
      ELSE
         CALL ARGS_RDIM(IXPOS,IYPOS,ISX,ISY,I,I,ISTAT)
         IXOR = IXPOS - (ISX/2)
         IYOR = IYPOS - (ISY/2)
         CALL ARGS_RDPAR('COMPRE',1,TEXT,NVALS,ISTAT)
         IF (ISTAT.EQ.0) THEN
            READ(TEXT,900)KXB,KXE,KYB,KYE,KCOMP
  900       FORMAT(5I10)
            COMFAC = REAL(KCOMP)
            DISPX = REAL(KXB)
            DISPY = REAL(KYB)
         ELSE
            COMFAC = 1.0
            DISPX = 1.0
            DISPY = 1.0
         ENDIF
      ENDIF
C
C
C
      END
      SUBROUTINE GETCMD(NAME,CMDLST,IREPLY,NCOMM,IDCOMM,
     +                  LENCOM,IERR)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO OBTAIN CHARACTER PARAMETERS FROM THE ENVIRONMENT, CHECK
*	THEIR VALIDITY AND HANDLE ANY ERRORS WHICH OCCUR
*
*METHOD
*	EXTRACT THE DEFAULT VALUE FROM THE INPUT LIST OF ALTERNATIVES.
*	OBTAIN A VALUE FROM THE ENVIRONMENT. IF OBTAINED OK, CALL
*	CMDWRD TO CHECK ITS VALIDITY AGAINST THE LIST OF ALTERNATIVES.
*	IF VALID, USE IT, OTHERWISE PROMPT FOR A NEW VALUE.
*
*ARGUMENTS
*	NAME (IN)
*	CHARACTER*(*)
*		THE NAME OF THE PARAMETER TO BE OBTAINED
*	CMDLST (IN)
*	CHARACTER*(*)
*		A LIST OF ALTERNATIVE VALID PARAMETER VALUES, SEPARATED
*		BY COMMAS AND TERMINATED WITH A FULL STOP.
*	IREPLY (IN)
*	INTEGER
*		IF SET TO 1 OR MORE, THE ROUTINE ISSUES HELPFUL MESSAGES
*		TO THE USER IF ERRONEOUS VALUES ARE ENTERED
*	NCOMM (IN/OUT)
*	INTEGER
*		ON ENTRY, SPECIFIES THE POSITION IN THE LIST 'CMDLST'
*		OF THE DEFAULT VALUE FOR THE PARAMETER. ON EXIT, GIVES
*		THE POSITION OF THE VALUE OBTAINED FROM THE ENVIRONMENT
*	IDCOMM (OUT)
*	CHARACTER*(*)
*		THE FULL FORM OF THE VALUE OBTAINED FROM THE ENVIRONMENT
*	LENCOM (OUT)
*	INTEGER
*		THE NUMBER OF NON-BLANK CHARACTERS IN IDCOMM
*	IERR (OUT)
*	INTEGER
*		ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*	'NAME'
*		PARAMETER NAME TO BE OBTAINED IS GIVEN IN THE ARGUMENT
*		'NAME'
*	BADCMD/ERROR/
*		ACCESSED IF AN INVALID VALUE IS GIVEN FOR THE PARAMETER
*	AMBIGCMD/ERROR/
*		ACCESSED IF AN AMBIGUOUS ABBREVIATION IS USED FOR
*		THE PARAMETER
*	TOOBAD/ERROR/
*		ACCESSED IF THE MAXIMUM PERMISSIBLE NUMBER OF INVALID
*		VALUES HAS BEEN GIVEN AND THE ROUTINE IS TO USE THE
*		DEFAULT VALUE
*
*CALLS
*	THIS PACKAGE:
*		NINDEX,CMDWRD
*	STARLINK:
*		RDKEYC,WRERR,WRUSER,CNPAR
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER NAME*(*),CMDLST*(*),IDCOMM*(*),CMD(1)*80
      LOGICAL EXIT
      INTEGER START,END,ENDLST,CERR
C
C SET MAX. NUMBER OF ERRONEOUS ENTRIES ALLOWED
C
      PARAMETER (MAXWNG=4)
      IERR=0
      NWRONG=0
C
C FIND THE BEGINNING AND END OF THE DEFAULT PARAMETER VALUE IN THE
C INPUT LIST
C
      START=NINDEX(CMDLST,',',NCOMM-1)+1
      ENDLST=NINDEX(CMDLST,'.',1)-1
      END=MIN(NINDEX(CMDLST,',',NCOMM)-1,ENDLST)
      LENGTH=END-START+1
C
C IF IT HAS ZERO LENGTH, ABORT WITH IERR=1
C
      IF(LENGTH.LE.0) THEN
        IERR=1
      ELSE
C
C SET DEFAULT VALUE, THEN OBTAIN NEW VALUE FROM THE ENVIRONMENT
C
   67     CMD(1)=CMDLST(START:END)
          CALL RDKEYC(NAME,.TRUE.,1,CMD,NVALS,KEYERR)
	call str$upcase(cmd,cmd)
C
C IF PARAMETER NAME IS INVALID, EXIT WITH IERR=2
C
          IF(KEYERR.EQ.4) THEN
            IERR=2
            NWRONG=0
            EXIT=.TRUE.
C
C IF NULL WAS ENTERED, USE THE DEFAULT VALUE
C
          ELSE IF(KEYERR.EQ.1) THEN
            IDCOMM=CMDLST(START:END)
            LENCOM=LENGTH
            NWRONG=0
            EXIT=.TRUE.
          ELSE
C
C IF VALUE WAS ENTERED SUCCESSFULLY, CALL CMDWRD TO IDENTIFY IT
C
            CALL CMDWRD(CMDLST(:ENDLST),CMD(1),
     +      NINPUT,NID,IDCOMM,LENCOM,CERR)
C
C IF THE INPUT WAS BLANK, USE THE DEFAULT
C
            IF(CERR.EQ.4) THEN
              IDCOMM=CMDLST(START:END)
              LENCOM=LENGTH
	      NWRONG=0
              EXIT=.TRUE.
C
C IF NOT A VALID VALUE, GIVE MESSAGE AND RETURN FOR A NEW VALUE
C
            ELSE IF(CERR.EQ.3) THEN
              CALL WRERR('BADCMD')
              NWRONG=NWRONG+1
              EXIT=.FALSE.
C
C IF AN AMBIGUOUS ABBREVIATION, GIVE MESSAGE AND RETURN FOR A NEW
C VALUE
C
            ELSE IF(CERR.EQ.2) THEN
	      CALL WRERR('AMBIGCMD')
              NWRONG=NWRONG+1
              EXIT=.FALSE.
            ELSE
C
C OTHERWISE OK...
C
              NWRONG=0
              EXIT=.TRUE.
	      NCOMM=NID
C
C IF MIS-TYPED, GIVE MESSAGE, BUT CONTINUE
C
              IF(CERR.EQ.1) THEN
		IF(IREPLY.GE.1) THEN
		  CALL WRUSER(' ***'''//CMD(1)(:NINPUT)//
     +		  ''' IS NOT VALID',ISTAT)
		  CALL WRUSER(' ***ASSUMING YOU MEAN '''//
     +		  IDCOMM(:LENCOM)//'''',ISTAT)
		ENDIF
              ENDIF
	    ENDIF
	  ENDIF
C
C IF PARAMETER WAS BAD, BUT MAX NUMBER OF BAD ENTRIES HAS NOT BEEN
C REACHED, GIVE MESSAGE SHOWING THE VALID VALUES
C
	  IF(NWRONG.LT.MAXWNG) THEN
	    IF(NWRONG.GT.0) THEN
	      IF(IREPLY.GE.1) THEN
		CALL WRUSER(' ***VALID VALUES ARE: '//CMDLST(:ENDLST),
     +		ISTAT)
	      ENDIF
	    ENDIF
	  ELSE
C
C IF MAX. NUMBER OF BAD ENTRIES HAS BEEN REACHED, GIVE MESSAGE AND USE
C THE DEFAULT
C
	    IDCOMM=CMDLST(START:END)
	    LENCOM=LENGTH
	    CALL WRERR('TOOBAD')
	    IF(IREPLY.GE.1) THEN
	      CALL WRUSER(' ***DEFAULTING TO '''//IDCOMM(:LENCOM)//
     +	      '''',ISTAT)
            ENDIF
	    GO TO 99
	  ENDIF
C
C IF A NEW VALUE IS NEEDED, CANCEL OLD VALUE AND RETURN FOR A NEW ONE
C
	  IF(.NOT.EXIT) THEN
	    CALL CNPAR(NAME,ISTAT)
	    GO TO 67
	  ENDIF
      ENDIF
   99 RETURN
      END
