C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                      ***********************
C                      *                     *
C                      * Program   PDSRIPPLE *
C                      *                     *
C                      ***********************
C
C
C
C           CALLING SEQUENCE:-
C                RIPPLE
C
C
C           FUNCTION:-
C                This program can be used to eliminate the zero-point ripple
C                on  PDS  produced  images  (or  on  any other image where a
C                change in level in the Y direction is desired).  The  input
C                image  is  scanned  by the program line by line, taking all
C                the points in the +/- n horizontal  lines  about  the  line
C                being  dealt  with  (where n is the user defined smoothing)
C                and calculating the mode by fitting  a  parabola  thru  the
C                histogram  about  the mode. This can then be plotted out on
C                any device. The user can then use a cursor to define levels
C                that he wants subtracted from the input image by defining a
C                value for a Y line. The computer then fills in between  the
C                Y  positions defined. The display and cursor placing can be
C                repeated,  with  new  levels  being  input  or   old   ones
C                overwritten.  The user can have all the  levels not defined
C                filled  from  the  defined  levels by  linear interpolation
C                between the defined ranges (This is also done automatically
C                at the end of the program  when the Output  image is made).
C                The total defined levels can also  be displayed on the plot
C                or cleared. When the user is satisfied, the computer  finds
C                the minimum level defined and resets the levels taking this
C                as zero and makes a new  image  by  subtracting  these  new
C                levels from the input image.
C
C
C           USE:-
C
C
C
C          USER PARAMETERS:-
C
C          IMAGE                               The input image (should be an
C                                              integer I*2 image)
C
C          NSMOOTH         3                   The  number  of  lines  about
C                                              each  line that taken for the
C                                              mode calcs.  It  smooths  the
C                                              calcs.
C
C          DEVICE          ARGS                The  device  to  display  the
C                                              mode   values   and  for  any
C                                              cursor   use.   Choices   are
C                                              NONE,ARGS,VERS,GOC,CC81,
C                                              CALCOMP,TEKTRONIX.
C
C          DEVSIZE         For device          The  physical  size  of   the
C                                              plot.
C
C          TEXT                                For  hard-copy  devices,  the
C                                              text  to be written below the
C                                              plot.
C
C          RANGE           Full                The range in values, or in  Y
C                                              line    positions    to    be
C                                              displayed.
C
C          DISVAL          No                  Flag   for   displaying   the
C                                              present     placed    levels.
C                                              Choices are YES,NO.
C
C          CURSOR          Yes                 Flag for  use  of  cursor  to
C                                              place   levels.  Choices  are
C                                              YES,NO.
C
C          CLEAR           No                  Flag for clearing the present
C                                              placed  levels.  Choices  are
C                                              YES,NO.
C
C          FILLUP          No                  Flag for filling  the  levels
C                                              which    are    outside   any
C                                              previous  placed  Y   ranges.
C                                              Choices are YES,NO.
C
C          AGAIN           Yes                 Flag for repeating the  level
C                                              display  and setting process.
C                                              Choices are Y
C
C          SAVE            Yes                 Flag  for  making  an  Output
C                                              image
C
C          OUTPUT                              The Output image
C
C          TITLE           Input Title         The title to be  attached  to
C                                              the Output image.
C
C
C          NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C
C          USE OF TRACKER-BALL BUTTONS:-
C
C          GREEN 1     None
C
C          WHITE 2     Used for telling the computer  to  take  the  present
C                      position  of  the  cursor.  If the position is to the
C                      left  of the picture, the  position is not  taken and
C                      the cursor placing ends.
C
C          WHITE 3     As 2
C
C          RED   4     None
C
C
C
C
C
C
C
C
C          AJPENNY                  RGO                            26-NOV-82
C
C
C---------------------------------------------------------------------------
C
C
C
C
      PROGRAM PDSRIPPLE
C
C
      CHARACTER CVAL*1,PRBUF*80
      INTEGER NPT(201),NVT(1),NSIZE(2)
      CHARACTER TEXT*72,TITLE*72
      LOGICAL*1 LOOP
      REAL SIZE(2)
C
C
C
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C  Get Input Image
C
      CALL GTIMG(IPIN,NSIZE,BS,BZ,INVAL,TITLE,IERR)
      NPIX = NSIZE(1)
      NLINE = NSIZE(2)
C
C  Allocate working space
C
      CALL GETDYN('VALM',204,NLINE,IPVM,IERR)
      CALL GETDYN('VALMX',204,NLINE,IPVMX,IERR)
      CALL GETDYN('ALINE',204,NLINE,IPAL,IERR)
C
C  Get number of lines to smooth over
C
      NSM = 3
      CALL RDKEYI('NSMOOTH',.TRUE.,1,NSM,NVAL,IERR)
      IF (IERR.EQ.0) THEN
         NEVEN = 2*(NSM/2)
         IF (NEVEN.EQ.0) THEN
            CALL WRUSER('NOT ODD NUMBER, INCREASED BY 1',ISTAT)
            NSM = NSM + 1
         ENDIF
      ENDIF
      IF (IERR.EQ.ERR_PARINV) THEN
         CALL WRUSER('BAD NUMBER, SET TO 3',ISTAT)
         NSM = 3
      ENDIF
      CALL CNPAR('NSMOOTH',ISTAT)
C
C  Get the  area mode values
C
      CALL DOMODE(%VAL(IPIN),NPIX,NLINE,INVAL,%VAL(IPVM),%VAL(IPVMX),
     +            NSM)
C
C ------------------------------------------------------------
C
C  Set Ripple mean line values to null
C
      CALL SETLIN(%VAL(IPAL),NLINE)
C
C  Loop displaying the modes and fitting and changing mean values
C
      LOOP = .TRUE.
      DO WHILE (LOOP)
C
C        Open the display device
C
         CALL DEVOPN(IDEV,SIZE)
C
C        Display the mode values
C
         CALL MADIS(%VAL(IPAL),NLINE,%VAL(IPVM),%VAL(IPVMX),TITLE,
     +              IDEV,SIZE,AMIN,AMAX)
C
C        Display ripple mean levels so far, if desired
C
         KW = 2
         CALL GETCMD('DISVAL','YES,NO.',1,KW,TEXT,KTEXT,IERR)
         CALL CNPAR('DISVAL',ISTAT)
         IF (KW.EQ.1) THEN
             CALL VALDIS(%VAL(IPAL),NLINE)
         ENDIF
C
C        Get Cursor placed ripple mean levels
C
         CALL CURPOS(IDEV,SIZE,AMIN,%VAL(IPAL),NLINE)
C
C        Close display device
C
         CALL DEVCLS(IDEV)
C
C        Fill up gaps in ripple mean levels, if desired
C
         KW = 2
         CALL GETCMD('FILLUP','YES,NO.',1,KW,TEXT,KTEXT,IERR)
         CALL CNPAR('FILLUP',ISTAT)
         IF (KW.EQ.1) THEN
            CALL AFILL(%VAL(IPAL),NLINE)
         ENDIF
C
C        Clear ripple mean levels, if desired
C
         KW = 2
         CALL GETCMD('CLEAR','YES,NO.',1,KW,TEXT,KTEXT,IERR)
         CALL CNPAR('CLEAR',ISTAT)
         IF (KW.EQ.1) THEN
            CALL SETLIN(%VAL(IPAL),NLINE)
         ENDIF
C
C  Get Wether display again
C
         K = 1
         CALL GETCMD('AGAIN','YES,NO.',1,K,TEXT,KK,IERR)
         CALL CNPAR('AGAIN',ISTAT)
         IF (K.EQ.1) THEN
            LOOP = .TRUE.
         ELSE
            LOOP = .FALSE.
         ENDIF
C
C
C
      ENDDO
C
C ------------------------------------------------------------
C
C
C   Get if to save new image
C
      KW = 1
      CALL GETCMD('SAVE','YES,NO.',1,KW,TEXT,KTEXT,IERR)
      CALL CNPAR('SAVE',ISTAT)
      IF (KW.EQ.1) THEN
C
C  Open output image
C
         CALL GT2DIW('OUTPUT',102,.FALSE.,NPIX,NLINE,IPOUT,IERROU)
         IF(IERROU.EQ.0) THEN
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,I,ISTAT)
            CALL CNPAR('TITLE',ISTAT)
            CALL WRDSCR('OUTPUT','TITLE',TITLE,1,ISTAT)
            CALL PTDSCR('OUTPUT','INVAL','INTEGER',INVAL,RVAL,TEXT,
     +                  IERR)
            CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,BS,TEXT,IERR)
            CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,BZ,TEXT,IERR)
         ENDIF
C
C     Load Output image
C
         IF (IERROU.EQ.0) THEN
            CALL AFILL(%VAL(IPAL),NLINE)
            CALL LOADIM(%VAL(IPIN),%VAL(IPOUT),NPIX,NLINE,%VAL(IPAL),
     +                  INVAL)
         ENDIF
      ENDIF
C
C -----------------------------------------------------------------
C
C  Free data arrays and exit
C
      CALL END PLT
      CALL FRDATA(' ',ISTAT)
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R LOADIM *
C      *            *
C      **************
C
C
C ------------------------------------------------------------
C
C
C
      SUBROUTINE LOADIM(IMIN,IMOUT,NX,NY,ALINE,INVAL)
C
C
C
      INTEGER*2 IMIN(NX,NY),IMOUT(NX,NY)
      REAL ALINE(NY)
C
C  Make new mean ripple levels as levels above lowest
C
      AMIN = ALINE(1)
      DO K = 1,NPT
         IF (ALINE(K).LT.AMIN) AMIN  = ALINE(K)
      ENDDO
C
C  Make new image by subtracting new ripple mean levels
C
      DO K = 1,NY
         L = INT(ALINE(K)-AMIN+0.5)
         DO J = 1,NX
            LL = IMIN(J,K)
            IF (LL.NE.INVAL) THEN
               AB = REAL(LL) - REAL(L)
               IF (AB.GT.32767.0) THEN
                   AB = 32767.0
               ELSE
                  IF (AB.LT.-32768.0) THEN
                     AB = -32768.0
                  ENDIF
               ENDIF
               IMOUT(J,K) = AB
            ELSE
               IMOUT(J,K) = INVAL
            ENDIF
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
C      * S/R AFILL  *
C      *            *
C      **************
C
C
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE AFILL(ALINE,NPT)
C
C
C
      REAL ALINE(NPT)
C
C  Fill up any gap at start
C
      J = 0
      KW = 1
      DO WHILE (KW.EQ.1)
         J = J + 1
         IF (ALINE(J).GT.-99999.0) KW = 0
      ENDDO
      DO K = 1,J
         ALINE(K) = ALINE(J)
      ENDDO
C
C  Fill up any gap at end
C
      J = NPT + 1
      KW = 1
      DO WHILE (KW.EQ.1)
         J = J - 1
         IF (ALINE(J).GT.-99999.0) KW = 0
      ENDDO
      DO K = NPT,J,-1
         ALINE(K) = ALINE(J)
      ENDDO
C
C  Search for and fill any gaps in body
C
      J = 0
      DO WHILE (J.LE.NPT)
         J = J + 1
         IF (ALINE(J).LT.-99999.0) THEN
            JA = J
            KW = 0
            DO WHILE (KW.EQ.0)
               JA = JA + 1
               IF (ALINE(JA).GT.-99999.0) KW = 1
            ENDDO
            AST = ALINE(J-1)
            AEND = ALINE(JA)
            R = REAL(JA-(J-1))
            SL = (AEND-AST)/R
            DO K = J,JA-1
               ALINE(K) = AST + REAL(K-J+1)*SL
            ENDDO
            J = JA
         ENDIF
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
C      * S/R MADIS *
C      *            *
C      **************
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE MADIS(ALINE,NY,VALM,VALMX,ATITLE,IDEV,SIZE,AMIN)
C
C
C
      REAL ALINE(NY)
      REAL SIZE(2),AX(2),AY(2)
      REAL VALM(NY),VALMX(NY)
      CHARACTER TEXT*72,ATITLE*72
      INTEGER TEXTA(18),KAX(2)
      EQUIVALENCE (TEXTA(1),TEXT)
C
C  If hardcopy, get text to caption graph
C
      IF (IDEV.EQ.5.OR.IDEV.EQ.6.OR.IDEV.EQ.8) THEN
         TEXT = ATITLE
         CALL RDKEYC('TEXT',.TRUE.,1,TEXT,I,ISTAT)
         CALL CNPAR('TEXT',ISTAT)
      ELSE
         TEXT = '  '
      ENDIF
C
C  Plot points
C
      CALL PACK IN(SIZE(1),SIZE(2))
      VALMAX = -32767.0
      VALMIN = 32767.0
      DO K = 1,NY
         IF (VALM(K).GT.-100000.0) THEN
            IF (VALM(K).GT.VALMAX) VALMAX = VALM(K)
            IF (VALM(K).LT.VALMIN) VALMIN = VALM(K)
         ENDIF
      ENDDO
      AX(1) = 1.0
      AX(2) = VALMX(NY)
      AY(1) = VALMIN
      AY(2) = VALMAX
      CALL WRUSER('INPUT DISPLAY RANGE FOR LEVELS',ISTAT)
      CALL RDKEYR('RANGE',.TRUE.,2,AY,NVAL,IERR)
      CALL CNPAR('RANGE',ISTAT)
      KAX(1) = AX(1)
      KAX(2) = AX(2)
      CALL WRUSER('INPUT DISPLAY RANGE FOR Y LINES',ISTAT)
      CALL RDKEYI('RANGE',.TRUE.,2,KAX,NVAL,IERR)
      CALL CNPAR('RANGE',ISTAT)
      AX(1) = REAL(KAX(1))
      AX(2) = REAL(KAX(2))
      CALL JBAXES(AX,2,SIZE(1),' ',1,AY,2,SIZE(2),' ',1)
      DO K = 1,NY
         X = VALMX(K)
         Y = VALM(K)
         IF (Y.GT.-100000.0) CALL MARK PT(X,Y,3)
      ENDDO
      CALL TITLE(2,2,TEXTA,72)
C
C
C
      AMIN = AX(1)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOMODE *
C      *            *
C      **************
C
C -------------------------------------------------------------
C
C
C
      SUBROUTINE DOMODE(MAIN,NX,NY,INVAL,VALM,VALMX,NSM)
C
C
C
      INTEGER*2 MAIN(NX,NY)
      REAL VALM(NY),VALMX(NY)
      REAL AX(2),AY(2),SIZE(2)
C
C
      KYA = NY/2 - NY/20
      KYB = KYA + NY/10
      CALL IMASPA(MAIN,NX,NY,1,NX,KYA,KYB,INVAL,AV,STD,NPT)
      KRANGE = 4.0*STD
      NRANGE = 10.0*STD
      CALL GETDYN('NUM',104,NRANGE,IPNUM,IERR)
      CALL GETDYN('XPOL',208,KRANGE,IPXPOL,IERR)
      CALL GETDYN('YPOL',208,KRANGE,IPYPOL,IERR)
      CALL GETDYN('SIGPOL',208,KRANGE,IPSIG,IERR)
C
C
C
      IF (NY.GT.4) THEN
      NH = NY/2
      NSMH = NSM/2
      NYM = NY - NSMH
      AVA = AV
      DO K = NH,NYM
         KYA = K - NSMH
         KYB = K + NSMH
         LIML = AVA - 5.0*STD
         LIMH = LIML + NRANGE - 1
         CALL CALMOD(MAIN,NX,NY,INVAL,LIML,LIMH,1,NX,KYA,KYB,AVH,
     +               KRANGE,NRANGE,%VAL(IPNUM),%VAL(IPXPOL),
     +               %VAL(IPYPOL),%VAL(IPSIG),IERR)
         IF (IERR.EQ.0) THEN
            AVA = AVH
            VALM(K) = AVH
         ELSE
            VALM(K) = -100001.0
         ENDIF
      ENDDO
      NYMA = NYM + 1
      DO K = NYMA,NY
         VALM(K) = VALM(K-1)
      ENDDO
C
C
C
      AVA = AV
      NYM = 1 + NSMH
      DO K = NH-1,NYM,-1
         KYA = K - NSMH
         KYB = K + NSMH
         LIML = AVA - 5.0*STD
         LIMH = LIML + NRANGE - 1
         CALL CALMOD(MAIN,NX,NY,INVAL,LIML,LIMH,1,NX,KYA,KYB,AVH,
     +               KRANGE,NRANGE,%VAL(IPNUM),%VAL(IPXPOL),
     +               %VAL(IPYPOL),%VAL(IPSIG),IERR)
         IF (IERR.EQ.0) THEN
            AVA = AVH
            VALM(K) = AVH
         ELSE
            VALM(K) = -100001.0
         ENDIF
      ENDDO
      NYMA = NYM - 1
      DO K = NYMA,1,-1
         VALM(K) = VALM(K+1)
      ENDDO
      ELSE
         DO K = 1,NY
            VALM(K) = 0.0
         ENDDO
      ENDIF
C
C
C
      DO K = 1,NY
         VALMX(K) = K
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
C      * S/R CALMOD *
C      *            *
C      **************
C
C -----------------------------------------------------------
C
C
C
      SUBROUTINE CALMOD(MAIN,NX,NY,INVAL,LIML,LIMH,NXA,NXB,NYA,NYB,
     +                  AV,KRANGE,NRANGE,NUM,XPOL,YPOL,SIGPOL,IERR)
C
C
C
      INTEGER*2 MAIN(NX,NY)
      INTEGER NUM(NRANGE)
      DOUBLE PRECISION XPOL(KRANGE),YPOL(KRANGE),SIGPOL(KRANGE)
      DOUBLE PRECISION ARRPOL(10),CHI,RMS
      REAL AX(2),AY(2),SIZE(2)
C
C
C
      IERR = 0
C
C
C
      DO K = 1,NRANGE
         NUM(K) = 0
      ENDDO
C
C
C
      DO K = NYA,NYB
         DO J = NXA,NXB
            KV = MAIN(J,K) - LIML + 1
            IF (KV.GE.1.AND.KV.LE.NRANGE) THEN
               IF (KV.NE.INVAL) NUM(KV) = NUM(KV) + 1
            ENDIF
         ENDDO
      ENDDO
C
C
C
      MAX = NUM(1)
      KMODE = 1
      DO K = 1,NRANGE
         IF (NUM(K).GT.MAX) THEN
            MAX = NUM(K)
            KMODE = K
         ENDIF
      ENDDO
C
C
C
      IF (NUM(KMODE).EQ.0) THEN
         IERR = 1
         AV = 0.0
      ELSE
         KA = KMODE - KRANGE/2
         KB = KA + KRANGE - 1
         IF (KA.LT.1) KA = 1
         IF (KB.GT.KRANGE) KB = KRANGE
         TOP = DBLE(NUM(KMODE))
         DO K = 1,KRANGE
            J = KA + K - 1
            YPOL(K) = DBLE(NUM(J))/TOP
            XPOL(K) = DBLE(K)
         ENDDO
         CALL POLFIT(XPOL,YPOL,SIGPOL,KRANGE,3,0,ARRPOL,CHI,RMS)
         BV = -1.0*ARRPOL(2)/(2.0*ARRPOL(3))
         IF (BV.LT.1.0.OR.BV.GT.REAL(KRANGE)) BV = REAL(KRANGE)/2.0
         AV = REAL(LIML+KA) + BV - 1.0
C
C
      ENDIF
C
C
C
      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R CURPOS *
C      *           *
C      *************
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE CURPOS(IDEV,SIZE,AMIN,ALINE,NPT)
C
C
C
      REAL ALINE(NPT)
      CHARACTER*72 TEXT
      LOGICAL LOOP
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C  See if want to use cursor
C
      KW = 1
      CALL GETCMD('CURSOR','YES,NO.',1,KW,TEXT,KTEXT,IERR)
      CALL CNPAR('CURSOR',ISTAT)
      IF (KW.EQ.1) THEN
C
C         Put out how to use
C
         IF (IDEV.EQ.3.OR.IDEV.EQ.4) THEN
            CALL WRUSER('TYPE SPACE TO GET',JSTAT)
            CALL WRUSER('POSN',JSTAT)
            CALL WRUSER('SET TO LEFT OF BOX',JSTAT)
            CALL WRUSER('TO EXIT',JSTAT)
            CALL WRUSER(' ',JSTAT)
         ENDIF
         IF (IDEV.EQ.2) THEN
            CALL WRUSER('PRESS WHITE BUTTON FOR POSN ',JSTAT)
            CALL WRUSER('SET TO LEFT OF BOX TO EXIT',JSTAT)
            CALL WRUSER(' ',JSTAT)
         ENDIF
C
C        Get posns and put out as values, until posn to left of diag picked
C        Draw a line between posns and store posns
C        Fill in all ALINE points between cursor defined points
C
         KW = 0
         CALL BREAK
         LOOP = .TRUE.
         DO WHILE (LOOP)
            CALL CURSOR(XA,YA)
            IF (XA.GE.AMIN) THEN
               KXA = XA
               IF (KXA.LT.1) KXA = 1
               IF (KXA.GT.NPT) KXA = NPT
               XA = REAL(KXA)
               WRITE(TEXT,902)KXA,YA
  902          FORMAT(1H ,I6,2X,G14.4)
               CALL WRUSER(TEXT,JSTAT)
               CALL JOIN PT(XA,YA)
               ALINE(KXA) = YA
               IF (KW.NE.0) THEN
                  IF (KXB.NE.KXA) THEN
                     R = REAL(KXA-KXB)
                     SL = (ALINE(KXA)-ALINE(KXB))/R
                     IF (KXA.GT.KXB) THEN
                        DO J = KXB,KXA
                           ALINE(J) = ALINE(KXB) + SL*REAL(J-KXB)
                        ENDDO
                     ELSE
                        DO J = KXB,KXA,-1
                           ALINE(J) = ALINE(KXB) + SL*REAL(J-KXB)
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
               KW = 1
               KXB = KXA
            ELSE
               LOOP = .FALSE.
            ENDIF
         ENDDO
C
C
C
      ENDIF
C
C
C
      END




C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R SETLIN *
C      *            *
C      **************
C
C
C ------------------------------------------------------------
C
C
C
      SUBROUTINE SETLIN(ALINE,NPT)
C
C
C
      REAL ALINE(NPT)
C
C
C
      DO K = 1,NPT
         ALINE(K) = -100000.0
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R VALDIS *
C      *            *
C      **************
C
C
C -----------------------------------------------------------
C
C
C
      SUBROUTINE VALDIS(ALINE,NLINE)
C
C
C
      REAL ALINE(NLINE)
C
C
C
      CALL BREAK
      DO K = 1,NLINE
         IF (ALINE(K).LT.-99999.0) THEN
            CALL BREAK
         ELSE
            XA = K
            YA = ALINE(K)
            CALL JOIN PT(XA,YA)
         ENDIF
      ENDDO
C
C
C
      END



