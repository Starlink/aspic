C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   MAGRMS  *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               MAGRMS
C
C
C          FUNCTION:-
C               This plots out a graph of the RMS errors of the measures of
C               stars  done  by APERCUR,APERMAG,GAUMAG,LORSIM,LORMUL,LORCUR,
C               against their estimated magnitudes.
C               The cursor can then be used to define a series of Magnitude,
C               RMS pairs which are then stored as MAGAV input.
C
C
C          USE:-
C               This is used for deciding which stars in the output of  one
C               of  the  above  photometry programs has residuals which are
C               significantly higher than the average for a  star  of  that
C               magnitude   and  is  thus  probably  a  bum  measure, and for
C               making a file of max acceptable RMS values at different
C               magnitude levels for use in MAGAV.
C                 The values of Magnitude vs RMS excess are plotted out, and
C               the cursor is then used to define the highest acceptable RMS
C               value at any number of Mag values (up to 1000 positions).
C               (The RMS excess is the value above a calculated line which
C               curves up to the bright stars to make use of the cursor
C               easy. The true RMS values can be seen from the number
C               plotted out when using the cursor or by inspection of the
C               Output file.)
C               The program then calcs matching values at 0.5mag intervals
C               from the faintest mag up to 8mag brighter, and plots
C               them out. The whole display and cursor work can be repeated.
C               At the end the mathcing values are stored in a .BDF file
C               for use by MAGAV.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The input magnitude list made
C                                             by    one    of   the   above
C                                             photometry programs
C
C         ALLSTARS        NO                  Flag for accepting a star for
C                                             plotting   out,  even  if  it
C                                             could be  rejecte  anyway  on
C                                             other  grounds  (i.e.  as too
C                                             far  from  expected  posn  or
C                                             fitting  iterations  were too
C                                             lengthy or image contained
C                                             invalid points or magnitude
C                                             flagged as 0.0 or 50.0).
C                                             Choices are YES,NO
C
C         OFFCEN          2.0                 If going to reject a star for
C                                             plotting   as  too  far  from
C                                             expected  posn,  t   is   the
C                                             distance  away in X or Y that
C                                             is the limit of acceptance
C
C         NUMITS          35                  If going to reject a star for
C                                             plotting   as   too   lengthy
C                                             fitting iteration this is the
C                                             limit   to   the   number  of
C                                             iterations      that       is
C                                             acceptable.
C
C         DEVICE          ARGS                Choice of NONE,
C                                             ARGS,GOC,TEKTRONIX,VERSATEC
C                                             CALCOMP,CC81
C                                             for the plot to come out on
C
C         DEVSIZE        Full                 Physical size of graph to be
C                                             plotted out
C
C         DEVLIMY,X      Whole                The limits of the plots in
C                                             RMS residuals and Magnitudes.
C                                             (NB RMS residuals above
C                                             allowance curve.)
C
C         DOTNUM         No                   Flag for numbering of the
C                                             points on the plot, in
C                                             sequential order of the input
C                                             list. Choices are YES,NO.
C
C         TEXT                                If put  out  on  VERSATEC,  a
C                                             line  of  text is written out
C                                             under the plot
C
C         AGAIN          No                   Flag for putting out the
C                                             graph again. Choices are YES,
C                                             NO.
C
C         XYWOUT                              File for storing RMS values
C
C         XYWTIT         Output               Title to be attached to the
C                                             output file
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C               None
C
C
C         USE OF TRACKERBALL BUTTONS
C
C
C         All have the same function. When the cursor is lit, pressing
C         one reads the present position of the cursor into a
C         temporary store. If the cursor is to the left of the left
C         edge of the graph, the aquisition of positions is ended.
C         The positions are then used to make the Output file.
C
C         A.J.Penny                RGO                             83-8-14
C
C
C--------------------------------------------------------------------------



C
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      CHARACTER*72 TEXT
      INTEGER TEXTA(15)
      EQUIVALENCE (TEXTA(1),TEXT)
      REAL X(10000),Y(10000),AX(2),AY(2),SIZE(2),DATA(2,100)
      REAL RMSMAG(2,1000)
      CHARACTER*1 KALL
      LOGICAL VALID,LOOP
C
C  Set continuation flag
C
      VALID = .TRUE.
C
C  Get Magnitude list
C
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPR,IERR)
      IF (IERR.NE.0.OR.NITEM.LT.13) THEN
         CALL WRUSER('BAD FILE',JSTAT)
         VALID = .FALSE.
      ENDIF
C
C  Get star acceptance conditions
C
      IF (VALID) THEN
         K = 2
         CALL GETCMD('ALLSTARS','YES,NO.',1,K,TEXT,KTEXT,IERR)
         IF (K.EQ.1) THEN
            KALL = 'Y'
         ELSE
            KALL = 'N'
         ENDIF
         IF (KALL.EQ.'N') THEN
            DLIM = 2.0
            CALL GETPAR('OFFCEN','REAL',1,0.0,1.0E10,.TRUE.,IVAL,
     +                  DLIM,IERR)
            IF (IERR.NE.0) THEN
                CALL WRUSER('BAD NUMBER',ISTAT)
                VALID = .FALSE.
            ELSE
               ITLIM = 35
               CALL GETPAR('NUMITS','INTEGER',1,0.0,1000.0,.TRUE.,
     +                     ITLIM,RVAL,IERRA)
               IF (IERRA.NE.0) THEN
                  CALL WRUSER('BAD NUMBER',ISTAT)
                  VALID = .FALSE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C ------------------------------------------------------------
C
C  Get the star magniutdes and RMSs and number of stars
C
      IF (VALID) THEN
         KTOT = 0
         KTOTA = 0
         DO L = 1,LSTLEN
            CALL GETDAT(%VAL(IPR),NITEM,LSTLEN,L,XA,YA,AMA,DX,DY,
     +                  ITER,RMS,INVAL)
            IF(RMS.GT.0.000001.AND.AMA.LT.49.9) THEN
               IF(KALL.EQ.'Y') THEN
                  KTOT = KTOT + 1
                  X(KTOT) = AMA
                  Y(KTOT) = RMS
               ELSE
                  IF(ITER.LT.ITLIM.AND.ABS(DX).LT.DLIM.AND.
     +               ABS(DY).LT.DLIM.AND.INVAL.EQ.0) THEN
                     KTOT = KTOT + 1
                     X(KTOT) = AMA
                     Y(KTOT) = RMS
                  ENDIF
               ENDIF
            ELSE
               KTOTA = KTOTA + 1
            ENDIF
         ENDDO
C
C  Print out results so far
C
         CALL WRUSER(' ',JSTAT)
         WRITE(TEXT,900)LSTLEN
  900    FORMAT(1H ,'TOTAL NO OF STARS = ',I6)
         CALL WRUSER(TEXT,JSTAT)
         WRITE(TEXT,901)KTOTA
  901    FORMAT(' ','NO OF STARS WITH NO FITS =',I6)
         CALL WRUSER(TEXT,JSTAT)
         IF (KALL.NE.'Y') THEN
            KTOTB = LSTLEN - KTOT - KTOTA
            WRITE(TEXT,902)KTOTB
  902       FORMAT(1H ,'NO OF STARS WITH TOO POOR FIT = ',I6)
            CALL WRUSER(TEXT,JSTAT)
         ENDIF
         WRITE(TEXT,910)KTOT
  910    FORMAT(' ','NO OF STARS PLOTTED = ',I6)
         CALL WRUSER(TEXT,ISTAT)
         CALL WRUSER(' ',JSTAT)
      ENDIF
C
C  Remove RMS growth law
C
      IF (VALID) THEN
         CALL RMSLAW(Y,X,10000,KTOT,SLOPE,ZMAG,ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL WRUSER('THE DATA ARE CRAZY OR TOO FEW',ISTAT)
            VALID = .FALSE.
         ENDIF
         DO K = 1,KTOT
            F = 10.0**((ZMAG-X(K))/2.5)
            Y(K) = Y(K) - SLOPE*F
         ENDDO
      ENDIF
C
C  Display diagram
C
      IF (VALID) THEN
         LOOP = .TRUE.
         DO WHILE (LOOP)
C
C  Plot diagram
C
            CALL DEVOPN(IDEV,SIZE)
            IF (IDEV.NE.1) THEN
               CALL DOPLT(SIZE,X,Y,KTOT,AMIN)
C
C  Use cursor (if can) to get posns on diag and fix limit line
C
               IF (IDEV.EQ.2.OR.IDEV.EQ.3.OR.IDEV.EQ.4) THEN
                  NUM = 0
                  CALL DOCUR(IDEV,AMIN,RMSMAG,1000,NUM,SLOPE,ZMAG)
                  CALL FIXRMS(RMSMAG,1000,NUM,DATA,SLOPE,ZMAG)
                  CALL PLINE(DATA,100,SLOPE,ZMAG)
               ENDIF
C
C  If outputting to hard copy, write a caption
C
               IF (IDEV.EQ.5.OR.IDEV.EQ.6.OR.IDEV.EQ.8) THEN
                  CALL WRUSER('TEXT TO WRITE BELOW DIAG?',ISTAT)
                  CALL RDKEYC('TEXT',.FALSE.,1,TEXT,I,ISTAT)
                  CALL CNPAR('TEXT',ISTAT)
                  CALL TITLE(2,2,TEXTA,60)
               ENDIF
               CALL DEVCLS(IDEV)
            ENDIF
C
C  See if display again
C
            CALL WRUSER('DISPLAY AGAIN ?',ISTAT)
            KW = 2
            CALL GETCMD('AGAIN','YES,NO.',1,KW,TEXT,KTEXT,IERR)
            CALL CNPAR('AGAIN',ISTAT)
            IF (KW.EQ.2) LOOP = .FALSE.
C
C
C
         ENDDO
C
C
      ENDIF
C
C --------------------------------------------------------------
C
C  Store aquired values
C
      IF (NUM.NE.0) THEN
         CALL XYLWRI(DATA,2,100,1,100,1,2,IERR)
      ELSE
         CALL WRUSER('NO FILE OUTPUT',ISTAT)
      ENDIF
C
C  Tidy up and exit
C
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
C
C
C
      END



C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       **************
C       *            *
C       * S/R GETDAT *
C       *            *
C       **************
C
C      This extracts wanted data from the input file raster
C
C ---------------------------------------------------------
C
C
      SUBROUTINE GETDAT(DATA,NITEM,LSTLEN,K,
     +                  X,Y,AMAG,DX,DY,ITER,RMS,NINVAL)
C
C
C
      REAL DATA(NITEM,LSTLEN)
C
C
C
      X = DATA(6,K)
      Y = DATA(7,K)
      AMAG = DATA(8,K)
      DX = DATA(9,K)
      DY = DATA(10,K)
      ITER = INT(DATA(11,K)+0.001)
      RMS = DATA(12,K)
      NINVAL = INT(DATA(13,K)+0.001)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R DOPLT *
C      *           *
C      *************
C
C
C
C -------------------------------------------------------------
C
C
C
       SUBROUTINE DOPLT(SIZE,X,Y,LTOT,ALHD)
C
C
C
      CHARACTER*1 DOTS
      CHARACTER*72 TEXT
      REAL X(10000),Y(10000),AX(2),AY(2),SIZE(2)
      CHARACTER*4 TEXTB(2)
      INTEGER TI(2)
      EQUIVALENCE (TEXTB(1),TI(1))
      EQUIVALENCE (TEXTB(2),TI(2))
C
C  Get ranges of mags
C
      AMINA = 30000.0
      AMAXA = -30000.0
      AMINB = 30000.0
      AMAXB = -30000.0
      DO L = 1,LTOT
         IF(Y(L).GT.AMAXA) AMAXA = Y(L)
         IF(Y(L).LT.AMINA) AMINA = Y(L)
         IF(X(L).GT.AMAXB) AMAXB = X(L)
         IF(X(L).LT.AMINB) AMINB = X(L)
      ENDDO
      AY(1) = AMINA - 0.05*(AMAXA-AMINA)
      IF (AY(1).LT.0.0) AY(1) = 0.0
      AY(2) = AMAXA + 0.05*(AMAXA-AMINA)
      CALL WRUSER('INPUT RMS LIMITS',ISTAT)
      CALL RDKEYR('DEVLIMY',.TRUE.,2,AY,I,ISTAT)
      CALL CNPAR('DEVLIMY',ISTAT)
      CALL WRUSER('INPUT MAG LIMITS',ISTAT)
      AX(1) = AMINB - 0.05*(AMAXB-AMINB)
      AX(1) = AINT(AX(1))
      AX(2) = AMAXB + 0.05*(AMAXB-AMINB)
      AX(2) = AINT(AX(2)) + 1.0
      CALL RDKEYR('DEVLIMX',.TRUE.,2,AX,I,ISTAT)
      CALL CNPAR('DEVLIMX',ISTAT)
      ALHD = AX(1)
      DOTS = 'N'
      KW = 2
      CALL GETCMD('DOTNUM','YES,NO.',1,KW,TEXT,KTEXT,IERR)
      CALL CNPAR('DOTNUM',ISTAT)
       IF (KW.EQ.1) THEN
          DOTS = 'Y'
       ELSE
          DOTS = 'N'
       ENDIF
      XL = SIZE(1)
      YL = SIZE(2)
C
C
      CALL NEWPLT(AX(1),AX(2),XL,AY(1),AY(2),YL)
      WRITE(TEXTB(1),900)
  900 FORMAT('OVER')
      WRITE(TEXTB(2),901)
  901 FORMAT('MAG ')
      CALL DRAW AX(TI(1),4,AX(1),90.0)
      CALL DRAW AX(TI(2),4,AY(1),0.0)
C
      DO L = 1,LTOT
         XX = X(L)
         YY = Y(L)
         IF(DOTS.EQ.'N') THEN
            CALL MARK PT(XX,YY,3)
         ELSE
            CALL NUMB PT(XX,YY,3,L)
         ENDIF
      ENDDO
      CALL BREAK
      CALL JOIN PT(AX(2),AY(1))
      CALL JOIN PT(AX(2),AY(2))
      CALL JOIN PT(AX(1),AY(2))
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R DOCUR *
C      *           *
C      *************
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE DOCUR(IDEV,AMIN,RMSMAG,NF,NUM,SLOPE,ZMAG)
C
C
C
      CHARACTER*72 TEXT
      LOGICAL LOOP
      REAL RMSMAG(2,NF)
C
C
C
      CALL WRUSER('PLOT OF REDUCED RMS',ISTAT)
      WRITE (TEXT,900)SLOPE,ZMAG
  900 FORMAT(' ','RMS reduced by S.10**((Z-mag)/2.5) , S= ',F6.2,
     +       '  Z= ',F6.2)
      CALL WRUSER(TEXT,ISTAT)
      IF (IDEV.EQ.3.OR.IDEV.EQ.4) THEN
         CALL WRUSER('CURSOR RETURNS ACTUAL RMS',ISTAT)
         CALL WRUSER('TYPE SPACE TO GET',JSTAT)
         CALL WRUSER('POSN',JSTAT)
         CALL WRUSER('SET TO LEFT OF BOX',JSTAT)
         CALL WRUSER('TO EXIT',JSTAT)
         CALL WRUSER(' ',JSTAT)
      ENDIF
      IF (IDEV.EQ.2) THEN
         CALL WRUSER('CURSOR RETURNS ACTUAL RMS',ISTAT)
         CALL WRUSER('PRESS WHITE BUTTON FOR POSN ',JSTAT)
         CALL WRUSER('SET TO LEFT OF BOX TO EXIT',JSTAT)
         CALL WRUSER(' ',JSTAT)
      ENDIF
      CALL WRUSER('    MAG        RMS',ISTAT)
      LOOP = .TRUE.
      DO WHILE (LOOP)
         CALL CURSOR(XA,YA)
         IF (XA.GE.AMIN) THEN
            YB = YA + SLOPE*(10.0**((ZMAG-XA)/2.5))
            WRITE(TEXT,902)XA,YB
  902       FORMAT(1H ,F9.2,2X,F9.2)
            CALL WRUSER(TEXT,JSTAT)
            IF (NUM.LT.NF) THEN
               NUM = NUM + 1
               RMSMAG(1,NUM) = XA
               RMSMAG(2,NUM) = YA
            ENDIF
         ELSE
            LOOP = .FALSE.
         ENDIF
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R FIXRMS *
C      *            *
C      **************
C
C
C  This s/r calcs at set of 100 mag,RMS pairs to match the input
C  pairs.
C
C  AJPENNY               RGO                          82-12-31
C ------------------------------------------------------------------
C
C
C
      SUBROUTINE FIXRMS(RMSMAG,NF,NUM,DATA,SLOPE,ZMAG)
C
C
C
      REAL RMSMAG(2,NF),ARMS(100),AMAG(100),DATA(2,100)
C
C  Sort into ascending mag list
C
         DO I = NUM,2,-1
            DO J = 1,I-1
               JG = J + 1
               IF (RMSMAG(1,J).GT.RMSMAG(1,JG)) THEN
                  VK = RMSMAG(1,J)
                  RMSMAG(1,J) = RMSMAG(1,JG)
                  RMSMAG(1,JG) = VK
                  VK = RMSMAG(2,J)
                  RMSMAG(2,J) = RMSMAG(2,JG)
                  RMSMAG(2,JG) = VK
               ENDIF
            ENDDO
         ENDDO
C
C Make into a series of 100 pairs at 0.2 mag spacing
C
      AMAX = RMSMAG(1,NUM)
      AMAXA = REAL(INT(AMAX*5.0))/5.0 + 0.2
      AMAG(1) = AMAXA
      ARMS(1) = RMSMAG(2,NUM)
      DO K = 2,100
         AMAG(K) = AMAXA - 0.2*(REAL(K-1))
         AMHERE = AMAG(K)
C
C        Find mag just fainter and brighter than here
C
         KW = 1
         L = NUM
         DO WHILE (KW.EQ.1)
            L = L - 1
            IF (L.LT.1) THEN
               KW = 0
               ARMS(K) = RMSMAG(2,1)
            ELSE
               DIFF = AMHERE - RMSMAG(1,L)
               IF (DIFF.GT.0.0) THEN
                  KW = 0
                  ALO = RMSMAG(2,L+1)
                  AHI = RMSMAG(2,L)
                  AMLO = RMSMAG(1,L+1)
                  AMHI = RMSMAG(1,L)
                  AM = (AMHERE-AMLO)/(AMHI-AMLO)
                  ARMS(K) = RMSMAG(2,L+1) + AM*(AHI-ALO)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
C
C  Store result allowing for reduced values
C
      DO K = 1,100
         DATA(1,K) = AMAG(K)
         DATA(2,K) = ARMS(K) + SLOPE*(10.0**((ZMAG-AMAG(K))/2.5))
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
C      * S/R PLINE  *
C      *            *
C      **************
C
C
C  This s/r plots the real RMS line on the device
C
C    AJPENNY                    RGO                83-1-1
C ---------------------------------------------------------
C
C
C
      SUBROUTINE PLINE(DATA,NUM,SLOPE,ZMAG)
C
C
C
      REAL DATA(2,NUM)
C
C
C
      CALL BREAK
      DO L = 1,NUM
         X = DATA(1,L)
         Y = DATA(2,L) - SLOPE*(10.0**((ZMAG-X)/2.5))
         CALL JOIN PT(X,Y)
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
C      * S/R RMSLAW *
C      *            *
C      **************
C
C
C   PURPOSE
C
C   ARGUMENTS
C  IN
C  IN/OUT
C  OUT
C
C   STARLINK PARAMETERS
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
C   A.J.PENNY                   RGO                    83-8-14
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE RMSLAW(RMS,AMAG,NS,NUM,SLOPE,AMG,ISTAT)
C
C
C
      REAL RMS(NS),AMAG(NS),RMIN(25),X(25)
      DOUBLE PRECISION TA(25),TB(25),TC(25)
C
C
C
      ISTAT = 0
      IF (NS.LT.4.OR.NUM.LT.4) THEN
         ISTAT = 1
      ELSE
         AMIN = RMS(1)
         DO K = 2,NUM
            IF (RMS(K).LT.AMIN) AMIN = RMS(K)
         ENDDO
         AMG = AMAG(1)
         DO K = 1,NUM
            IF (AMAG(K).GT.AMG) AMG = AMAG(K)
         ENDDO
C
C
C
         DO K = 1,25
            RMIN(K) = 1.0E20
            X(K) = 0.0
         ENDDO
         DO K = 1,NUM
            DIFF = AMG - AMAG(K)
            IF ((DIFF.GT.2.1).AND.(DIFF.LT.12.4)) THEN
               L = 2.0*DIFF + 1.0
               IF (RMS(K).LT.RMIN(L)) THEN
                  RMIN(L) = RMS(K)
                  X(L) = AMAG(K)
               ENDIF
            ENDIF
         ENDDO
C
C
C
         DO K = 1,24
            L = 1
            DO WHILE (L.EQ.1)
               IF (RMIN(K).GT.1.0E19) THEN
                  LC = 0
                  DO J = K,24
                     IF (RMIN(J+1).LT.1.0E19) LC = 1
                     RMIN(J) = RMIN(J+1)
                     X(J) = X(J+1)
                  ENDDO
               ENDIF
               IF ((RMIN(K).LT.1.0E19).OR.(LC.EQ.0)) L = 0
            ENDDO
         ENDDO
         NK = 0
         DO K = 1,25
            IF (RMIN(K).LE.1.0E19) THEN
               NK = NK + 1
               RMIN(K) = RMIN(K) - AMIN
            ENDIF
         ENDDO
C
C
C
         IF (NK.LE.2) THEN
            SLOPE = 0.0
            ZERO = AMAG(1)
         ELSE
            DO J = 1,NK
               X(J) = 10.0**((AMG-X(J))/2.5)
            ENDDO
            CALL FIT(X,RMIN,NK,SLOPE,ZERO,TA,TB,TC,IERR)
            IF (IERR.NE.0) ISTAT = 1
         ENDIF
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
C      * S/R FIT    *
C      *            *
C      **************
C
C
C   PURPOSE
C     This takes N X,Y points and fits a line to them and returns
C     the values of the coefficients.
C
C   ARGUMENTS
C   IN
C       X Real(N)   The X values
C       Y Real(N)   The Y values
C       N Inte      The number of points
C   OUT
C       SL Real    The slope
C       ZERO Real   The zero
C       IERR  Int     Error flag (=0 for OK)
C
C   CALLS
C     Grasp
C       POLFIT
C
C   USES
C     Double precision inside
C
C   A.J.PENNY                   RGO                    83-8-14
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE FIT(X,Y,N,SL,ZERO,DX,DY,SD,IERR)
C
C
C
      REAL X(N),Y(N)
      DOUBLE PRECISION DX(N),DY(N),SD(N),A(3),CHI,RMS
C
C
C
      IERR = 0
C
C
C
      XMAX = X(1)
      YMAX = Y(1)
      DO K = 2,N
         IF (X(K).GT.XMAX) XMAX = X(K)
         IF (Y(K).GT.YMAX) YMAX = Y(K)
      ENDDO
      XMIN = X(1)
      YMIN = Y(1)
      DO K = 2,N
         IF (X(K).LT.XMIN) XMIN = X(K)
         IF (Y(K).LT.YMIN) YMIN = Y(K)
      ENDDO
      XR = XMAX - XMIN
      YR = YMAX - YMIN
      DO K = 1,N
         DX(K) = DBLE((X(K)-XMIN)/XR)
         DY(K) = DBLE((Y(K)-YMIN)/YR)
         SD(K) = 1.0
      ENDDO
C
C
C
      CALL POLFIT(DX,DY,SD,N,3,0,A,CHI,RMS)
C
C
C
      SL = A(2)*YR/XR
      ZERO = A(1)*YR + YMIN - A(2)*YR*XMIN/XR
C
C
C
      END


