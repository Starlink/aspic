
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   MAGAV   *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               MAGAV
C
C
C          FUNCTION:-
C               This  takes  magnitude  lists  created   by   the   stellar
C               photometry  programs  VOLMAG,GAUMAG,LORSIM,LORMUL,or LORCUR
C               and averages them. It calculates a mean difference for
C               each file from file 1 and subtracts it to make common
C               mags for the averages.
C                 The mean differences are calculated by comparing
C               pairs of good magnitudes and averaging and then displaying
C               the residuals to allow the user to adjust the mean by
C               using the cursor (except if a hard-copy Device is used,
C               when the calculated value is taken).
C                 In  calculating the output mag for each star only 'good'
C               magnitudes are taken and then only if they agree to within
C               a certain limit.
C                 The definition of 'good' is user defined and checks
C               star centering, no of iterations, RMS residual, invalid
C               pixels and difference from the mags in the other files.
C               lists, it ignores measures of bad fits in making the average
C               for a star, and makes two lists, a MAGAV.LIS file for later
C               listing and an Output file for input to MAGDIAG.
C
C
C          USE:-
C
C
C
C         USER PARAMETERS:-
C
C         OFFCEN          2.0                 Distance in X or Y  star  can
C                                             be  from  its  box centre and
C                                             still be accepted
C
C         CENACC          NO                  Flag  for  accepting  a  star
C                                             measure  if its only fault is
C                                             that it is too far off  centre.
C                                             Choices are YES,NO
C
C         USERMS          NO                  Flag for using the RMS values
C                                             for accepting a star. Choices
C                                             are YES,NO
C
C         RMSFAC          1.0                 Factor for use on the listing
C                                             of the matches. The RMSs  are
C                                             listed as integers.  If  they
C                                             small you may want  them  put
C                                             out multiplied by a factor so
C                                             that they dont all look  like
C                                             0s and 1s.
C
C         NUMINV          0                   The number of invalid  points
C                                             per    star    allowed    for
C                                             acceptance.
C
C         NUMITS          35                  The  number   of   iterations
C                                             allowed  in  the star fitting
C                                             for acceptance.
C
C         DIFLIM          0.20                The magnitude difference from
C                                             the mean a star can have and
C                                             still be accepted.
C
C         INPUT                               The  star   magnitude   input
C                                             files.
C
C         FAINT                               The faint magnitude  where
C                                             the RMS limit list starts.
C
C         RMSFILE                             The file containing the RMS
C                                             values, at 0.2mag steps, above
C                                             which a star is rejected. These
C                                             files, if used, must have
C                                             the RMS value of the
C                                             the faintest mag first, be in
C                                             steps of 0.2mags starting at
C                                             the faint mag. If less
C                                             than 100 values are input, then
C                                             the RMS values are filled up to
C                                             100 with the last. More than 100
C                                             values are ignored.
C
C         OUTPUT                              The Output file name.
C
C         TITLE    Output from MAGAV          The Title to be  inserted  in
C                                             the Output file.
C
C         DEVICE        ARGS                  Device to display graph of
C                                             residuals of an input file
C                                             from the mags in file 1.
C                                             Choices are ARGS,GOC,VERSATEC,
C                                             TEKTRONIX,CC81,CALCOMP.
C
C         DEVSIZE      Various                Physical size of plot in cm
C
C         DEVLIM       Min,Max                The ranges of diffs and mags
C                                             to be plotted out.
C
C         DOTNUM       N                      Flag for plotting point numbers
C                                             with points. Choices are Y/N.
C
C         AGAIN        NO                     Flag for redisplaying plot and
C                                             reusing cursor. Choices are
C                                             YES,NO.
C
C
C
C
C         USE OF TRACKERBALL BUTTONS
C           Press any of them when you have placed the horizontal line
C         of the cursor at the correct height.
C
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         A.J.Penny                RGO                             83-8-14
C
C
C--------------------------------------------------------------------------



C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      INTEGER ITER(5),NINVAL(5),LDIFF(20,5),KBAD(5)
      INTEGER IPR(5),NITEM(5),NUMG(5)
      REAL ALRMS(5),RMS(5),RMSL(100,5),RRVAL(1,1)
      REAL AMAG(5),AVDIFF(5),DX(5),DY(5),X(5),Y(5)
      CHARACTER*4 NAME*12
      CHARACTER CVAL*1,TITLE*30,KCEN*1,KIFRMS*1
      CHARACTER*72 TEXT,TEXTI(5),TEXTO
      REAL TEMP(1)
C
C
C  Get the conditions for accepting a star from a file
C
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('INPUT THE VARIOUS BAD STAR FLAG LIMITS',ISTAT)
      DCEN = 2.0
      CALL GETPAR('OFFCEN','REAL',1,0.0,1000.0,.TRUE.,IVAL,
     +            DCEN,IERR)
      K = 2
      CALL GETCMD('CENACC','YES,NO.',1,K,TEXT,KTEXT,IERR)
      IF (K.EQ.1) THEN
         KCEN = 'Y'
      ELSE
         KCEN = 'N'
      ENDIF
      CALL GETCMD('USERMS','YES,NO.',1,K,TEXT,KTEXT,IERR)
      IF (K.EQ.1) THEN
         KIFRMS = 'Y'
      ELSE
         KIFRMS = 'N'
      ENDIF
      RMSFAC = 1.0
      CALL GETPAR('RMSFAC','REAL',1,0.0,1.0E10,.TRUE.,IVAL,
     +            RMSFAC,IERR)
      NUMINV = 0
      CALL GETPAR('NUMINV','INTEGER',1,0.0,10.0E6,.TRUE.,NUMINV,
     +            RVAL,IERR)
      NUMITS = 35
      CALL GETPAR('NUMITS','INTEGER',1,0.0,100.0,.TRUE.,NUMITS,
     +            RVAL,IERR)
      DIFLIM = 0.2
      CALL GETPAR('DIFLIM','REAL',1,-100.0,100.0,.TRUE.,IVAL,DIFLIM,
     +            IERR)
      CALL WRUSER(' ',ISTAT)
C
C ------------------------------------------------------------
C  Get the input magnitude files
C
      CALL WRUSER(' ',ISTAT)
      K = 0
      LOOP = 1
      DO WHILE (LOOP.EQ.1)
         K = K + 1
         CALL GTXYLR('INPUT',.TRUE.,NITEM(K),LSTLNA,IPR(K),IERR)
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TEXTI(K),
     +               IERRA)
         CALL CNPAR('INPUT',ISTAT)
         IF (IERR.NE.0) THEN
            K = K - 1
            IF (K.NE.0) THEN
               LOOP = 0
            ELSE
               CALL WRUSER('MUST HAVE AT LEAST ONE FILE',ISTAT)
            ENDIF
         ELSE
            IF (K.EQ.1) LSTLEN = LSTLNA
            IF (NITEM(K).GE.13.AND.LSTLNA.EQ.LSTLEN) THEN
               IF (KIFRMS.EQ.'Y') THEN
                  CALL WRUSER('RMS VALUE FILE IS ?',ISTAT)
                  LOOPA = 0
                  DO WHILE (LOOPA.EQ.0)
                     CALL GTXYLR('RMSFILE',.TRUE.,NITRMS,LSTRMS,IPRMS,
     +                           IERR)
                     CALL CNPAR('RMSFILE',ISTAT)
                     IF (NITRMS.LT.7) THEN
                        CALL WRUSER('TOO FEW PARAMS,TRY AGAIN',ISTAT)
                     ELSE
                        LOOPA = 1
                     ENDIF
                  ENDDO
                  CALL EXTLSA(%VAL(IPRMS),NITRMS,LSTRMS,1,1,6,6,
     +                        RRVAL,1,1,1,1)
                  ALRMS(K) = RRVAL(1,1)
                  DO L = 1,LSTRMS
                     CALL EXTLSA(%VAL(IPRMS),NITRMS,LSTRMS,L,L,7,7,
     +                           RRVAL,1,1,1,1)
                     RMSL(L,K) = RRVAL(1,1)
                  ENDDO
                  IF (LSTRMS.LT.100) THEN
                     DO L = LSTRMS,100
                        RMSL(L,K) = RMSL(LSTRMS,K)
                     ENDDO
                  ENDIF
               ENDIF
               IF (K.EQ.5) THEN
                  LOOP = 0
               ENDIF
            ELSE
               CALL WRUSER('FILE BAD,TRY AGAIN',ISTAT)
            ENDIF
         ENDIF
      ENDDO
      KTOT = K
C
C --------------------------------------------------------
C
C  Open printing file
C
      KUNIT = 9
      OPEN(UNIT=KUNIT,NAME='MAGAV.LIS',TYPE='NEW')
C
C  Get Output file
C
      CALL WRUSER(' ',ISTAT)
      CALL GTXYLW('OUTPUT',.FALSE.,9,LSTLEN,IPO,IERR)
      CALL PTDSCR('OUTPUT','NITEM','INTEGER',9,RVAL,CVAL,IERR)
      CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,IERR)
      TITLE = 'Output from MAGAV'
      CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,IERR)
      CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,
     +            TITLE,IERR)
      WRITE(TEXTO,900)TITLE
  900 FORMAT(' ',A30)
C
C
C --------------------------------------------------------
C  Go through each file calculating for good stars the difference
C  from the star in File 1, and use that to find the mean difference
C  which is put in AVDIFF, and information about the distribution of
C  of differences into LDIFF.
C
      CALL FILAVS(IPR,KTOT,NITEM,LSTLEN,NUMITS,KIFRMS,DCEN,NUMINV,
     +            ALRMS,RMSL,AVDIFF,LDIFF,DIFLIM,NUMG)
C
C ------------------------------------------------------------
C  Write out the header information to the Print file
C
      CALL WRHEAD(KUNIT,DCEN,KCEN,KIFRMS,NUMINV,NUMITS,
     +            KTOT,ALRMS,RMSL,RMSFAC,AVDIFF,LDIFF,
     +            TEXTI,TEXTO,NUMG,DIFLIM)
C
C -------------------------------------------------------------
C
C  Go through star making the best estimate of mag and position
C
C
      DO KLIST = 1,LSTLEN
C
C  Get the data on this star
C
         DO K = 1,KTOT
             CALL GETDAT(%VAL(IPR(K)),NITEM(K),LSTLEN,KLIST,X(K),
     +            Y(K),AMAG(K),DX(K),DY(K),ITER(K),RMS(K),NINVAL(K))
         ENDDO
C
C  Calc best mean mag and posn and number of good estimates
C
         CALL AVSTAR(KTOT,AMAG,X,Y,DX,DY,ITER,RMS,NINVAL,AVDIFF,
     +               KIFRMS,ALRMS,RMSL,KCEN,DCEN,NUMINV,DIFLIM,
     +               NUMITS,AX,AY,AM,NUM,KBAD)
C
C  Print and store this result and the various parameters
C
         CALL WRLINE(%VAL(IPR(1)),NITEM(1),LSTLEN,%VAL(IPO),KLIST,
     +               KUNIT,KTOT,KIFRMS,KCEN,AVDIFF,AMAG,KBAD,DX,DY,
     +               ITER,RMS,RMSFAC,NINVAL,AM,AX,AY,NUM)
C
C
C
      ENDDO
C
C ----------------------------------------------------------------
C
C  Tidy up and exit
C
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
C
C
C
      END



C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R FILAVS *
C      *            *
C      **************
C
C       This subroutine calculates the mean difference between the
C       star magnitudes in the files, taking only acceptable stars
C       to do the calculations
C
C -----------------------------------------------------------------
C
C
      SUBROUTINE FILAVS(IPR,KTOT,NITEM,LSTLEN,NUMITS,KIFRMS,DCEN,
     +                  NUMINV,ALRMS,RMSL,AVDIFF,LDIFF,DIFLIM,
     +                  NUM)
C
C
C
      REAL ALRMS(5),RMSL(100,5),AVDIFF(5),GOODMG(1000),RDIFF(1000)
      INTEGER IPR(5),NITEM(5),LDIFF(20,5),LFLAG(1000)
      CHARACTER KIFRMS*1
      REAL AMAG(5),X(5),Y(5),DX(5),DY(5),RMS(5)
      INTEGER ITER(5),NINVAL(5),NUM(5)
      LOGICAL LOOP
      CHARACTER*72 TEXT
C
C  Set first file as reference level and store the number of good stars
C  in it.
C
      NUM(1) = 0
      DO KLIST = 1,LSTLEN
         CALL GETDAT(%VAL(IPR(1)),NITEM(1),LSTLEN,KLIST,X(1),Y(1),
     +               AMAG(1),DX(1),DY(1),ITER(1),RMS(1),NINVAL(1))
         KW = 1
         IF (AMAG(1).GT.49.9.OR.AMAG(1).LT.0.01) KW = 0
         IF(ITER(1).GT.NUMITS)KW = 0
         IF(KIFRMS.EQ.'Y') THEN
            AKM = (ALRMS(1)-AMAG(1))/0.5 + 1.0
            KM = IFIX(AKM)
            AKM = AKM - FLOAT(KM)
            IF(KM.LE.0)KM = 1
            IF(KM.GT.16)KM = 16
            RS = RMSL(KM,1) + AKM*(RMSL(KM+1,1)-RMSL(KM,1))
            IF(RMS(1).GT.RS)KW = 0
         ENDIF
         IF(ABS(DX(1)).GT.DCEN) KW = 0
         IF(ABS(DY(1)).GT.DCEN) KW = 0
         IF(NINVAL(1).GT.NUMINV) KW = 0
         IF (KW.EQ.1) NUM(1) = NUM(1) + 1
      ENDDO
      IF (NUM(1).GT.1000) NUM(1) = 1000
      DO L = 1,20
         LDIFF(L,1) = 0
      ENDDO
      LDIFF(10,1) = NUM(1)
      AVDIFF(1) = 0.0
C
C  Calculate mean levels in the other files
C
      DO K = 2,KTOT
C
C  Go through all the stars, noting good pairs
C
         NGOOD = 0
         DO KLIST = 1,LSTLEN
C
C  See if this is a good pair
C
            CALL GETDAT(%VAL(IPR(K)),NITEM(K),LSTLEN,KLIST,X(K),Y(K),
     +                  AMAG(K),DX(K),DY(K),ITER(K),RMS(K),NINVAL(K))
            CALL GETDAT(%VAL(IPR(1)),NITEM(1),LSTLEN,KLIST,X(1),Y(1),
     +                  AMAG(1),DX(1),DY(1),ITER(1),RMS(1),NINVAL(1))
C
            KW = 1
            DO L = 1,2
               IF (L.EQ.1) LL = 1
               IF (L.EQ.2) LL = K
               IF (AMAG(LL).GT.49.9.OR.AMAG(LL).LT.0.01) KW = 0
               IF(ITER(LL).GT.NUMITS)KW = 0
               IF(KIFRMS.EQ.'Y') THEN
                  AKM = (ALRMS(LL)-AMAG(LL))/0.5 + 1.0
                  KM = IFIX(AKM)
                  AKM = AKM - FLOAT(KM)
                  IF(KM.LE.0)KM = 1
                  IF(KM.GT.16)KM = 16
                  RS = RMSL(KM,LL) + AKM*(RMSL(KM+1,LL)-RMSL(KM,LL))
                  IF(RMS(LL).GT.RS)KW = 0
               ENDIF
               IF(ABS(DX(LL)).GT.DCEN) KW = 0
               IF(ABS(DY(LL)).GT.DCEN) KW = 0
               IF(NINVAL(LL).GT.NUMINV) KW = 0
            ENDDO
c
C  If OK, store that away, noting file 1 mag and magdiff
C
            IF (KW.EQ.1.AND.NGOOD.LT.1000) THEN
               NGOOD = NGOOD + 1
               RDIFF(NGOOD) = AMAG(K) - AMAG(1)
               GOODMG(NGOOD) = AMAG(1)
            ENDIF
         ENDDO
         NUM(K) = NGOOD
C
C  Calculate raw mean
C
         IF (NGOOD.GE.1) THEN
            AM = 0.0
            DO L = 1,NGOOD
               AM = AM + RDIFF(L)
            ENDDO
            AM = AM/REAL(NGOOD)
         ELSE
            AM = 0.0
         ENDIF
C
C  Recalculate mean, by throwing out largest diff and recalculating
C  until none thrown out more than 0.2 diff from mean
C
         IF (NGOOD.GT.1) THEN
            DO L = 1,NGOOD
               LFLAG(L) = 1
            ENDDO
            LOOP = .TRUE.
            DO WHILE (LOOP)
               RMAX = 0.0
               DO L = 1,NGOOD
                  IF (LFLAG(L).EQ.1) THEN
                     ABDIFF = ABS(RDIFF(L)-AM)
                     IF (ABDIFF.GT.RMAX) THEN
                        RMAX = ABDIFF
                        LNOTE = L
                     ENDIF
                  ENDIF
               ENDDO
               IF (RMAX.GT.0.2) THEN
                  LFLAG(LNOTE) = 0
               ELSE
                  LOOP = .FALSE.
               ENDIF
               SUM = 0.0
               LSUM = 0
               DO L = 1,NGOOD
                  IF (LFLAG(L).EQ.1) THEN
                     SUM = SUM + RDIFF(L)
                     LSUM = LSUM + 1
                  ENDIF
               ENDDO
               AM = SUM/REAL(LSUM)
            ENDDO
         ENDIF
C
C  Display on screen and use cursor to refine
C
C
         CALL WRUSER(' ',ISTAT)
         WRITE(TEXT,900)K,AM
  900    FORMAT(' ','MEAN DIFF FROM FILE 1 FOR FILE ',I1,' IS ',F6.2)
         IF (NGOOD.GT.1) THEN
            CALL WRUSER(TEXT,ISTAT)
            CALL WRUSER('DISPAY DIFFS ON DEVICE',ISTAT)
            LOOPB = 0
            DO WHILE (LOOPB.EQ.0)
               CALL DEVOPN(IDEV,SIZE)
               CALL REFINE(IDEV,SIZE,GOODMG,RDIFF,1000,NGOOD,AM,AMA)
               CALL DEVCLS(IDEV)
               KW = 2
               CALL GETCMD('AGAIN','YES,NO.',1,KW,TEXT,KTEXT,IERR)
               CALL CNPAR('AGAIN',ISTAT)
               IF (KW.EQ.2) LOOPB = 1
            ENDDO
            AVDIFF(K) = AMA
         ELSE
            WRITE(TEXT,901)NGOOD
  901       FORMAT(' ','ONLY',I2,' GOOD PAIRS')
            CALL WRUSER(TEXT,ISTAT)
            AVDIFF(K) = AM
         ENDIF
C
C  Store the distribution of good stars around mean
C
         DO L = 1,20
            LDIFF(L,K) = 0
         ENDDO
         IF (NGOOD.GE.1) THEN
            DO L = 1,NGOOD
               KD = (RDIFF(L)-AVDIFF(K))*1000.0
               KD = (KD+525)/50
               IF (KD.LT.1) KD = 1
               IF (KD.GT.20) KD = 20
               LDIFF(KD,K) = LDIFF(KD,K) + 1
            ENDDO
         ENDIF
C
C
C
      ENDDO
C
C
C
      END



C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R WRHEAD *
C      *            *
C      **************
C
C   This subroutine prints out the header information to the Print
C   file.
C
C -----------------------------------------------------------------
C
C
      SUBROUTINE WRHEAD(KUNIT,DCEN,KCEN,KIFRMS,NUMINV,NUMITS,
     +                  KTOT,ALRMS,RMSL,RMSFAC,AVDIFF,LDIFF,
     +                  TEXTI,TEXTO,NUMG,DIFLIM)
C
C
C
      REAL ALRMS(5),RMSL(100,5),AVDIFF(5),BRMSL(100)
      INTEGER LDIFF(20,5),NUMG(5)
      CHARACTER*132 AOUT
      CHARACTER*72 TEXTI(5),TEXTO
      CHARACTER KCEN*1,KIFRMS*1
C
C
C
      WRITE (KUNIT,910)TEXTO
  910 FORMAT(' ','OUTPUT FILE TITLE IS   ',A72)
C

C
C
      WRITE(KUNIT,900)DCEN,KIFRMS,KCEN,NUMINV,NUMITS,DIFLIM
  900 FORMAT(' ','CENTRE DISTANCE ALLOWED = ',F6.1,
     +       /' ','CHECK FOR RMS ERRORS (Y/N)   ',A1,
     +       /' ','ACCEPT IF CENTRE OFF ONLY FAULT (Y/N) ',A1,
     +       /' ','INVALID POINTS ALLOWED = ',I5,
     +       /' ','ITERATIONS ALLOWED = ',I5,
     +       /' ','Max difference from mean of an estimate = ',F6.2,
     +       /' ',' '//)
C
C
C
      DO K = 1,KTOT
         WRITE(KUNIT,908)K,TEXTI(K)
  908    FORMAT(' ','INPUT FILE NO ',I1,' TITLE IS   ',A72)
         IF (KIFRMS.EQ.'Y') THEN
            DO L = 1,60,5
               BRMSL(L) = RMSL(L,K)
               IF (BRMSL(L).GT.9999.0) BRMSL(L) = 9999.0
            ENDDO
            WRITE(KUNIT,901)ALRMS(K),(BRMSL(L),L=1,60,5)
  901       FORMAT(' ','RMS MAG START = ',F6.2/
     +             ' ','RMS limits at 1 mag steps'/
     +             ' ',6F7.1/
     +             ' ',7F7.1)
         ENDIF
         WRITE(KUNIT,911)
  911    FORMAT(' ',' ')
      ENDDO
      WRITE(KUNIT,911)
C
C
C
      DO K = 1,KTOT
         WRITE(KUNIT,902)K,AVDIFF(K)
  902    FORMAT(' ','MEAN DIFF FROM FILE 1 FOR FILE ',I1,' IS ',
     +          F8.2)
         WRITE(KUNIT,912)NUMG(K)
  912    FORMAT(' ','NO OF GOOD CLOSE MATCHES USED FOR DIFF =',
     +          I6)
         WRITE(KUNIT,903)(LDIFF(J,K),J=1,20)
  903    FORMAT(' ','GOOD DIFFS FROM FILE 1 (IN 0.05MAG GROUPS) = '/
     +          ' ',10I5/
     +          ' ',10I5/)
      ENDDO
C
C  Write caption to results
C
      WRITE(KUNIT,904)RMSFAC
  904 FORMAT(' ',///10X,'RMS VALUES HAVE BEEN MULTIPLIED BY ',F10.2/)
C
      DO J = 1,132
         AOUT(J:J) = ' '
      ENDDO
      J = 1
      AOUT(J:J) = 'N'
      AOUT(J+1:J+1) = 'A'
      AOUT(J+2:J+2) = 'M'
      AOUT(J+3:J+3) = 'E'
C
      J = 15
      AOUT(J:J) = 'X'
C
      J = 20
      AOUT(J:J) = 'Y'
C
      J = 24
      AOUT(J:J) = 'M'
      AOUT(J+1:J+1) = 'A'
      AOUT(J+2:J+2) = 'G'
C
      J = 31
      AOUT(J:J) = 'N'
C
      J = 36
      AOUT(J:J) = 'D'
      AOUT(J+1:J+1) = 'I'
      AOUT(J+2:J+2) = 'F'
      AOUT(J+3:J+3) = 'S'
C
      J = 37 + 4*KTOT
      AOUT(J:J) = 'I'
      AOUT(J+1:J+1) = 'T'
      AOUT(J+2:J+2) = 'S'
      J = J + 2 + 3*KTOT
      AOUT(J:J) = 'R'
      AOUT(J+1:J+1) = 'M'
      AOUT(J+2:J+2) = 'S'
      J = J + 2 + 3*KTOT
      AOUT(J:J) = 'D'
      AOUT(J+1:J+1) = 'X'
      J = J + 2 + 3*KTOT
      AOUT(J:J) = 'D'
      AOUT(J+1:J+1) = 'Y'
      J = J + 2 + 3*KTOT
      AOUT(J:J) = 'I'
      AOUT(J+1:J+1) = 'N'
      AOUT(J+2:J+2) = 'V'
      AOUT(J+3:J+3) = 'A'
      AOUT(J+4:J+4) = 'L'
      AOUT(J+5:J+5) = 'I'
      AOUT(J+6:J+6) = 'D'
      WRITE(KUNIT,905)(AOUT(K:K),K=1,J+6)
  905 FORMAT(' ',132A1)
C
C
C
      END



C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          **************
C          *            *
C          * S/R WRLINE *
C          *            *
C          **************
C
C    This puts out a line of results to a line of the Print file
C    and also stores them in the Output file
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE WRLINE(FILIN,NITEM,LSTLEN,FILOUT,KLIST,KUNIT,KTOT,
     +                  KIFRMS,KCEN,AVDIFF,AMAG,KBAD,DX,DY,ITER,RMS,
     +                  RMSFAC,NINVAL,AM,AX,AY,NUM)
C
C
      REAL FILIN(NITEM,LSTLEN),FILOUT(9,LSTLEN),TEMP(1,1)
      REAL AVDIFF(5),AMAG(5),DX(5),DY(5),RMS(5)
      INTEGER ITER(5),NINVAL(5),KBAD(5)
      CHARACTER KCEN*1,KIFRMS*1
      CHARACTER*132 AOUT
      CHARACTER ID*20
      BYTE ASC(20,1)
C
C  Make an ASCII line of the results
C
      DO K = 1,132
         AOUT(K:K) = ' '
      ENDDO
C
      CALL EXTLSA(FILIN,NITEM,LSTLEN,KLIST,KLIST,1,5,
     +            ASC,5,1,1,1)
      DO K = 1,10
         NCHAR = ASC(K,1)
         ID(K:K) = CHAR(NCHAR)
         AOUT(K:K) = ID(K:K)
      ENDDO
C
      IF (ABS(AX).LT.1.0E6) THEN
         KX = AX
      ELSE
         KX = SIGN(1.0E6,AX)
      ENDIF
      LC = 11
      CALL DATOUT(AOUT,LC,KX,4,1)
C
      KY = AY
      LC = LC + 1
      CALL DATOUT(AOUT,LC,KY,4,1)
C
      KAM = AM
      LC = LC + 3
      CALL DATOUT(AOUT,LC,KAM,2,1)
      IF (AM.LT.0.0.AND.AM.GT.-1.0) THEN
         LCA = LC - 1
         AOUT(LCA:LCA) = '-'
      ENDIF
      LC = LC + 1
      AOUT(LC:LC) = '.'
      KAMA = (AM-REAL(KAM))*10.0
      KAMA = ABS(KAMA)
      CALL DATOUT(AOUT,LC,KAMA,1,1)
      KAMB = (AM-REAL(KAM)-(REAL(KAMA)/10.0))*100.0
      KAMB = ABS(KAMB)
      CALL DATOUT(AOUT,LC,KAMB,1,1)
C
      LC = LC + 2
      CALL DATOUT(AOUT,LC,NUM,1,0)
C
      LC = LC + 2
      DO K = 1,KTOT
         IF (KBAD(K).EQ.0.OR.(KBAD(K).EQ.2.AND.KCEN.EQ.'Y')) THEN
            KMAG = IFIX(100.0*(AMAG(K)-AVDIFF(K)-AM))
            IF(KMAG.GT.999)KMAG = 999
            IF(KMAG.LT.-999)KMAG = -999
            CALL DATOUT(AOUT,LC,KMAG,4,1)
         ELSE
           KMAG = 0
           CALL DATOUT(AOUT,LC,KMAG,4,0)
         ENDIF
      ENDDO
C
      LC = LC + 2
      DO K = 1,KTOT
         CALL DATOUT(AOUT,LC,ITER(K),3,1)
      ENDDO
C
      LC = LC + 2
      DO K = 1,KTOT
         KRMS = INT(RMSFAC*RMS(K))
         CALL DATOUT(AOUT,LC,KRMS,3,1)
      ENDDO
C
      LC = LC + 2
      DO K = 1,KTOT
         KDX = INT(DX(K))
         CALL DATOUT(AOUT,LC,KDX,3,1)
      ENDDO
C
      LC = LC + 2
      DO K = 1,KTOT
         KDY = INT(DY(K))
         CALL DATOUT(AOUT,LC,KDY,3,1)
      ENDDO
C
      LC = LC + 2
      DO K = 1,KTOT
         CALL DATOUT(AOUT,LC,NINVAL(K),3,0)
      ENDDO
C
C  Write line to Print file
C
      WRITE(KUNIT,904)(AOUT(J:J),J=1,LC)
  904 FORMAT(' ',132A1)
C
C  Store result in Output file
C
      CALL EXTLSA(FILIN,NITEM,LSTLEN,KLIST,KLIST,1,5,
     +            FILOUT,9,LSTLEN,KLIST,1)
      TEMP(1,1) = AX
      CALL EXTLSA(TEMP,1,1,1,1,1,1,FILOUT,9,LSTLEN,KLIST,6)
      TEMP(1,1) = AY
      CALL EXTLSA(TEMP,1,1,1,1,1,1,FILOUT,9,LSTLEN,KLIST,7)
      TEMP(1,1) = AM
      CALL EXTLSA(TEMP,1,1,1,1,1,1,FILOUT,9,LSTLEN,KLIST,8)
      TEMP(1,1) = NUM
      CALL EXTLSA(TEMP,1,1,1,1,1,1,FILOUT,9,LSTLEN,KLIST,9)
C
C
C
      END



C
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
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         **************
C         *            *
C         * S/R DATOUT *
C         *            *
C         **************
C
C
C
C   This loads an integer into a defined position of an 132A1 line.
C   The position starts at LC and ends at LC+NW-1.
C   On output from the s/r, LC is put equal LC+NW-1, the end position
C   of the integer.
C
C ----------------------------------------------------
C
C
C
      SUBROUTINE DATOUT(A,LC,K,NW,KBLANK)
C
C
C
      CHARACTER*132 A
C
      IF(K.LT.0) THEN
         L = -1*K
         NWA = NW - 1
         LC = LC + 1
      ELSE
         L = K
         NWA = NW
      ENDIF
C
      KW = 1
      IF(NWA.NE.1) KW = 10**(NWA-1)
      IF(L.GE.10**NWA) THEN
         DO J = 1,NWA
            LCA = LC + J
            A(LCA:LCA) = '*'
         ENDDO
         IF(K.LT.0) A(LC+1:LC+1) = '-'
      ELSE
         LBLANK = 0
         DO J = 1,NWA
            LA = L/KW
            IF(LA.EQ.0) THEN
               LCA = LC + J
               A(LCA:LCA) = ' '
               IF(LBLANK.NE.0) A(LCA:LCA) = '0'
            ELSE
               LCA = LC + J
               IF(LBLANK.EQ.0) LBLANK = J
               IF(LA.EQ.1) A(LCA:LCA)= '1'
               IF(LA.EQ.2) A(LCA:LCA)= '2'
               IF(LA.EQ.3) A(LCA:LCA)= '3'
               IF(LA.EQ.4) A(LCA:LCA)= '4'
               IF(LA.EQ.5) A(LCA:LCA)= '5'
               IF(LA.EQ.6) A(LCA:LCA)= '6'
               IF(LA.EQ.7) A(LCA:LCA)= '7'
               IF(LA.EQ.8) A(LCA:LCA)= '8'
               IF(LA.EQ.9) A(LCA:LCA)= '9'
            ENDIF
            L = L - LA*KW
            KW = KW/10
         ENDDO
         LCA = LC + NWA
         IF(K.EQ.0.AND.KBLANK.EQ.1) A(LCA:LCA) = '0'
      ENDIF
C
      IF(K.LT.0) A(LC+LBLANK-1:LC+LBLANK-1) = '-'
      LC = LC + NWA
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R AVSTAR *
C      *            *
C      **************
C
C
C  This s/r calcs the best mean mag and position for a star with up
C  to 5 estimates, using various tests to reject some of the
C  estimates
C
C
C -------------------------------------------------------------
C
C
C
      SUBROUTINE AVSTAR(KTOT,AMAG,X,Y,DX,DY,ITER,RMS,NINVAL,AVDIFF,
     +                  KIFRMS,ALRMS,RMSL,KCEN,DCEN,NUMINV,DIFLIM,
     +                  NUMITS,AX,AY,AM,NUM,KBAD)
C
C
C
      REAL AMAG(5),X(5),Y(5),DX(5),DY(5),AVDIFF(5),ALRMS(5),RMS(5)
      REAL RMSL(100,5)
      INTEGER ITER(5),NINVAL(5)
      CHARACTER*1 KCEN,KIFRMS
      INTEGER KUSE(5),KBAD(5)
      LOGICAL LOOP
C
C  See which files have a good magnitude measure
C
      DO K = 1,KTOT
         KBAD(K) = 0
         IF (AMAG(K).GT.49.9) KBAD(K) = 1
         IF (AMAG(K).LT.0.01) KBAD(K) = 1
         IF (ITER(K).GT.NUMITS) KBAD(K) = 1
         IF (KIFRMS.EQ.'Y') THEN
            AKM = (ALRMS(K)-AMAG(K))/0.2 + 1.0
            KM = IFIX(AKM)
            AKM = AKM - FLOAT(KM)
            IF(KM.LT.1)KM = 1
            IF(KM.GT.99)KM = 99
            RS = RMSL(KM,K) + AKM*(RMSL(KM+1,K)-RMSL(KM,K))
            IF (RMS(K).GT.RS) KBAD(K) = 1
         ENDIF
         IF (NINVAL(K).GT.NUMINV) KBAD(K) = 1
         IF (KBAD(K).EQ.0) THEN
            IF (ABS(DX(K)).GT.DCEN.OR.ABS(DY(K)).GT.DCEN) THEN
               KBAD(K) = 2
            ENDIF
         ENDIF
      ENDDO
C
C  Average the mag and position of the good measures for this star
C
      NUM = 0
      AM = 0.0
      AX = 0.0
      AY = 0.0
      DO K = 1,KTOT
         IF (KBAD(K).EQ.0.OR.(KBAD(K).EQ.2.AND.KCEN.EQ.'Y')) THEN
            KUSE(K) = 1
            AM = AM + AMAG(K) - AVDIFF(K)
            AX = AX + X(K)
            AY = AY + Y(K)
            NUM = NUM + 1
         ELSE
            KUSE(K) = 0
         ENDIF
      ENDDO
      IF (NUM.NE.0) THEN
         AM = AM/REAL(NUM)
         AX = AX/REAL(NUM)
         AY = AY/REAL(NUM)
      ELSE
         AM = 0.0
         AX = X(1)
         AY = Y(1)
      ENDIF
C
C  Throw out mags too far from mean
C
      IF (NUM.NE.0) THEN
         LOOP = .TRUE.
         DO WHILE (LOOP)
            DIFMAX = DIFLIM
            NUMDIF = 0
            DO K = 1,KTOT
               IF (KUSE(K).EQ.1) THEN
                  ADIFF = ABS(AM-(AMAG(K)-AVDIFF(K)))
                  IF (ADIFF.GT.DIFMAX) THEN
                     DIFMAX = ADIFF
                     NUMDIF = K
                  ENDIF
               ENDIF
            ENDDO
            IF (NUMDIF.EQ.0) THEN
               LOOP = .FALSE.
            ELSE
               IF (NUM.EQ.2) THEN
                  NUM = 0
                  LOOP = .FALSE.
               ELSE
                  NUM = NUM - 1
                  AM = AM*REAL(NUM+1)-(AMAG(NUMDIF)-AVDIFF(NUMDIF))
                  AM = AM/REAL(NUM)
                  AX = (AX*REAL(NUM+1)-X(NUMDIF))/REAL(NUM)
                  AY = (AY*REAL(NUM+1)-Y(NUMDIF))/REAL(NUM)
                  KUSE(NUMDIF) = 0
               ENDIF
            ENDIF
         ENDDO
      ENDIF
C
C
C
C
      END





C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R REFINE *
C      *            *
C      **************
C
C
C
C -------------------------------------------------------------
C
C
C
       SUBROUTINE REFINE(IDEV,SIZE,X,Y,LNUM,NUM,AM,AMA)
C
C
C
      CHARACTER*1 DOTS
      CHARACTER*72 TEXT
      REAL X(LNUM),Y(LNUM),AX(2),AY(2),SIZE(2)
      CHARACTER*4 TEXTB(2)
      INTEGER TI(2)
      EQUIVALENCE (TEXTB(1),TI(1))
      EQUIVALENCE (TEXTB(2),TI(2))
C
C  Get ranges of values
C
      XMIN = X(1)
      XMAX = X(1)
      YMIN = Y(1) - AM
      YMAX = YMIN
      DO L = 1,NUM
         IF(X(L).GT.XMAX) XMAX = X(L)
         IF(X(L).LT.XMIN) XMIN = X(L)
         IF((Y(L)-AM).GT.YMAX) YMAX = (Y(L)-AM)
         IF((Y(L)-AM).LT.YMIN) YMIN = (Y(L)-AM)
      ENDDO
      CALL WRUSER('ENTER MAG PLOT LIMITS',ISTAT)
      AX(1) = REAL(INT(XMIN*2.0))/2.0 - 0.5
      AX(2) = REAL(INT(XMAX*2.0))/2.0 + 1.0
      CALL CNPAR('DEVLIM',ISTAT)
      CALL RDKEYR('DEVLIM',.TRUE.,2,AX,I,ISTAT)
      CALL WRUSER('ENTER DIFF PLOT LIMITS',ISTAT)
      CALL CNPAR('DEVLIM',ISTAT)
      AY(1) = REAL(INT(YMIN*20.0))/20.0 - 0.05
      AY(2) = REAL(INT(YMAX*20.0))/20.0 + 0.10
      CALL RDKEYR('DEVLIM',.TRUE.,2,AY,I,ISTAT)
      DOTS = 'N'
      CALL CNPAR('DOTNUM',ISTAT)
      CALL RDKEYC('DOTNUM',.TRUE.,1,DOTS,I,ISTAT)
      XL = SIZE(1)
      YL = SIZE(2)
C
C  Plot residuals from mean
C
      CALL NEWPLT(AX(1),AX(2),XL,AY(1),AY(2),YL)
      TEXTB(1) = 'DIFF'
      TEXTB(2) = 'MAG'
      CALL DRAW AX(TI(1),4,AX(1),90.0)
      CALL DRAW AX(TI(2),4,AY(1),0.0)
      DO L = 1,NUM
         XX = X(L)
         YY = Y(L) - AM
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
C  Draw line at zero residual from mean
C
      CALL BREAK
      CALL JOIN PT(AX(1),0.0)
      CALL JOIN PT(AX(2),0.0)
C
C  Use cursor to  get a better mean
C
      IF (IDEV.EQ.2.OR.IDEV.EQ.3.OR.IDEV.EQ.4) THEN
         CALL WRUSER('USE CURSOR TO REFINE MEAN',ISTAT)
         CALL CURSOR(XA,YA)
         AMA = AM + YA
         WRITE(TEXT,904)AMA
  904    FORMAT(' ','NEW MEAN = ',F6.2)
         CALL WRUSER(TEXT,ISTAT)
      ELSE
         AMA = AM
      ENDIF
C
C
C
      END



