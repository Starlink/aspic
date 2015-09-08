      PROGRAM PLOTPS
C+
C
C   Program PLOTPS
C
C   This program plots a power spectrum and, optionally, a window
C   function.
C
C   This is a new program incorporating some new features for
C   plotting data associated with the period-finding programs
C   In particular it will plot on all GKS devices,
C   and plot a window function as well.
C
C   It uses the interim Starlink environment and SIMPLEPLOT
C   on top of GKS.
C
C   Written by K.F.Hartley at RGO
C
C   Version 1 - 12-Jan-1984
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),AX2(2),STATUS
      CHARACTER*70 TEXF,TEXS
      REAL LIMX(2),LIMY(2),RANGE(2)
      REAL LIMXT(2),LIMYT(2)
      LOGICAL CUR,FRE
C
C   This monstrous thing is to get round the fact that Simpleplot
C   works in Hollerith strings!
C
      CHARACTER*72 TEXT
      INTEGER ITEXT(18)
      EQUIVALENCE (TEXT,ITEXT)
C
C   First obtain the power spectrum
C
      CALL RDIMAG('POWER',FMT_DP,2,AX,I,IP1,STATUS)
      CALL PER_STOP(STATUS,'BADDATA')
C
C   There are no markers in power spectra, so flag them as such
C
      IFIRST=AX(2)+1
      ISECND=AX(2)+1
C
C   Get two 1-D arrays for X and Y co-ordinates
C
      CALL GETDYN('WORK1',FMT_R,AX(2),IPW1,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
      CALL GETDYN('WORK2',FMT_R,AX(2),IPW2,STSTUS)
      CALL PER_STOP(STATUS,'ERRWRK')
C
C   Fill them with data and find the min and max
C
   10 CONTINUE
      CALL PER_FILLMM(%VAL(IP1),%VAL(IPW1),%VAL(IPW2),AX,LIMX,LIMY)
      LIMXT(1)=LIMX(1)
      LIMXT(2)=LIMX(2)
C
C   Now obtain plotting limits in X and Y
C
      FRE=.TRUE.
      CALL RDKEYL('FREQ',.TRUE.,1,FRE,I,STATUS)
      IF (.NOT.FRE) THEN
         CALL WRUSER('This program has to work in FREQUENCY',STATUS)
         IF (LIMX(1).EQ.0.0) THEN
             CALL WRUSER('First PERIOD is infinite',STATUS)
         ELSE
             TEMP=1.0/LIMX(1)
             WRITE (TEXT,900) TEMP
  900        FORMAT (1H ,'First PERIOD is ',F10.6)
             CALL WRUSER(TEXT,STATUS)
         END IF
         IF (LIMX(2).EQ.0.0) THEN
            CALL WRUSER('Last PERIOD is infinite',STATUS)
         ELSE
            TEMP=1.0/LIMX(2)
            WRITE (TEXT,910) TEMP
  910       FORMAT (1H ,'Last PERIOD is ',F10.6)
            CALL WRUSER(TEXT,STATUS)
         END IF
      END IF
      CALL WRUSER('Enter 2 identical frequencies to exit (eg 0,0)',
     :            ISTAT)
      CALL RDKEYR('FREQLIMS',.TRUE.,2,LIMX,I,STATUS)
      IF (LIMX(1).EQ.LIMX(2)) GO TO 899
      CALL RDKEYR('YLIMITS',.TRUE.,2,LIMY,I,STATUS)
C
C   Power spectra are always plotted as line plots
C
      MARK=.FALSE.
C
C   Now find out if there is to be a window function.
C
      CALL RDIMAG('WINDOW',FMT_DP,2,AX2,I,IP2,STATUS)
C
C   For sensible display the window function should have been
C   computed at the same frequencies as the power spectrum.
C   So at least check that the same number of frequencies are present
C
      IF (STATUS.EQ.ERR_NORMAL.AND.AX2(2).NE.AX(2)) THEN
         CALL WRERR('HELLDIM',STATUS)
         STATUS=ERR_NORMAL+1
      END IF
C
C   If a good response was found (excluding a null response)
C   make room for the second plot and the differences
C
      IF (STATUS.EQ.ERR_NORMAL) THEN
         DELTAY=LIMY(2)-LIMY(1)
         LIMY(1)=LIMY(1)-DELTAY
      ELSE
         DELTAY=0.0
      END IF
C
C   Add a little to the top Y limit to allow room for a title
C
      LIMY(2)=LIMY(2)*1.15
C
C   Now get the plotting device, open it and draw some axes
C
      CALL PER_AXESF(LIMX,LIMY)
C
C   Now plot the first dataset.
C   OFF defines the offset for this plot, RANGE defines lines to
C   mark any gap.
C
      OFF=0.0
      RANGE(1)=LIMY(1)+DELTAY
      RANGE(2)=LIMY(2)
      CALL PER_PLOTL(%VAL(IPW1),%VAL(IPW2),AX(2),IFIRST,ISECND,
     :              MARK,OFF,RANGE)
C
C   Now allow the user to define some peaks
C   if the selected device has a cursor
C
      CALL JBINQ(XT,YT,CUR)
      IF (CUR) THEN
         CALL PER_FITPAR(%VAL(IPW1),%VAL(IPW2),AX(2),LIMX,NCEN)
      ELSE
C
C      If a window function is to be plotted, ask where to put it.
C
         IF (DELTAY.NE.0.0) THEN
            CALL WRUSER('Specifiy location of peak of the window'//
     :                  ' function',STATUS)
            F=(LIMXT(1)+LIMXT(2))/2.0
            CALL RDKEYR('FREQCEN',.TRUE.,1,F,I,STATUS)
            NCEN = (F-LIMXT(1))*REAL(AX(2))/(LIMXT(2)-LIMXT(1)) + 1
         ELSE
            NCEN = AX(2)/2
         END IF
      END IF
C
C   Now handle a possible second dataset
C
      IF (DELTAY.NE.0.0) THEN
         CALL CNPAR('WORK1',ISTAT)
         CALL CNPAR('WORK2',ISTAT)
         CALL FRDATA('WORK1',ISTAT)
         CALL FRDATA('WORK2',ISTAT)
         CALL GETDYN('WORK1',FMT_R,AX2(2),IPW1,STATUS)
         CALL GETDYN('WORK2',FMT_R,AX2(2),IPW2,STATUS)
         CALL PER_WINMM(%VAL(IP2),%VAL(IPW1),%VAL(IPW2),AX2,
     :                  LIMX,LIMY,NCEN)
         OFF=DELTAY
         RANGE(1)=LIMY(1)
         RANGE(2)=LIMY(1)+DELTAY
         CALL PER_PLOTL(%VAL(IPW1),%VAL(IPW2),AX2(2),IFIRST,ISECND,
     :                 MARK,OFF,RANGE)
      END IF
C
C   Add a title
C
      TEXT=' '
      CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
      IF (TEXT.NE.' ') THEN
C
C      Find out how long it is
C
         DO I=70,1,-1
            IF (TEXT(I:I).NE.' ') THEN
               NCHAR=I
               GO TO 100
            END IF
         END DO
  100    CONTINUE
         CALL TITLE(1,2,ITEXT,NCHAR)
      END IF
C
C   Allow the user to go back for another plot.
C
      CALL CNPAR('FREQLIMS',STATUS)
      CALL CNPAR('YLIMITS',STATUS)
      GO TO 10
C
C   Finally close the plotter and tidy up.
C
  899 CONTINUE
      CALL ENDPLT
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
