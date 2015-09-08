      PROGRAM FOLD
C+
C   Program FOLD
C
C   This program folds a given dataset at a given frequency (or period)
C   sorts the resulting phases into order and plots them.
C   It then fits a smooth curve through them and draws that.
C   The user is then given the choice of whether he wishes to subtract
C   the smooth curve or not and whether to store it.
C   In either case he is then asked for a new frequency
C   to repeat the operation on the new or original data.
C
C   If subtraction has taken place the result is stored in
C   a new dataset.
C
C   This version works by computing the median value at a series
C   of overlapping phase bins (the number of bins and their width
C   can be specified by the user).
C
C
C
C   Written by K.F.Hartley at RGO on 22 Feb 1984
C
C-
      DOUBLE PRECISION FREQ,TEMP,F,RANEP,ERRVAL
      DOUBLE PRECISION EP1,EP2,ZEROEP
      REAL LIMF(2),SIZE(2)
      DOUBLE PRECISION PHAVE(1000),YAVE(1000),NAVE(1000)
      INTEGER AX(2),PIN,PX,PY,PPH,PSTAT,PF,PWRK,PIND,PINDW,STATUS
      INTEGER MDIM(2)
      LOGICAL CUR,FRE,GRAPH,YES
      CHARACTER*8 DEV
      CHARACTER*72 TEXT
C
C   This code is needed because Simpleplot uses Hollerith strings.
C
      INTEGER ITXT(18)
      EQUIVALENCE (TEXT,ITXT)
      CHARACTER*1 MODE
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   First obtain the input data.
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      CALL PER_STOP(STATUS,'ERRIN')
C
C   and the first and last epochs
C
      CALL PER_GETLIM(%VAL(PIN),AX,EP1,EP2)
C
C   Then obtain a plotting device and open it.
C   If it is an invalid device then no plotting is done.
C
      DEV='ARGS'
      CALL WRUSER('Enter NO if graphics are not wanted',STATUS)
      CALL RDKEYC('DEVICE',.TRUE.,1,DEV,I,STATUS)
      CALL DEVTRAN(DEV,IDEV,ICONID,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL WRUSER('No graphics will be drawn',STATUS)
         GRAPH=.FALSE.
      ELSE
         GRAPH=.TRUE.
      END IF
      IF (GRAPH) THEN
         CALL JBDEV(DEV)
C
C      This is device dependent code to compensate for a bug
C      in the driver for the HP 2648 driver.
C
         IF (IDEV.EQ.12) PRINT *,'I'
         CALL JBINQ(XL,YL,CUR)
C
C   If the selected device has no cursor it is probably a plotter
C   so we need to pick a size for the output
C   The defualt is A4 paper!
C
         IF (.NOT.CUR) THEN
            SIZE(1)=25.0
            SIZE(2)=16.0
            CALL WRUSER('Enter size of plot in cms.',STATUS)
            CALL WRUSER('The default is A4 paper',STATUS)
            CALL RDKEYR('SIZE',.TRUE.,2,SIZE,I,STATUS)
            XL=SIZE(1)
            YL=SIZE(2)
            IF (YL.EQ.0.0) YL=XL
            CALL CNPAR('SIZE',STATUS)
         END IF
      END IF
C
C   Obtain workspace for the epochs and values and phases
C
      CALL GETDYN('X',FMT_DP,AX(2),PX,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('Y',FMT_DP,AX(2),PY,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('PHASE',FMT_DP,AX(2),PPH,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C   Now store the epochs and values
C
      CALL PER_SEPAR(%VAL(PIN),%VAL(PX),%VAL(PY),AX)
C
C   Set up defaults for the number of bins and their width.
C   These are independent parameters so the bins may well
C   overlap. The defaults mean that each bin overlaps half
C   of the preceding one. (Remember that the bins will cover
C   phases from -0.5 to +1.5
C
      NB=40
      BINWIDTH=0.1
C
C   Obtain the frequency to be used
C
C   It returns to here after each loop
C
      FRE=.TRUE.
      CALL RDKEYL('FREQ',.TRUE.,1,FRE,I,STATUS)
  100 CONTINUE
      IF (FRE) THEN
         CALL WRUSER('Enter the FREQUENCY',STATUS)
      ELSE
         CALL WRUSER('Enter the PERIOD',STATUS)
      END IF
      CALL WRUSER('Hit <CR> to exit',STATUS)
      CALL RDKEYD('VALUE',.FALSE.,1,FREQ,IFREQ,STATUS)
      IF (STATUS.EQ.ERR_NORMAL.AND.IFREQ.EQ.1) THEN
C
C   Carry on and do something useful
C
C   First convert period to frequency
C
         IF (.NOT.FRE) FREQ=1.0/FREQ
C
C      and obtain workspace for the sorting.
C
         CALL GETDYN('WORK',FMT_DP,AX(2),PWRK,STATUS)
         CALL PER_STOP(STATUS,'ERRDYN')
         CALL GETDYN('INDEX',FMT_SL,AX(2),PIND,STATUS)
         CALL PER_STOP(STATUS,'ERRDYN')
         CALL GETDYN('INDEXW',FMT_SL,AX(2),PINDW,STATUS)
         CALL PER_STOP(STATUS,'ERRDYN')
C
C      Obtain the epoch corresponding to zero phase
C
         ZEROEP=EP1
         CALL WRUSER('Enter EPOCH for zero phase',STATUS)
         CALL RDKEYD('ZEPOCH',.TRUE.,1,ZEROEP,I,STATUS)
C
C         Compute the phases for this frequency
C
         CALL PER_PHASEZ(%VAL(PX),%VAL(PPH),AX(2),FREQ,ZEROEP)
C
C         and sort them into increasing order.
C
         CALL PER_SORT(%VAL(PPH),%VAL(PY),%VAL(PWRK),%VAL(PIND),
     :              %VAL(PINDW),%VAL(PX),AX(2))
C
C      Now plot the results
C
         IF (GRAPH) THEN
            CALL PER_PPHASE(%VAL(PPH),%VAL(PY),AX(2),XL,YL)
         END IF
C
C      Now fit a smooth curve through the data
C
  200    CONTINUE
C
C      Choose parameters for this attempt to fit a smooth curve.
C
         NBOLD=NB
         BWOLD=BINWIDTH
         CALL WRUSER('Enter total number of bins (covering 2 cycles)',
     :               STATUS)
         CALL WRUSER('Zero to exit',STATUS)
         CALL RDKEYI('NUMBIN',.TRUE.,1,NB,I,STATUS)
         IF (NB.GT.0.AND.NB.LE.1000) THEN
  210       CONTINUE
            BINWIDTH=BWOLD
            CALL WRUSER('Enter width of each bin (They may overlap)',
     :                  STATUS)
            CALL RDKEYR('BINWIDTH',.TRUE.,1,BINWIDTH,I,STATUS)
            BINW=BINWIDTH
            BINSTEP=2.0/REAL(NB)
C
C         Obtain some more workspace to store phases from -0.5 to
C         1.5 which makes it a lot easier to handle the ends of the
C         curve.
C
            NAX2=AX(2)*2
            CALL GETDYN('SPLWRKP',FMT_DP,NAX2,IPSWP,STATUS)
            CALL PER_STOP(STATUS,ERRDYN)
            CALL GETDYN('SPLWRKY',FMT_DP,NAX2,IPSWY,STATUS)
            CALL PER_STOP(STATUS,ERRDYN)
C
C         This is the call which fits the curve and plots it,
C         if required.
C
            CALL PER_MED(%VAL(PPH),%VAL(PY),AX(2),%VAL(IPSWP),
     :                    %VAL(IPSWY),NAX2,BINW,BINSTEP,PHAVE,YAVE,NAVE,
     :                    GRAPH,NPTS)
            CALL FRDATA('SPLWRKP',STATUS)
            CALL FRDATA('SPLWRKY',STATUS)
C
C         Offer the option of trying different parameters.
C
            CALL CNPAR('NUMBIN',STATUS)
            CALL CNPAR('BINWIDTH',STATUS)
            GO TO 200
         ELSE
            NB=NBOLD
            BINWIDTH=BWOLD
         END IF
C
C   Allow a title to be added to hardcopy plots.
C
         IF (GRAPH.AND..NOT.CUR) THEN
            TEXT=' '
            CALL RDKEYC('PLOTITLE',.TRUE.,1,TEXT,I,STATUS)
            IF (TEXT.NE.' ') THEN
               DO I=72,1,-1
                  IF (TEXT(I:I).NE.' ') THEN
                     NCHAR=I
                     GO TO 250
                  END IF
               END DO
  250          CONTINUE
               CALL TITLE(1,2,ITXT,NCHAR)
               CALL CNPAR('PLOTITLE',STATUS)
            END IF
         END IF
  300    CONTINUE
C
C         Now ask if it is to be subtracted
C
         YES=.TRUE.
         CALL WRUSER('Is it to be removed?',STATUS)
         CALL RDKEYL('REMOVE',.TRUE.,1,YES,I,STATUS)
         IF (YES) THEN
C
C             If required, the mean curve is removed.
C
            CALL PER_SPLREM(%VAL(PPH),%VAL(PY),AX(2),PHAVE,YAVE,NPTS)
C
C            The user then has the chance to store the results.
C
            CALL WRIMAG('OUTPUT',FMT_DP,AX,2,POUT,STATUS)
            IF (STATUS.EQ.ERR_NORMAL) THEN
C
C               If it obtains an output dataset then the data must be
C               un-sorted into order of increasing epoch
C
               CALL PER_SORT(%VAL(PX),%VAL(PY),%VAL(PWRK),
     :                 %VAL(PIND),%VAL(PINDW),%VAL(PPH),AX(2))
C
C               and stored in a 2-D array
C
               CALL PER_OUT(%VAL(PX),%VAL(PY),%VAL(POUT),AX)
               CALL CNPAR('OUTPUT',STATUS)
               CALL FRDATA('OUTPUT',STATUS)
            END IF
         END IF
C
C      Now tidy up and return to try again -
C      either with the original data or with the result of
C      subtracting the mean curve.
C
         CALL CNPAR('VALUE',STATUS)
         CALL CNPAR('NUMBIN',STATUS)
         CALL CNPAR('BINWIDTH',STATUS)
         CALL CNPAR('REMOVE',STATUS)
         CALL CNPAR('ZEPOCH',STATUS)
         CALL CNPAR('INDEX',STATUS)
         CALL CNPAR('WORK',STATUS)
         CALL CNPAR('INDEXW',STATUS)
         CALL FRDATA('INDEX',STATUS)
         CALL FRDATA('WORK',STATUS)
         CALL FRDATA('INDEXW',STATUS)
         GO TO 100
      END IF
C
C   The final option is to store a representation of the
C   final mean curve fitted - this might be used as a template.
C
      CALL WRUSER('Do you wish to store the mean curve?',STATUS)
      YES=.FALSE.
      CALL RDKEYL('STORE',.TRUE.,1,YES,I,STATUS)
      IF (YES) THEN
         NV=100
         CALL RDKEYI('NUMVALS',.TRUE.,1,NV,I,STATUS)
         MDIM(1)=3
         MDIM(2)=NV
         CALL WRIMAG('NAME',FMT_DP,MDIM,2,IPM,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            CALL PER_MEDSTR(%VAL(IPM),MDIM,PHAVE,YAVE,NPTS)
         END IF
      END IF
C
C   Finally end it
C
      CALL ENDPLT
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
