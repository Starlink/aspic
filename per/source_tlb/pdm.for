      PROGRAM PDM
C+
C   Program PDM
C
C   (This is only a working name)
C   (It is derived from Phase Discrimination Methods (PELT))
C
C   This program offers an alternative to looking at power spectra
C   of data to find periodic phenomena. Basically it computes light
C   curves at a number of periods in a defined range
C   and asseses which is the
C   smoothest, offering a choice of criteria. After possibly repeating
C   the operation several times for different rangs of periods it allows
C   the user to estimate the best period.
C
C   The algorithms in use are derived from various sources, but
C   were built into the earlier programs STRING and NEWST.
C   The data are scaled and the output presented in terms of the
C   discussion by Dworetsky in MNRAS 203,p917,1983
C
C   Written by K.F.Hartley at RGO on 14 Feb 1984
C
C-
      DOUBLE PRECISION FREQ(2),TEMP,F,RANEP,ERRVAL
      REAL LIMF(2),SIZE(2)
      INTEGER AX(2),PIN,PX,PY,PPH,PSTAT,PF,PWRK,PIND,PINDW,STATUS
      LOGICAL CUR,FRE,GRAPH
      CHARACTER*8 DEV
      CHARACTER*72 TEXT
      CHARACTER*1 MODE
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   First obtain the input data.
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      CALL PER_STOP(STATUS,'ERRIN')
C
C   Decide on mode of operation
C
      MODE='S'
      CALL WRUSER('Choose mode of operation',STATUS)
      CALL WRUSER('S means "Shortest piece of string round all points"'
     :             ,STATUS)
      CALL WRUSER('L means "Longest mean curve through data"',STATUS)
      CALL RDKEYC('MODE',.TRUE.,1,MODE,I,STATUS)
      CALL STR$UPCASE(MODE,MODE)
C
C   If the choice of mode is S, then an estimate of the
C   error in the data values is useful
C
      IF (MODE(1:1).EQ.'S') THEN
         CALL WRUSER('Enter the error in the observations',
     :                STATUS)
         CALL WRUSER('A null return or 0 is acceptable',STATUS)
         CALL RDKEYD('ERROR',.FALSE.,1,ERRVAL,I,STATUS)
         IF (STATUS.NE.ERR_NORMAL) ERRVAL = 0.0
      ELSE
         ERRVAL=0.0
      END IF
C
C   Then obtain a plotting device and open it.
C   If it is an invalid device then no plotting is done.
C
      DEV='ARGS'
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
      CALL PER_SPLITP(%VAL(PIN),%VAL(PX),%VAL(PY),AX,RANEP,ERRVAL)
C
C   Obtain range of frequencies
C
C   It returns to here after each loop
C
      FRE=.TRUE.
      CALL RDKEYL('FREQ',.TRUE.,1,FRE,I,STATUS)
  100 CONTINUE
      IF (FRE) THEN
         CALL WRUSER('Enter range of FREQUENCIES',STATUS)
      ELSE
         CALL WRUSER('Enter range of PERIODS',STATUS)
      END IF
      CALL RDKEYD('RANGE',.FALSE.,2,FREQ,IFREQ,STATUS)
      IF (STATUS.EQ.ERR_NORMAL.AND.IFREQ.EQ.2) THEN
C
C   Carry on and do something useful
C
C   First sort out frequencies if periods were input
C
         IF (.NOT.FRE) THEN
            TEMP=FREQ(1)
            FREQ(1)=1.0D0/FREQ(2)
            FREQ(2)=1.0D0/TEMP
         END IF
C
C      and put them into the right order
C
         IF (FREQ(1).GT.FREQ(2)) THEN
            TEMP=FREQ(1)
            FREQ(1)=FREQ(2)
            FREQ(2)=TEMP
         END IF
C
C      Ask for number of frequencies
C      Default value is such that phase errors are always less
C      than 0.1 (see Dworetsky op.cit. p921)
C
         NUMF = INT((FREQ(2)-FREQ(1))*RANEP/0.1)
         CALL WRUSER('The number of frequencies should be AT LEAST'//
     :                ' the default value',STATUS)
         CALL RDKEYI('NUMFREQ',.TRUE.,1,NUMF,I,STATUS)
C
C      Obtain space to store NUMF statistics
C
         CALL GETDYN('STAT',FMT_R,NUMF,PSTAT,STATUS)
         CALL PER_STOP(STATUS,'ERRDYN')
C
C      and for workspace for the sorting, if required.
C
         IF (MODE.EQ.'S') THEN
            CALL GETDYN('WORK',FMT_DP,AX(2),PWRK,STATUS)
            CALL PER_STOP(STATUS,'ERRDYN')
            CALL GETDYN('INDEX',FMT_SL,AX(2),PIND,STATUS)
            CALL PER_STOP(STATUS,'ERRDYN')
            CALL GETDYN('INDEXW',FMT_SL,AX(2),PINDW,STATUS)
            CALL PER_STOP(STATUS,'ERRDYN')
         END IF
C
C      Now start to loop through the frequencies.
C
         DO I=1,NUMF
            F = FREQ(1) + DBLE(I-1)*(FREQ(2)-FREQ(1))/DBLE(NUMF-1)
C
C         Compute the phases for frequency F
C
            CALL PER_PHASES(%VAL(PX),%VAL(PPH),AX(2),F)
C
C         and sort them into increasing order, if required,
C         and compute the statistic for this frequency
C
            IF (MODE.EQ.'S') THEN
               CALL PER_SORT(%VAL(PPH),%VAL(PY),%VAL(PWRK),%VAL(PIND),
     :                    %VAL(PINDW),%VAL(PX),AX(2))
               CALL PER_SHORT(%VAL(PPH),%VAL(PY),AX(2),%VAL(PSTAT),
     :                      NUMF,I)
            ELSE
               CALL PER_LONG(%VAL(PPH),%VAL(PY),AX(2),%VAL(PSTAT),
     :                       NUMF,I)
            END IF
C
C         Encourage the user
C
            IF (MOD(I,10).EQ.0) THEN
               ILEFT=NUMF-I
               WRITE (TEXT,900) ILEFT
  900          FORMAT(1H ,I5,' frequencies remaining')
               CALL WRUSER(TEXT,STATUS)
            END IF
C
C         That completes the loop through the frequencies.
C
         END DO
C
C      Now plot the results
C
         IF (GRAPH) THEN
            CALL PER_PLOTRES(%VAL(PSTAT),NUMF,FREQ,XL,YL,ERRVAL,
     :                      AX(2))
C
C         Obtain workspace for the frequencies
C         if a cursor is available
C
            IF (CUR) THEN
                  CALL GETDYN('F',FMT_R,NUMF,PF,STATUS)
                  CALL PER_STOP(STATUS,'ERRDYN')
C
C               and set up the array of frequencies
C
                  CALL PER_FREQ(%VAL(PF),NUMF,FREQ)
C
C               then allow the user to find an estimate of the frequency
C
                  LIMF(1)=FREQ(1)
                  LIMF(2)=FREQ(2)
                  CALL PER_FITPAR(%VAL(PF),%VAL(PSTAT),NUMF,LIMF,NCEN)
                  CALL CNPAR('F',STATUS)
                  CALL FRDATA('F',STATUS)
            ELSE
C
C            If no cursor simply find the peak value and write it out
C
               CALL PER_PEAK(%VAL(PSTAT),NUMF,FREQ)
            END IF
         ELSE
C
C         If no graphics simply find the peak and write it out
C
            CALL PER_PEAK(%VAL(PSTAT),NUMF,FREQ)
         END IF
C
C      Now tidy up and return for more
C
         CALL CNPAR('RANGE',STATUS)
         CALL CNPAR('NUMFREQ',STATUS)
         CALL CNPAR('STAT',STATUS)
         CALL FRDATA('STAT',STATUS)
         IF (MODE.EQ.'S') THEN
            CALL CNPAR('INDEX',STATUS)
            CALL CNPAR('WORK',STATUS)
            CALL CNPAR('INDEXW',STATUS)
            CALL FRDATA('INDEX',STATUS)
            CALL FRDATA('WORK',STATUS)
            CALL FRDATA('INDEXW',STATUS)
         END IF
         GO TO 100
      END IF
C
C   Finally end it
C
      CALL ENDPLT
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
