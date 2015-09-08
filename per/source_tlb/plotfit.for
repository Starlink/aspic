      PROGRAM PLOTFIT
C+
C
C   Program PLOTFIT
C
C   Allows a synthetic dataset, as defined by a set of parameters,
C   to be plotted on top of an observed dataset. The parameters need
C   not have been derived from this set of observations.
C
C   The data may be plotted against EPOCH or PHASE.
C
C   Written by K.F.Hartley at RGO on 1-Mar-84
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER PIN,PP,PX,PY,PFX,PFY,PWORK,STATUS
      DOUBLE PRECISION EP(2),YR(2),FREQ
      DOUBLE PRECISION XLIM(2),YLIM(2),TEMP,TZERO
      REAL SIZE(2),XPR(2),YPR(2)
      DOUBLE PRECISION FI(20),CI(21),PI(20)
      INTEGER AX(2),AXP(2)
      LOGICAL CUR,YES,EPO,FRE,IN,MARK,MARKF
      CHARACTER*8 DEV
      CHARACTER*72 TEXT
C
C   This code is needed to handle a title for Simpleplot
C
      CHARACTER*72 GRTEXT
      INTEGER IGRT(18)
      EQUIVALENCE (GRTEXT,IGRT)
C
C   First obtain the input dataset
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      CALL PER_STOP(STATUS,'HELLIN')
C
C   and some workspace
C
      NVALS=AX(2)*3
      CALL GETDYN('WORK',FMT_DP,NVALS,PWORK,STATUS)
      CALL PER_STOP(STATUS,'HELLWRK')
C
C   Copy data to workspace
C
      CALL PER_COPY(%VAL(PIN),%VAL(PWORK),AX(1),AX(2),EP)
C
C   Now obtain space for two 1-D arrays for plotting
C
      CALL GETDYN('X',FMT_R,AX(2),PX,STATUS)
      CALL PER_STOP(STATUS,'HELLWRK')
      CALL GETDYN('Y',FMT_R,AX(2),PY,STATUS)
      CALL PER_STOP(STATUS,'HELLWRK')
C
C   Pick up the parameter set describing the fitted data
C
      CALL RDIMAG('PARIN',FMT_DP,2,AXP,I,PP,STATUS)
      CALL PER_STOP(STATUS,'ERRPAR')
      CALL PER_PUTFRE3(%VAL(PP),AXP,FI,CI,PI)
C
C   Now obtain the mean value...
C
      CALL RDDSCR('PARIN','MEAN',1,TEXT,I,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
         CALL CTOR(TEXT,TEMP,STATUS)
         CI(1) = TEMP
      ELSE
         CI(1) = 0.0
      END IF
C
C   ...and the zero epoch for the phases
C
      CALL RDDSCR('PARIN','EPOCH',1,TEXT,I,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
         CALL CTOR(TEXT,TEMP,STATUS)
         TZERO = TEMP
      ELSE
         TZERO=EP(1)
      END IF
C
C   Now handle device and opening SIMPLEPLOT
C
      DEV='ARGS'
   50 CONTINUE
      CALL RDKEYC('DEVICE',.TRUE.,1,DEV,I,STATUS)
      CALL STR$UPCASE(DEV,DEV)
      CALL DEVTRAN(DEV,IDEV,ICONID,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL WRUSER('Failed to recognize that device',STATUS)
         CALL PER_DEVHLP
         CALL CNPAR('DEVICE',STATUS)
         GO TO 50
      END IF
      CALL JBDEV(DEV)
C
C   *** Device specific code to handle bug in HP driver ***
C
      IF (IDEV.EQ.12) PRINT *,'I'
      CALL JBINQ(XL,YL,CUR)
C
C   If there is no cursor it is a plotter and so
C   we ought to define an exact size for the output
C
      IF (.NOT.CUR) THEN
         SIZE(1)=25.0
         SIZE(2)=16.0
         CALL RDKEYR('SIZE',.TRUE.,2,SIZE,I,STATUS)
         XL=SIZE(1)
         IF (I.GT.1) THEN
            YL=SIZE(2)
         ELSE
            YL=SIZE(1)
         END IF
      END IF
C
C   Now decide whether the data are to be plotted as points or lines
C
      MARK=.TRUE.
      CALL WRUSER('Are the DATA to be plotted as POINTS (Y) or ' //
     :            'LINES (N)?',STATUS)
      CALL WRUSER('The fitted values will use the opposite',STATUS)
      CALL RDKEYL('POINTS',.TRUE.,1,MARK,I,STATUS)
      MARKF=.NOT.MARK
C
C   First decide whether to work in epoch or phase terms.
C   (Former is the favourite)
C
      EPO=.TRUE.
      CALL WRUSER('Do you wish to work in EPOCH (YES) or PHASE (NO)',
     :             STATUS)
      CALL RDKEYL('EPOCH',.TRUE.,1,EPO,I,STATUS)
C
C   Now split the data into suitable arrays
C   depending on whether phase or epoch is required.
C
      NSAMP=100
  200 CONTINUE
      IF (EPO) THEN
C
C   It may be easier to work on a short range of epochs
C   in graphics mode (so you can see individual points)
C
         CALL WRUSER('Enter 2 identical values (eg 0,0) to exit',
     :                STATUS)
         CALL RDKEYD('EPOCHS',.TRUE.,2,EP,I,STATUS)
         IF (EP(1).NE.EP(2)) THEN
            CALL PER_SPLIT(%VAL(PWORK),AX(2),%VAL(PX),%VAL(PY),
     :                     EP,YR,NVAL)
C
C      Now draw some axes
C
            DO I=1,2
               XPR(I)=EP(I)
               YPR(I)=YR(I)
            END DO
C
C         The Y scale is increased a little to leae room
C         for a title!
C
            YPR(2)=YPR(2)*1.15
            CALL JBAXES(XPR,2,XL,'EPOCH',5,YPR,2,YL,'VALUE',5)
         ELSE
            GO TO 9999
         END IF
      ELSE
C
C      If we are working in phases we need a period or frequency
C      and then to compute the phases.
C
         CALL RDKEYL('FREQ',.FALSE.,1,FRE,I,STATUS)
         IF (FRE) THEN
            CALL WRUSER('Enter FREQUENCY to calculate phases',STATUS)
         ELSE
            CALL WRUSER('Enter PERIOD to calculate phases',STATUS)
         END IF
         CALL WRUSER('Hit <CR> to exit',STATUS)
         CALL RDKEYD('VALUE',.FALSE.,1,FREQ,I,STATUS)
         IF (STATUS.NE.ERR_NORMAL) THEN
            GO TO 9999
         ELSE
            IF (.NOT.FRE) THEN
               FREQ=1.0/FREQ
            END IF
C
C      Compute the phases for this frequency
C
            CALL PER_PHASE(%VAL(PWORK),AX(2),%VAL(PX),%VAL(PY),
     :                     FREQ,YR,NVAL)
C
C         Plot some axes
C
            XPR(1)=0.0
            XPR(2)=1.0
            YPR(1)=YR(1)
            YPR(2)=YR(2)*1.15
            CALL JBAXES(XPR,2,XL,'PHASE',5,YPR,2,YL,'VALUE',5)
         END IF
      END IF
C
C   Plot the results
C
      CALL PER_PLOTL(%VAL(PX),%VAL(PY),NVAL,0,0,MARK,0.0,YPR)
      CALL CNPAR('EPOCHS',STATUS)
      CALL CNPAR('VALUE',STATUS)
C
C   Now create data from the fit
C
      CALL RDKEYI('NSAMP',.TRUE.,1,NSAMP,I,STATUS)
      CALL GETDYN('FX',FMT_R,NSAMP,PFX,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('FY',FMT_R,NSAMP,PFY,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL PER_CREDAT(%VAL(PFX),%VAL(PFY),NSAMP,FI,CI,PI,AXP(2),
     :                EPO,TZERO,EP,FREQ)
C
C   and plot the resulting data
C
      CALL PER_PLOTL(%VAL(PFX),%VAL(PFY),NSAMP,0,0,MARKF,0.0,YPR)
C
C   and add a title.
C
      GRTEXT=' '
      CALL RDKEYC('TITLE',.TRUE.,1,GRTEXT,I,STATUS)
      IF (GRTEXT.NE.' ') THEN
C
C      Find out how long it is
C
         DO I=72,1,-1
            IF (GRTEXT(I:I).NE.' ') THEN
               NCHAR=I
               GO TO 100
            END IF
         END DO
  100    CONTINUE
         CALL TITLE(1,2,IGRT,NCHAR)
      END IF
      CALL CNPAR('TITLE',STATUS)
C
C   clear out the workspace
C
      CALL CNPAR('FX',STATUS)
      CALL CNPAR('FY',STATUS)
      CALL FRDATA('FX',STATUS)
      CALL FRDATA('FY',STATUS)
      CALL CNPAR('NSAMP',STATUS)
C
C   Go back for another try
C
      GO TO 200
C
C   In any case tidy up and exit
C
 9999 CONTINUE
      CALL ENDPLT
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
