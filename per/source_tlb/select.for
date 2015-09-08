      PROGRAM SELECT
C+
C
C   Program SELECT
C
C   Allows the user to select a sub-set of data for storage as
C   a seperate dataset. Selection may be in terms of epoch or phase,
C   and is done by defining a box in epoch/value (or phase/value)
C   space and saving or excluding all points within the box.
C
C   The program may be run in graphical mode, using a cursor, or
C   under keyboard control.
C
C   If only a few points are to be rejected they should be marked
C   as bad by using the program FLAG instead.
C
C   Written by K.F.Hartley at RGO on 27-Jan-84
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER PIN,POUT,PX,PY,PWORK,STATUS
      DOUBLE PRECISION EP(2),ORGEP(2),YR(2),FREQ,OLDEP(2),ZEPOCH
      DOUBLE PRECISION XLIM(2),YLIM(2),TEMP
      INTEGER AX(2),AXO(2)
      LOGICAL CUR,YES,EPO,FRE,IN,AGAIN,FIRST,CHANGE
      CHARACTER*8 DEV
      CHARACTER*72 TEXT
C
C   The parameter FIRST defines whether a graphics devive has
C   or has not been initialized.
C
      FIRST=.TRUE.
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
C   More than one selection may be made - each selection being
C   put into a separate output file. Control returns to here for
C   second and subsequent selections, controlled by the parameter
C   AGAIN.
C
    1 CONTINUE
C
C   Copy data to workspace
C
      CALL PER_COPY(%VAL(PIN),%VAL(PWORK),AX(1),AX(2),EP)
C
C   and store the full range of epochs
C
      ORGEP(1)=EP(1)
      ORGEP(2)=EP(2)
C
C   Now obtain space for two 1-D arrays for plotting
C
      CALL GETDYN('X',FMT_R,AX(2),PX,STATUS)
      CALL PER_STOP(STATUS,'HELLWRK')
      CALL GETDYN('Y',FMT_R,AX(2),PY,STATUS)
      CALL PER_STOP(STATUS,'HELLWRK')
C
C   Now handle device and opening SIMPLEPLOT
C   if it is the first time through the program
C
      IF (FIRST) THEN
         DEV='ARGS'
         CALL WRUSER('If you dont want graphics say NONE',STATUS)
         CALL RDKEYC('DEVICE',.TRUE.,1,DEV,I,STATUS)
C
C      First test to see if NO or NONE etc. was entered.
C
         CALL STR$UPCASE(DEV,DEV)
         IF (DEV(1:2).EQ.'NO') THEN
            CUR=.FALSE.
            GO TO 100
         END IF
         CALL DEVTRAN(DEV,IDEV,ICONID,ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL WRUSER('Failed to recognize that device',STATUS)
            CALL WRUSER('Will use keyboard instead',STATUS)
            GO TO 100
         END IF
         CALL JBDEV(DEV)
C
C      *** Device specific code to handle bug in HP driver ***
C
         IF (IDEV.EQ.12) PRINT *,'I'
         CALL JBINQ(XL,YL,CUR)
         IF (.NOT.CUR) THEN
            CALL WRUSER('That device has no cursor',STATUS)
            CALL WRUSER('Will use keyboard instead',STATUS)
            GO TO 100
         END IF
  100    CONTINUE
      END IF
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
  200 CONTINUE
      IF (EPO) THEN
C
C   It may be easier to work on a short range of epochs
C   in graphics mode (so you can see individual points)
C
         IF (CUR) THEN
            CALL WRUSER('You may find it easier to look at only a'//
     :                ' limited range of epochs',STATUS)
            CALL RDKEYD('EPOCHS',.TRUE.,2,EP,I,STATUS)
            IF (FIRST) THEN
               OLDEP(1)=EP(1)
               OLDEP(2)=EP(2)
               CHANGE=.TRUE.
            ELSE
               IF (OLDEP(1).EQ.EP(1).AND.OLDEP(2).EQ.EP(2)) THEN
                  CHANGE=.FALSE.
               ELSE
                  CHANGE=.TRUE.
               END IF
               OLDEP(1)=EP(1)
               OLDEP(2)=EP(2)
            END IF
         END IF
         IF (EP(1).NE.EP(2)) THEN
            CALL PER_SPLIT(%VAL(PWORK),AX(2),%VAL(PX),%VAL(PY),
     :                     ORGEP,YR,NVAL)
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
         CALL RDKEYD('VALUE',.FALSE.,1,FREQ,I,STATUS)
         IF (STATUS.NE.ERR_NORMAL) THEN
            CALL CNPAR('VALUE',STATUS)
            GO TO 200
         ELSE
            IF (.NOT.FRE) THEN
               FREQ=1.0/FREQ
            END IF
            ZEPOCH=ORGEP(1)
            CALL RDKEYD('ZEPOCH',.TRUE.,1,ZEPOCH,I,STATUS)
            EP(1)=0.0
            EP(2)=1.0
C
C      Now sort the data into order of increasing phase
C
            CALL PER_PHAZE(%VAL(PWORK),AX(2),%VAL(PX),%VAL(PY),
     :                     FREQ,YR,NVAL,ZEPOCH)
         END IF
         CHANGE=.FALSE.
      END IF
C
C   Now define the search strategy.
C
      CALL WRUSER('Define a box (limits in EPOCH and VALUE)',STATUS)
      CALL WRUSER('and then say whether you wish to select',STATUS)
      CALL WRUSER('only those points inside the box, or,',STATUS)
      CALL WRUSER('all points outside the box.',STATUS)
C
C   First the box - depending on mode
C
      IF (CUR) THEN
         IF (FIRST.OR.CHANGE) THEN
            CALL PER_PLOT(%VAL(PX),%VAL(PY),NVAL,EP,YR,XL,YL)
         END IF
         CALL CURSOR(XP,YP)
         XLIM(1)=XP
         YLIM(1)=YP
         CALL CURSOR(XP,YP)
         XLIM(2)=XP
         YLIM(2)=YP
C
C      In order to draw the box properly it is essential that
C      the cursor position be on the graph. This does not
C      affect the selection because the edges of the box are
C      always considered to be part of the box.
C
         DO I=1,2
            IF (XLIM(I).LT.EP(1)) XLIM(I)=EP(1)
            IF (XLIM(I).GT.EP(2)) XLIM(I)=EP(2)
            IF (YLIM(I).LT.YR(1)) YLIM(I)=YR(1)
            IF (YLIM(I).GT.YR(2)) YLIM(I)=YR(2)
         END DO
C
C      Now draw the box to help the user
C
         CALL BREAK
         XP=XLIM(1)
         YP=YLIM(1)
         CALL JOIN PT(XP,YP)
         YP=YLIM(2)
         CALL JOIN PT(XP,YP)
         XP=XLIM(2)
         CALL JOIN PT(XP,YP)
         YP=YLIM(1)
         CALL JOIN PT(XP,YP)
         XP=XLIM(1)
         CALL JOIN PT(XP,YP)
      ELSE
         XLIM(1)=EP(1)
         XLIM(2)=EP(2)
         YLIM(1)=YR(1)
         YLIM(2)=YR(2)
         CALL RDKEYD('XLIMITS',.TRUE.,2,XLIM,I,STATUS)
         CALL RDKEYD('YLIMITS',.TRUE.,2,YLIM,I,STATUS)
      END IF
C
C   Then sort it out so the limits are in the right order
C
      IF (XLIM(1).GT.XLIM(2)) THEN
         TEMP=XLIM(1)
         XLIM(1)=XLIM(2)
         XLIM(2)=TEMP
      END IF
      IF (YLIM(1).GT.YLIM(2)) THEN
         TEMP=YLIM(1)
         YLIM(1)=YLIM(2)
         YLIM(2)=TEMP
      END IF
C
C   Now decide whether inside or outside
C
      CALL WRUSER('Select only points inside this box (YES)',STATUS)
      CALL WRUSER('or only those outside it (NO)',STATUS)
      IN=.TRUE.
      CALL RDKEYL('INSIDE',.TRUE.,1,IN,I,STATUS)
C
C   Now do the actual selection
C
      CALL PER_SELCT(%VAL(PWORK),AX(2),%VAL(PX),%VAL(PY),NVAL,
     :                XLIM,YLIM,IN,NOUT)
C
C   Finally obtain space fo the output
C
      AXO(1)=3
      AXO(2)=NOUT
C
C   That completes the work so store the results.
C
      CALL WRIMAG('OUTPUT',FMT_DP,AXO,2,POUT,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
C
C   Copy the results into the output array
C
         CALL PER_OUT2W(%VAL(PWORK),AX(2),%VAL(POUT),NOUT)
C
C      Allow an optional title
C
         TEXT=' '
         CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
         CALL WRDSCR('OUTPUT','TITLE',TEXT,1,STATUS)
      ELSE
         CALL WRERR('HELLOUT',STATUS)
      END IF
C
C   Now see if another selection is required.
C
      AGAIN=.TRUE.
      CALL RDKEYL('AGAIN',.TRUE.,1,AGAIN,I,STATUS)
      IF (AGAIN) THEN
         CALL CNPAR('AGAIN',STATUS)
         FIRST=.FALSE.
         CALL CNPAR('EPOCHS',STATUS)
         CALL CNPAR('INSIDE',STATUS)
         CALL CNPAR('TITLE',STATUS)
         CALL FRDATA('X',STATUS)
         CALL FRDATA('Y',STATUS)
         CALL FRDATA('OUTPUT',STATUS)
         CALL CNPAR('X',STATUS)
         CALL CNPAR('Y',STATUS)
         CALL CNPAR('OUTPUT',STATUS)
         GO TO 1
      END IF
C
C   In any case tidy up and exit
C
 9999 CONTINUE
      CALL ENDPLT
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
