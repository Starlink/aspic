      PROGRAM FLAG
C+
C
C   Program FLAG
C
C   Allows the user to select individual points from an existing
C   dataset and flag them with zero weight, so that they are no
C   longer included in the analysis.
C
C   This may be of use in removing typing errors or clearly
C   defective measurements.
C
C   If whole sets of points are to be removed users are advised
C   to use the programs SELECT and EXTRACT.
C
C   Written by K.F.Hartley at RGO on 25-Jan-84
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER PIN,POUT,PX,PY,PWORK,STATUS
      DOUBLE PRECISION EP(2),YR(2)
      INTEGER AX(2),AXO(2)
      LOGICAL CUR
      CHARACTER*8 DEV
      CHARACTER*72 TEXT
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
C   Now handle device and opening SIMPLEPLOT
C
  100 CONTINUE
      DEV='ARGS'
      CALL WRUSER('Device MUST have a cursor',STATUS)
      CALL RDKEYC('DEVICE',.TRUE.,1,DEV,I,STATUS)
      CALL DEVTRAN(DEV,IDEV,ICONID,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL PER_DEVHLP
         CALL CNPAR('DEVICE',STATUS)
         GO TO 100
      END IF
      CALL JBDEV(DEV)
C
C   Lousy code to fix bug in HP driver
C
      IF (IDEV.EQ.12) PRINT *,'II'
      CALL JBINQ(XL,YL,CUR)
      IF (.NOT.CUR) THEN
         CALL WRUSER('That device has no cursor',STATUS)
         CALL WRUSER('Cannot continue - try again!',STATUS)
         GO TO 9999
      END IF
C
C   Obtain a range of epochs
C
  200 CONTINUE
      CALL WRUSER('Select range of epochs',STATUS)
      CALL WRUSER('Enter two equal values (eg. 0,0) to exit',STATUS)
      CALL RDKEYD('EPOCHS',.TRUE.,2,EP,I,STATUS)
      IF (EP(1).NE.EP(2)) THEN
         CALL PER_SPLIT(%VAL(PWORK),AX(2),%VAL(PX),%VAL(PY),
     :                  EP,YR,NVAL)
C
C      Now plot the data
C
         CALL PER_PLOT(%VAL(PX),%VAL(PY),NVAL,EP,YR,XL,YL)
C
C      Now remove any points that are not wanted
C
         CALL PER_FLAGEM(%VAL(PWORK),AX(2))
C
C      In principle some of the weights in array PWORK will
C      now have been set to zero.
C
C      So copy them into the two 1-D arrays again
C
         CALL PER_SPLIT(%VAL(PWORK),AX(2),%VAL(PX),%VAL(PY),
     :                  EP,YR,NVAL)
         CALL CNPAR('EPOCHS',STATUS)
         GO TO 200
      END IF
C
C   Count the samples with non-zero weights
C
      CALL PER_COUNTW(%VAL(PWORK),AX(2),NONZ)
C
C   That completes the work so store the results.
C   Ensure that the output has room for the weights
C
      AXO(1)=3
      AXO(2)=NONZ
      CALL WRIMAG('OUTPUT',FMT_DP,AXO,2,POUT,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
C
C   Copy the results into the output array
C
         CALL PER_OUT2(%VAL(PWORK),AX(2),%VAL(POUT),AXO(2))
C
C   and add an optional title.
C
         TEXT=' '
         CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
         CALL WRDSCR('OUTPUT','TITLE',TEXT,1,STATUS)
      ELSE
         CALL WRERR('HELLOUT',STATUS)
      END IF
C
C   In any case tidy up and exit
C
 9999 CONTINUE
      CALL ENDPLT
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
