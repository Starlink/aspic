      PROGRAM PW
C+
C
C   Program PW
C
C   Values computed from a set of parameters are removed from an
C   existing dataset, the results being stored as a new dataset.
C
C   This process is known as "pre-whitening with respect to the
C   the specified frequencies" and hence the name of this program.
C
C   Written by K.F.Hartley at RGO on 1-Mar-84
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER PIN,PP,POUT,STATUS
      DOUBLE PRECISION EP(2),YR(2),FREQ
      DOUBLE PRECISION XLIM(2),YLIM(2),TEMP,TZERO
      REAL SIZE(2),XPR(2),YPR(2)
      DOUBLE PRECISION FI(20),CI(21),PI(20)
      INTEGER AX(2),AXP(2)
      LOGICAL CUR,YES,EPO,FRE,IN,MARK,MARKF
      CHARACTER*8 DEV
      CHARACTER*72 TEXT
C
C   First obtain the input dataset
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      CALL PER_STOP(STATUS,'HELLIN')
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
         EPOCH = TEMP
      ELSE
         TZERO=EP(1)
      END IF
C 
C   Obtain an output dataset
C 
      CALL WRIMAG('OUTPUT',FMT_DP,AX,2,POUT,STATUS)
      CALL PER_STOP(STATUS,'ERROUT')
C
C   Now create data from the fit and do the subtraction.
C
      CALL PER_SUBFIT(%VAL(PIN),%VAL(POUT),AX,FI,CI,PI,AXP(2),EPOCH)
C
C   Obtain a title for the output dataset
C
      TEXT=' '
      CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
      CALL WRDSCR('OUTPUT','TITLE',TEXT,1,STATUS)
C
C   In any case tidy up and exit
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
