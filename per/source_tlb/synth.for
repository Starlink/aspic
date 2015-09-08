      PROGRAM SYNTH
C+
C     Program SYNTH
C
C     Creates synthetic data for the period finding programs.
C
C         It takes its epochs from an input dataset, unless a
C         null response is given, in which case it will create
C         equally spaced data starting at a given epoch with
C         a specified sampling interval.
C
C         It then prompts for a mean level,
C         the standard deviation of the (additive noise)
C         and then the parameters for up to 20 sine waves.
C         The parameters are amplitude, frequency (or period)
C         and phase relative to the starting epoch.
C         These may be read from a parameter set created by LIN or
C         NON, or may be input from the keyboard.
C
C         Note that the choice of frequency or period is defined
C         by the external commands FREQ or PERIOD (which set a global
C         symbol GLOBAL_FREQ to YES or NO.
C
C   Written by K.F.Hartley at RGO on 24-Jan-1984
C   (Based on earlier versions by K.F.Hartley and C.D.Pike)
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),AXP(2),PIN,POUT,PPAR,STATUS
      REAL FREQ(20),AMP(20),PHASE(20),CONS,NOISE,PARAMS(3)
      DOUBLE PRECISION EPOCH,INTVAL,TZERO
      CHARACTER*72 TITLE,TEXT
      LOGICAL FRE
C
C   First try to get an input dataset.
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
C
C   If that fails then make one up.
C
         CALL RDKEYI('NSAMP',.FALSE.,1,NSAMP,I,STATUS)
         IF (STATUS.NE.ERR_NORMAL) GO TO 9999
         CALL RDKEYD('EPOCH',.FALSE.,1,EPOCH,I,STATUS)
         IF (STATUS.NE.ERR_NORMAL) GO TO 9999
         CALL RDKEYD('INTERVAL',.FALSE.,1,INTVAL,I,STATUS)
         IF (STATUS.NE.ERR_NORMAL) GO TO 9999
C
C      Now get some workspace
C
         NSIZE=3*NSAMP
         CALL GETDYN('WORK',FMT_DP,NSIZE,PIN,STATUS)
         CALL PER_STOP(STATUS,'HELLWRK')
         AX(1)=3
         AX(2)=NSAMP
         CALL PER_MAKE(%VAL(PIN),AX(1),AX(2),EPOCH,INTVAL)
      ELSE
         CALL PER_FRSTEP(%VAL(PIN),AX(1),AX(2),EPOCH)
      END IF
C
C   Now obtain the parameters which define the data.
C
C   First see if a parameter set exists.
C
      CALL RDIMAG('PARIN',FMT_DP,2,AXP,I,PPAR,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CONS = 0.0
         CALL RDKEYR('CONSTANT',.TRUE.,1,CONS,I,STATUS)
         IF(STATUS.GT.ERR_PARNUL)  GO TO 9999
C
C   Then the standard deviation of the noise - default is no noise!
C
         NOISE = 0.0
         CALL RDKEYR('NOISE',.TRUE.,1,NOISE,I,STATUS)
         IF(STATUS.GT.ERR_PARNUL)  GO TO 9999
C
C   Now the details of the sine waves.
C
C   But first decide whether user is working in frequency
C   or amplitude
C
         CALL RDKEYL('FREQ',.FALSE.,1,FRE,I,STATUS)
         IF (FRE) THEN
            CALL WRUSER('Enter FREQUENCY, amplitude and phase',STATUS)
         ELSE
            CALL WRUSER('Enter PERIOD, amplitude and phase',STATUS)
         END IF
         CALL WRUSER('N.B. Phase refers to the first epoch',STATUS)
         DO I=1,20
            CALL RDKEYR('PARAMS',.FALSE.,3,PARAMS,II,STATUS)
            IF(STATUS.GE.ERR_PARNUL)  GO TO 300
C
C      Note that no matter what the user is using, this program
C      works in frequency
C
            IF (FRE) THEN
                  FREQ(I)=PARAMS(1)
            ELSE
                  FREQ(I)=1.0/PARAMS(1)
            END IF
            AMP(I) = PARAMS(2)
            PHASE(I) = PARAMS(3)
            CALL CNPAR('PARAMS',STATUS)
         END DO
  300    CONTINUE
         IFREQ = I-1
         TZERO=EPOCH
      ELSE
         CALL PER_PAREXT(%VAL(PPAR),AXP,FREQ,AMP,PHASE)
         IFREQ=AXP(2)
         NOISE=0.0
         CALL RDKEYR('NOISE',.TRUE.,1,NOISE,I,STATUS)
         CALL RDDSCR('PARIN','MEAN',1,TEXT,I,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            CALL CTOR(TEXT,CONS,STATUS)
         ELSE
            CONS = 0.0
         END IF
         CALL RDDSCR('PARIN','EPOCH',1,TEXT,I,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            CALL CTOR(TEXT,T,STATUS)
            TZERO = T
         ELSE
            TZERO = EPOCH
         END IF
      END IF
      CALL WRIMAG('OUTPUT',FMT_DP,AX,2,POUT,STATUS)
      CALL PER_STOP(STATUS,'HELL')
      CALL PER_SYNDAT(%VAL(PIN),%VAL(POUT),AX(1),AX(2),
     1           FREQ,AMP,PHASE,CONS,NOISE,IFREQ,TZERO)
C
C   Finish off by adding a title
C
      TITLE=' '
      CALL RDKEYC('TITLE',.TRUE.,1,TITLE,I,STATUS)
      CALL WRDSCR('OUTPUT','TITLE',TITLE,1,STATUS)
 9999 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END 
