      PROGRAM NON
C+
C
C   Program NON
C
C   This program reads in approximate values for up to 20 sets
C   of frequency, amplitude and phase and does a full non-linear
C   least squares fit to obtain the best possible values for
C   these parameters.
C
C   The results are stored in a BDF file, for subsequent use by SYNTH,
C   or whatever, and a report is written to NON.LIS.
C
C   Written by C.D.Pike at RGO
C   (Modified for this version by K.F.Hartley, 28-2-84)
C
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),AXF(2),AXO(2),PIN1,PIN2,POUT
      DOUBLE PRECISION FKNOWN(20),C(21),PHI(20),PARMS(3)
      DOUBLE PRECISION VECTOR(61)
      DOUBLE PRECISION NEWAMP(20),NEWPHI(20),NEWF(20)
      DOUBLE PRECISION TMEAN,EPOCH,DELTA
      LOGICAL FRE
      CHARACTER*72 TEXT
C
C      Pick up a dataset
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN1,ISTAT)
      CALL PER_STOP(STATUS,'ERRDIN')
C
C   Pick up KNOWN frequencies
C
      CALL RDIMAG('PARIN',FMT_DP,2,AXF,I,PIN2,ISTAT)
      IF(ISTAT.NE.ERR_NORMAL)  THEN
         C(1)=0.0
         CALL RDKEYR('MEANVAL',.TRUE.,1,C(1),I,STATUS)
         FRE=.TRUE.
         CALL RDKEYL('FREQ',.TRUE.,1,FRE,I,STATUS)
         IF (FRE) THEN
            CALL WRUSER('Enter initial FREQUENCY, amplitude and phase',
     :                   STATUS)
         ELSE
            CALL WRUSER('Enter initial PERIOD, amplitude and phase',
     :                   STATUS)
         END IF
         CALL WRUSER('Hit <CR> to end',STATUS)
         IFREQ = 0
   10    CONTINUE
         CALL RDKEYD('VALUE',.FALSE.,3,PARMS,I,ISTAT)
         IF(ISTAT.EQ.ERR_NORMAL)   THEN
            IFREQ = IFREQ + 1
            IF (FRE) THEN
              FKNOWN(IFREQ)=PARMS(1)
            ELSE
               FKNOWN(IFREQ)=1.0/PARMS(1)
            END IF
            C(IFREQ+1) = PARMS(2)
            PHI(IFREQ) = PARMS(3)
            CALL CNPAR('VALUE',ISTAT)
            GO TO 10
         ENDIF
      ELSE
         CALL PER_PUTFRE3(%VAL(PIN2),AXF,FKNOWN,C,PHI)
C
C      Now pick up the mean value
C
         CALL RDDSCR('PARIN','MEAN',1,TEXT,I,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            CALL CTOR(TEXT,TEMP,STATUS)
            C(1) = TEMP
         ELSE
            C(1) = 0.0
         END IF
C
C      and the epoch to which the phases refer
C
         CALL RDDSCR('PARIN','EPOCH',1,TEXT,I,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            CALL CTOR(TEXT,TZERO,STATUS)
         ELSE
            TZERO=0.0
         END IF
C
C      Re-set phases to correspond to the mean epoch of this dataset.
C
         IFREQ=AXF(2)
         CALL PER_RESETP(%VAL(PIN1),AX,FKNOWN,PHI,IFREQ,TZERO)
      END IF
C
C   Check that some frequencies have been found
C
      IF (IFREQ.LE.0) THEN
         CALL WRUSER('No frequencies have been entered',STATUS)
         GO TO 999
      END IF
C
C  Call routine to do the non-linear fit
C
      CALL PER_NONLFIT(%VAL(PIN1),AX,FKNOWN,IFREQ,C,PHI,TMEAN,EPOCH)

C
C   Obtain an output dataset for the computed parameters.
C
      AXO(1)=3
      AXO(2)=IFREQ
      CALL WRIMAG('PAROUT',FMT_DP,AXO,2,POUT,ISTAT)
      IF(ISTAT.NE.ERR_NORMAL)  THEN
         CALL WRUSER('Results not stored ',ISTAT)
      ELSE
C
C     adjust phase to user defined zero 
C
         CALL RDKEYD('EPOCH',.TRUE.,1,EPOCH,NVAL,ISTAT)
         DO I=1,IFREQ
            NEWF(I) = FKNOWN(I)
            NEWAMP(I) = C(I+1)
            IF(TMEAN.GT.EPOCH)  THEN
               DELTA = (TMEAN-EPOCH)*FKNOWN(I)
               IDEL = INT(DELTA)
               DELTA = DELTA - IDEL
               NEWPHI(I) = PHI(I)/6.283185D0 - DELTA
               IF(NEWPHI(I).LT.0.0)  NEWPHI(I) = NEWPHI(I) + 1.0
            ELSE
               DELTA = (EPOCH-TMEAN)*FKNOWN(I)
               IDEL = INT(DELTA)
               DELTA = DELTA - IDEL
               NEWPHI(I) = PHI(I)/6.283185D0 + DELTA
               IF(NEWPHI(I).GT.1.0)  NEWPHI(I) = NEWPHI(I) - 1.0
            END IF
            IF(ABS(NEWPHI(I)).LT.0.0001)  NEWPHI(I) = 0.0
C
C         Finally at the end of the day convert phase back into radians
C
            NEWPHI(I) = NEWPHI(I)*6.283185D0
         END DO
C
C      Store the results in the output array
C
         CALL PER_PUTPAR(NEWF,NEWAMP,NEWPHI,IFREQ,%VAL(POUT),AXO)
C
C      and add descriptors for EPOCH of the phases and
C      the mean level found.
C
         WRITE (TEXT,900) EPOCH
         CALL WRDSCR('PAROUT','EPOCH',TEXT,1,STATUS)
         WRITE (TEXT,900) C(1)
         CALL WRDSCR('PAROUT','MEAN',TEXT,1,STATUS)
  900    FORMAT (1H ,D15.8)
C
C      Finally write the results to the terminal
C
         WRITE (TEXT,910)
  910    FORMAT (14X,'Frequency',6X,'Amplitude',10X,'Phase')
         CALL WRUSER(' ',STATUS)
         CALL WRUSER(TEXT,STATUS)
         CALL WRUSER(' ',STATUS)
         CALL WRUSER(' ',STATUS)
         DO I=1,IFREQ
            WRITE (TEXT,920) NEWF(I),NEWAMP(I),NEWPHI(I)
  920       FORMAT (11X,3(F12.5,3X))
            CALL WRUSER(TEXT,STATUS)
         END DO
         CALL WRUSER(' ',STATUS)
         WRITE (TEXT,930) EPOCH
  930    FORMAT (11X,'The phases are referred to epoch ',F12.5)
         CALL WRUSER(TEXT,STATUS)
         WRITE (TEXT,940) C(1)
  940    FORMAT(11X,'The mean value found is ',F12.5)
         CALL WRUSER(TEXT,STATUS)
      END IF
C
C   Tidy up and exit
C
  999 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
