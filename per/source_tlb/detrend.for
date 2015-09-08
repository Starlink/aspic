      PROGRAM DETREND
C+
C
C      Program DETREND
C
C      It fits a polynomial of specified degree to an input
C      dataset and subtracts it from the data, storing the
C      result in an output dataset.
C
C      Optionally the same correction can be applied to as many
C      data sets as required, which need not be sampled
C      at the same epochs, though clearly they should be
C      in the same range.
C      (An example of this might be where a slope has been
C      removed from data after it has ben pre-whitened, which
C      should also be taken out of the original data.)
C
C      Written by K.F.Hartley at RGO on 25-Jan-1984
C      (based on earlier versions by K.F.Hartley and C.D.Pike)
C
C-
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      INTEGER PIN,POUT,AX(2),STATUS
      INTEGER PX,PY,PW,PWRK
C
C   Get the input dataset
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      CALL PER_STOP(STATUS,'HELLIN')
C
C      and output data set.
C
      CALL WRIMAG('OUTPUT',FMT_DP,AX,2,POUT,STATUS)
      CALL PER_STOP(STATUS,'HELLOUT')
C
C      Pick up the degree of polynomial to be used
C      (Default is a constant ie reduce to zero mean)
C
      MAXD=0
      CALL WRUSER('   Enter degree of polynomial to be used',STATUS)
      CALL WRUSER('   0 for constant, 1 for straight line etc.',STATUS)
      CALL WRUSER('   Up to a value of 11',STATUS)
      CALL RDKEYI('DEGREE',.TRUE.,1,MAXD,I,STATUS)
      IF (MAXD.LT.0) THEN
         MAXD=0
         CALL WRUSER('Set to 0',STATUS)
      ELSE IF (MAXD.GT.11) THEN
         MAXD=11
         CALL WRUSER('Set to 11',STATUS)
      END IF
C
C   Now get workspace for the fitting
C
      NSAMP=AX(2)
      NSAMP3=NSAMP*3
      CALL GETDYN('X',FMT_DP,NSAMP,PX,IS1)
      CALL PER_STOP(IS1,'ERRDYN')
      CALL GETDYN('Y',FMT_DP,NSAMP,PY,IS2)
      CALL PER_STOP(IS2,'ERRDYN')
      CALL GETDYN('W',FMT_DP,NSAMP,PW,IS3)
      CALL PER_STOP(IS3,'ERRDYN')
      CALL GETDYN('WORK',FMT_DP,NSAMP3,PWRK,IS4)
      CALL PER_STOP(IS4,'ERRDYN')
C
C   Now do the work - Note that the details of the fit are
C   passed into PER_REMVE via a common block.
C
      CALL PER_POLFT(%VAL(PIN),%VAL(PX),%VAL(PY),%VAL(PW),
     :               %VAL(PWRK),%VAL(POUT),AX(1),AX(2),MAXD)
C
C   Having completed the fit it may be used for other datasets.
C
  100 CONTINUE
      CALL WRUSER('Enter name of a second dataset to which',STATUS)
      CALL WRUSER('the SAME correction is to be applied',STATUS)
      CALL WRUSER('(Need not be at the same epochs)',STATUS)
      CALL WRUSER('Hit <CR> to exit',STATUS)
      CALL  FRDATA('INPUT',STATUS)
      CALL FRDATA('OUTPUT',STATUS)
      CALL CNPAR('INPUT',STATUS)
      CALL CNPAR('OUTPUT',STATUS)
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
C
C      Obtain an output dataset
C
         CALL WRIMAG('OUTPUT',FMT_DP,AX,2,POUT,STATUS)
         CALL PER_STOP(STATUS,'HELLOUT')
C
C      Now subtract the fit from the new dataset
C
         CALL PER_REMVE(%VAL(PIN),%VAL(POUT),AX(1),AX(2))
C
C      Now go back for another dataset
C
         GO TO 100
      END IF
C
C   Now tidy up and go home
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
