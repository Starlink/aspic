      PROGRAM EXTEND
C+
C
C   Program EXTEND
C
C  This is a very dodgy program, and should only be used as a last
C  resort, when trying to process data beyond its natural limits!
C
C  It takes a dataset and the set of coefficients calculated by MEMPOW
C  and extends the data to earlier or later epochs, so that the
C  result may be compared with fragmentary data which cannot be
C  added to the good data.
C
C  The results should not be trusted except as a guide to what
C  might be happening!
C
C  Written by K.F.Hartley at RGO on 17-1-84
C
C  Based on program MEMPS by
C  CDP/RGO  5/1/82
C
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER AXIS,PIN,POUT,STATUS,AXES(2),PIN0
      INTEGER AXOUT(2)
      DOUBLE PRECISION FREQ(2),P(1000),Q(1000),GAM(1000)
      CHARACTER*72 TEXT
      CHARACTER*10 DESCR
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C  Read image to get times
C
      CALL RDIMAG('INPUT',FMT_DP,2,AXES,NAX,PIN0,ISTAT)
      CALL PER_STOP(STATUS,'ERRIN')
C
C  Get new epoch
C
      CALL PER_GETLIM(%VAL(PIN0),AXES,ST,END)
      WRITE (TEXT,900) ST
  900 FORMAT (1H ,'Start epoch is ',E16.7)
      CALL WRUSER(TEXT,ISTAT)
      WRITE (TEXT,910) END
  910 FORMAT (1H ,'End epoch is ',E16.7)
      CALL WRUSER(TEXT,ISTAT)
  100 CONTINUE
      CALL RDKEYD('EPOCH',.FALSE.,1,EP,I,ISTAT)
      CALL PER_STOP(STATUS,'ERREP')
      IF (ST.LE.EP.AND.EP.LE.END) THEN
         CALL WRUSER('New epoch must be outside existing range',ISTAT)
         CALL CNPAR('EPOCH',ISTAT)
         GO TO 100
      END IF
C
C   Now work out how many points are needed for the output.
C
      DELTAT=(END-ST)/DBLE(AXES(2)-1)
      IF (EP.LT.ST) THEN
         NSAMP= (END-EP)/DELTAT + 1
         IFRST=1
         ISCND=NSAMP-AXES(2)
      ELSE
         NSAMP = (EP-ST)/DELTAT + 1
         IFRST=AXES(2)+1
         ISCND=NSAMP
      END IF
C
C  Pick up the coefficients of the autoregressive model
C
      CALL RDIMAG('COFFS',FMT_DP,1,AXIS,NAX,PIN,STATUS)
      CALL PER_STOP(STATUS,'ERRCOF')
      M=AXIS
C
C   Get an output dataset
C
      AXOUT(1)=3
      AXOUT(2)=NSAMP
      CALL WRIMAG('OUTPUT',FMT_DP,AXOUT,2,POUT,STATUS)
      CALL PER_STOP(STATUS,'ERROUT')
C
C   Now perform the extrapolation
C
      CALL PER_EXTP(%VAL(PIN0),AXES,%VAL(POUT),AXOUT,%VAL(PIN),M,
     :              IFRST,ISCND)
C
C   Now record the fact that extrapoated data are present
C
      WRITE (DESCR,'(I6)') IFRST
      CALL WRDSCR('OUTPUT','FIRST',DESCR,1,ISTAT)
      WRITE (DESCR,'(I6)') ISCND
      CALL WRDSCR('OUTPUT','SECOND',DESCR,1,ISTAT)
C
C   and add a title
C
      TEXT=' '
      CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,ISTAT)
      CALL WRDSCR('OUTPUT','TITLE',TEXT,1,ISTAT)
C
C   That completes the program so tidy up and exit
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END


