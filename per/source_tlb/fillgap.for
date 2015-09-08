      PROGRAM FILLGAP
C+
C   Program FILLGAP
C
C   This uses the maximum entropy / auto-regressive model to fill
C   gaps between up to 50 sets of equally spaced, "coherent" data.
C   It uses a simpler version of the auto-regressive model fitting
C   algorithm than that used by MEMPOW, because it has to be done
C   several times and the possible gains are not offset against
C   greater accuracy. (In fact the bi-directional approach seems
C   to be unstable when long gaps are present). It therefore
C   uses the earlier method of "triangular weights" applied to
C   the forward and backward predictions from data on either side
C   of each gap to fill in that gap. The whole cycle is then repeated
C   but fitting a model to the combined good and made-up data. The
C   whole process ought to converge!
C
C
C   This version written by K.F.Hartley at RGO on 2-Apr-1984,
C   but based directly on the original by Balona and the earlier
C   TRIFILL by KFH.
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER PIN(50),PY,POUT,STATUS
      INTEGER PG,PH,PPF,PPR,PYD,PT
      INTEGER NQ(50),MQ(50),NV(2,50),NQT(50)
      INTEGER AX(2),AXO(2)
      DOUBLE PRECISION PM,TD,DT,TT,EPS,EPOCH
      CHARACTER*72 TEXT
      LOGICAL FIRST,YES
      NVALS=0
      NGAP=0
      NDAT=0
C
C   Read in the datasets, exiting on a null return or error status
C
      DO I=1,50
         CALL RDIMAG('INPUT',FMT_DP,2,AX,IDIM,PIN(I),STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
C
C         Store the dimensions of each dataset as read.
C
            NV(1,I)=AX(1)
            NV(2,I)=AX(2)
C
C         Add another to the number of blocks.
C
            NB=I
C
C         After the first time it is possible to check out the
C         size of the gap between this block and the previous one.
C
            IF (I.NE.1) THEN
               CALL PER_SIZGAP(%VAL(PIN(I-1)),NV(1,I-1),NV(2,I-1),
     :                        %VAL(PIN(I)),NV(1,I),NV(2,I),NGAP)
               MQ(I)=NQ(I-1)+NGAP
               NVALS=NVALS+NGAP
            ELSE
C
C            However the first time around we set the initial gap
C            pointer to before the first data block and pick out
C            the first epoch of the first block, for use in creating
C            epochs for the output dataset.
C
               MQ(1)=0
               CALL PER_FRSTEP(%VAL(PIN(1)),NV(1,1),NV(2,1),EPOCH)
            END IF
C
C         NVALS counts the total of input points and points needed
C         to fill all the gaps.
C
            NVALS = NVALS + AX(2)
C
C         NDAT counts only the fixed input data samples
C
            NDAT=NDAT+AX(2)
            NQT(I)=NDAT
C
C         NQ(i) points to the last sample in the i'th dataset.
C
            NQ(I)=NVALS
            CALL CNPAR('INPUT',STATUS)
         ELSE
C
C         This is the escape from the input loop.
C
            CALL CNPAR('INPUT',STATUS)
            GO TO 100
         END IF
      END DO
C
C   That completes data entry - now store the values in a single
C   array, leaving enough space for the gap-fillers.
C
  100 CONTINUE
      MQ(NB+1)=NVALS
C
C   The input + gaps are to be stored in Y; the results will
C   also appear there.
C
      CALL GETDYN('Y',FMT_DP,NVALS,PY,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
C
C   We also need to store the data with no space left for the gaps,
C   for the first call to LABMEMPF
C
      CALL GETDYN('YD',FMT_DP,NDAT,PYD,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
      CALL GETDYN('PT',FMT_DP,NVALS,PT,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
      NVMIN=NV(2,1)
      NFRST=0
      NFRSTA=0
C
C   Loop through the blocks.
C
      DO I=1,NB
         AX(1)=NV(1,I)
         AX(2)=NV(2,I)
C
C      Store the values from the current block, throwing the
C      epochs away.
C
         CALL PER_ADDSET(%VAL(PIN(I)),AX,%VAL(PY),NVALS,NFRST,DT)
C
C      NFRST is used to point to the end of the current data;
C      this sets it to the end of the current GAP.
C
         NFRST=MQ(I+1)
C
C      It is also necessary to keep the data with no gaps.
C
         CALL PER_ADDSET(%VAL(PIN(I)),AX,%VAL(PYD),NDAT,NFRSTA,TT)
C
C      NFRSTA points to the end of this data set in this array.
C
         NFRSTA=NFRSTA+AX(2)
C
C      It is necessary to ensure that the datasets were sampled
C      at the same intervals in time.
C
         IF (I.EQ.1) THEN
            TD=DT
         ELSE
            IF ( ABS(TD-DT).GT.1.0D-4 ) THEN
               CALL WRUSER('Time intervals do not agree',STATUS)
               CALL FRDATA(' ',STATUS)
               CALL EXIT
            END IF
         END IF
         WRITE (TEXT,900) I,NV(2,I) 
  900    FORMAT(1H ,'Block ',I2,' has ',I5,' samples')
         CALL WRUSER(TEXT,STATUS)
         IF (I.NE.NB) THEN
            WRITE (TEXT,905) MQ(I+1)-NQ(I)
  905       FORMAT (1H ,'followed by a gap of ',I5,' samples.')
            CALL WRUSER(TEXT,STATUS)
         END IF
C
C      We also keep track of the length of the shortest block
C      of data, because the AR model must be shorter than that.
C
         IF (NV(2,I).LT.NVMIN) NVMIN=NV(2,I)
      END DO
C
C   Handle the exceptional case of no gaps!
C
      IF (NB.LE.1) THEN
         CALL WRUSER('No gaps present',STATUS)
         CALL FRDATA(' ',STATUS)
         CALL EXIT
      END IF
      M=NVMIN
C
C   Pick up a tolerance for the convergence of the gap-filling
C   operation, and a maximum number of iterations. It will fail
C   if it does not reach the required tolerence after the
C   maximum allowed number of iterations.
C
      CALL PER_GETEPS(%VAL(PY),NVALS,EPS)
      IF (EPS.LE.0.0D0) THEN
         CALL WRUSER('Data have constant values',STATUS)
         CALL FRDATA(' ',STATUS)
         CALL EXIT
      END IF
      NITR=500
      CALL RDKEYI('ITERS',.TRUE.,1,NITR,I,STATUS)
      FIRST=.TRUE.
      WRITE (TEXT,910) NVMIN
  910 FORMAT (1H ,'Maximum lag (M) is ',I4)
      CALL WRUSER(TEXT,STATUS)
C
C   Pick up the number of lags - the length of the AR model.
C
      CALL RDKEYI('M',.TRUE.,1,M,I,STATUS)
C
C   and working arrays.
C
      CALL GETDYN('COFFS',FMT_DP,M,PG,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
      CALL GETDYN('H',FMT_DP,M,PH,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
      MM=M*M
      CALL GETDYN('PF',FMT_DP,MM,PPF,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
      CALL GETDYN('PR',FMT_DP,NVALS,PPR,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
C
C   It returns here after each iteration.
C
  300 CONTINUE
C
C   Fit an AR model to all the data in the blocks
C
      IF (FIRST) THEN
         CALL WRUSER('This may take a little time',ISTAT)
      END IF
         CALL PER_LABMEMPF(M,%VAL(PH),%VAL(PY),NVALS,NB,MQ,NQ,%VAL(PG),
     :                     %VAL(PPF),%VAL(PPR),PM)
      IF (.NOT.FIRST) THEN
         NB=NBT
         NQ(1)=NQ1
      END IF
C
C   Fill the gaps
C
      CALL PER_TRIFILL(%VAL(PY),%VAL(PPR),%VAL(PT),NVALS,
     :                 NB,NQ,MQ,M,%VAL(PH))
C
C   It is now possible to repeat the operation, but fitting the
C   AR model to the input data and the current estimates of the
C   gap fillers.
C
      YES=.TRUE.
      CALL RDKEYL('AGAIN',.TRUE.,1,YES,I,STATUS)
      CALL CNPAR('AGAIN',STATUS)
      IF (YES) THEN
C
C      This sets up the pointers so that the AR fitting will
C      believe that there is only one data block whose length
C      is the full length of the dataset. The original pointers
C      are stored.
C
         NBT=NB
         NQ1=NQ(1)
         NQ(1)=NQ(NB)
         NB=1
         FIRST=.FALSE.
         GO TO 300
      END IF
C
C   At last the results are ready to be output.
C
      AXO(1)=3
      AXO(2)=NVALS
      CALL WRIMAG('OUTPUT',FMT_DP,AXO,2,POUT,STATUS)
      CALL PER_STOP(STATUS,'ERROUT')
      CALL PER_NEWOUT(%VAL(PY),NVALS,EPOCH,TD,%VAL(POUT),AXO)
C
C   Finally tidy up and exit.
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
