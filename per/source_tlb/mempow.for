      PROGRAM MEMPOW
C+
C
C   Program MEMPOW
C
C   This program computes the power spectrum of "gapped" data. By
C   "gapped" data we mean a dataset which is made up of several
C   (up to 50) sets of data which are themselves sampled at equal
C   (and the same) time intervals but which are separated from each
C   other by a relatively long gap - such as between two nights of
C   regular observation. It creates (a) a power spectrum in the same
C   format as POWER and FFTPOW and (b) a set of coefficients (see
C   below) which can be used by EXTEND.
C
C   The method used is due to Fahlmann and Ulrych and the coding of
C   the key subroutines was kindly supplied by Luis Balona of SAAO.
C   The method involves computation of the coefficients of an
C   auto-regressive model and then using these coefficients to
C   compute the power spectrum. This actually uses a variation
C   on the original algorithm which has been called "bi-directional"
C   because it makes use of the time symmetry of the model.
C
C   Written by K.F.Hartley at RGO on 29-3-84
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER PIN(50),PY,STATUS
      INTEGER PG,PH,PPF,PPR
      INTEGER NQ(50),NV(2,50)
      INTEGER AX(2),AXES(2)
      LOGICAL FRE
      DOUBLE PRECISION PM,TD,DT,EPS,FREQ(2)
      CHARACTER*72 TEXT,TITLE
C
C   Read in the individual datasets, exiting on an error or null
C   response.
C
      NVALS=0
      DO I=1,50
         CALL RDIMAG('INPUT',FMT_DP,2,AX,IDIM,PIN(I),STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            NVALS = NVALS + AX(2)
            NQ(I)=NVALS
            NV(1,I)=AX(1)
            NV(2,I)=AX(2)
            NB=I
            CALL CNPAR('INPUT',STATUS)
         ELSE
            CALL CNPAR('INPUT',STATUS)
            GO TO 100
         END IF
      END DO
C
C   The array NQ stores the position in the final array which will
C   be occupied by the last sample from each of the individual
C   datasets. There are NB of them in all.
C
  100 CONTINUE
C
C   The values (but not the epochs) from the individual datasets are
C   now extracted and stored in a single array Y.
C
      CALL GETDYN('Y',FMT_DP,NVALS,PY,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
      NVMIN=NV(2,1)
      NFRST=0
      DO I=1,NB
         AX(1)=NV(1,I)
         AX(2)=NV(2,I)
         CALL PER_ADDSET(%VAL(PIN(I)),AX,%VAL(PY),NVALS,NFRST,DT)
C
C     It is important that the data sampling interval is the same
C     for each dataset.
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
         NFRST=NFRST+NV(2,I)
         WRITE (TEXT,900) I,NV(2,I) 
  900    FORMAT(1H ,'Block ',I2,' has ',I5,' samples')
         CALL WRUSER(TEXT,STATUS)
         IF (NV(2,I).LT.NVMIN) NVMIN=NV(2,I)
      END DO
C
C   The main limitation of the AR method is that the length of the
C   model (ie the number of coefficients) must be no longer than
C   the shortest run of continuous data - in this case of the shortest
C   individual dataset.
C
      WRITE (TEXT,910) NVMIN
  910 FORMAT (1H ,'Maximum lag (M) is ',I4)
      CALL WRUSER(TEXT,STATUS)
      M=NVMIN
      CALL RDKEYI('M',.TRUE.,1,M,I,STATUS)
C
C   Obtain the .BDF file used to store the coefficients
C
      CALL WRIMAG('COFFS',FMT_DP,M,1,PH,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
C
C   and some workspace.
C
      CALL GETDYN('H',FMT_DP,M,PG,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
C
C   Pick up a tolerance for the convergence of the computation
C   of the bi-directional AR model. It will fail
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
C
C   Now obtain the workspace needed.
C
      MM=M*M
      CALL GETDYN('PF',FMT_DP,MM,PPF,STATUS)
      CALL PER_STOP(STATUS,'ERRWRK')
C
C   This is where the bi-directional auto-regressive model is computed.
C
      CALL PER_LABBIDIR(%VAL(PY),NVALS,NB,NQ,EPS,NITR,%VAL(PG),M,
     :                  %VAL(PPF),%VAL(PH))
C
C   Now make some minor changes to prepare for next stage
C
      M=M-1
      CALL PER_PUTCOF(%VAL(PH),M)
C
C   Now ready to compute the power spectrum......
C
C   ......first decide what is wanted.
C
      PM = 2.0*3.14159*TD
  200 CONTINUE
C
C   Now get the range over which the power spectrum is to be computed
C
      FRE=.TRUE.
      CALL RDKEYL('FREQ',.TRUE.,1,FRE,I,STATUS)
      IF (FRE) THEN
         CALL WRUSER('Enter range of FREQUENCIES',STATUS)
      ELSE
         CALL WRUSER('Enter range of PERIODS',STATUS)
         CALL WRUSER('(But calculations will be done in frequencies',
     :               STATUS)
      END IF
      CALL RDKEYD('RANGE',.FALSE.,2,FREQ,NVAL,STATUS)
      CALL PER_STOP(STATUS,'ERRRAN')
C
C   If periods were entered, convert to frequencies
C
      IF (.NOT.FRE) THEN
         DO I=1,2
            IF (FREQ(I).LE.0.0) THEN
               CALL WRUSER('Periods must be positive',STATUS)
               CALL CNPAR('RANGE',STATUS)
               GO TO 200
            END IF
         END DO
         TEMP=FREQ(1)
         FREQ(1)=1.0/FREQ(2)
         FREQ(2)=1.0/TEMP
      END IF
C
C Convert to pixel frequency steps
C
      FREQ(1) = FREQ(1)*TD
      FREQ(2) = FREQ(2)*TD
C
C   Obtain the number of frequencies
C
      NUMF=1000
      CALL RDKEYI('NUMFREQ',.TRUE.,1,NUMF,I,STATUS)
C
C  Set the step so only calculate NUMF  points
C
      STEP = (FREQ(2)-FREQ(1))/DBLE(NUMF-1)
C
C   Obtain the output dataset
C
      AXES(1)=2
      AXES(2) = NUMF
      CALL WRIMAG('POWER',FMT_DP,AXES,2,POUT,STATUS)
      CALL PER_STOP(STATUS,'ERROUT')
C
C   Compute the power spectrum and store it in out.
C
      CALL PER_MEMPS(%VAL(PH),M,FREQ,STEP,TD,PM,%VAL(POUT),
     :                AXES)
C
C   Finally pick up a title
C
      TITLE=' '
      CALL RDKEYC('TITLE',.TRUE.,1,TITLE,I,STATUS)
      CALL WRDSCR('POWER','TITLE',TITLE,1,STATUS)
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
