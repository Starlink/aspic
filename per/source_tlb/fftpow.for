      PROGRAM FFTPOW
C+
C
C      Program FFTPOW 
C
C   This program computes the power spectrum of an equally
C   spaced data set, using the NAG routine C06FAF to compute
C   the Fourier Transform. There is no choice of frequency
C   range and the window function is meaningless for equally
C   spaced data. Only weak restrictions are placed on the number
C   of samples which can be processed - it does not have to be a
C   power of 2. The dataset has zeroes added if it would fail!
C
C   The output is in the same format as that from the more generally
C   applicable program POWER.
C
C   Written by K.F.Hartley at RGO on 7-2-84
C
C-
      INTEGER PIN,PY,PW,PPOWD
      INTEGER AX(2),AXO(2),STATUS
      DOUBLE PRECISION T
      CHARACTER*72 TITLE
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C      First get input data
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      CALL PER_STOP(STATUS,'ERRIN')
C
C      We ought to verify that AX(2) is an acceptable
C      number of samples for this algorithm
C      (See NAG manual Vol 1, C06FAF section 5)
C      In either case NF is the number of samples to be transformed,
C      padding with zeros if the original length was not acceptable.
C
      CALL PER_CHECK(AX(2),NF)
C
C      Then get space to store the values  (double precision )
C
      CALL GETDYN('Y',FMT_DP,NF,PY,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C      and some workspace
C
      CALL GETDYN('W',FMT_DP,NF,PW,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C      Now copy data into Y
C
      CALL PER_COPYY(%VAL(PIN),%VAL(PY),AX(1),AX(2),NF,T)
C
C      Now obtain output dataset
C
      AXO(1)=2
C
C   Note that the number of frequencies is only half that of the
C   the augmented dataset.
C
      AXO(2)=NF/2
      CALL WRIMAG('POWER',FMT_DP,AXO,2,PPOWD,STATUS)
      CALL PER_STOP(STATUS,'ERROUT')
C
C     Now we are able to do something useful -
C     compute the Fourier Transform.
C
      CALL PER_FFT(%VAL(PY),%VAL(PW),NF)
C
C    Convert the Fourier transforms into Power Spectra
C
      CALL PER_FPOWR(%VAL(PY),AX(2),%VAL(PPOWD),AXO(2),T)
C
C    Tidy up and go home
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
