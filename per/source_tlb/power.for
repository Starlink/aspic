      PROGRAM POWER
C+
C
C      Program POWER 
C
C   This program computes the power spectrum and, optionally, the
C   window function of (usually) unequally spaced data.
C
C   It computes it at a user defined number of equally spaced
C   frequencies between user defined limits.
C
C   It uses Chebyshev recursion to compute the trig functions,
C   as described by Bell in "Intro to Fourier Transform Spectroscopy",
C   Academic Press (1970), p234 et seq.
C
C   This version was written by K.F.Hartley at RGO but relies
C   very heavilly on an earlier version by Paul Murdin at RGO.
C
      INTEGER PIN,PX,PY,PCD,PSD,PCW,PSW,PFQ,PPOWD,PPOWW
      INTEGER AX(2),AXO(2),STATUS
      REAL F(2)
      LOGICAL FRE
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C      First get input data
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      CALL PER_STOP(STATUS,'ERRIN')
C
C      Then get work space (double precision )
C
      CALL GETDYN('X',FMT_DP,AX(2),PX,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('Y',FMT_DP,AX(2),PY,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C      Now copy data into X and Y
C
      CALL PER_POCOPY(%VAL(PIN),%VAL(PX),%VAL(PY),AX(1),AX(2))
C
C      Now pick up the range and number of frequencies
C
      CALL RDKEYL('FREQ',.FALSE.,1,FRE,I,STATUS)
      IF (.NOT.FRE) THEN
         CALL WRUSER('Enter range of periods',STATUS)
         CALL WRUSER('But note that the computation will be done'//
     :               ' at equally spaced frequencies',STATUS)
      ELSE
         CALL WRUSER('Enter the range of frequencies',STATUS)
      END IF
  100 CONTINUE
      CALL RDKEYR('RANGE',.FALSE.,2,F,I,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL WRERR('HELLF')
         CALL CNPAR('RANGE',STATUS)
         GO TO 100
      END IF
C
C   If input was in periods, convert to frequencies
C
      IF (.NOT.FRE) THEN
         F(1)=1.0/F(1)
         F(2)=1.0/F(2)
      END IF
C
C   Ensure that the frequencies are in the right order
C
      IF (F(1).GT.F(2)) THEN
         TEMP=F(1)
         F(1)=F(2)
         F(2)=TEMP
      END IF
  200 CONTINUE
      CALL PER_GETNF(%VAL(PX),AX(2),F,NF)
      CALL WRUSER('The default value is a minimum value to',STATUS)
      CALL WRUSER('avoid under sampling for this range of',STATUS)
      CALL WRUSER('of frequencies; other values may be used',STATUS)
      CALL WRUSER('at the users discretion',STATUS)
      CALL RDKEYI('NUMFREQ',.TRUE.,1,NF,I,STATUS)
      IF (STATUS.GT.ERR_PARNULL.AND.NF.LE.1) THEN
         CALL WRERR('HELLNF')
         CALL CNPAR('NUMFREQ',STATUS)
         GO TO 200
      END IF
C
C      Now get working space for the fourier transforms
C
      CALL GETDYN('CD',FMT_DP,NF,PCD,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('SD',FMT_DP,NF,PSD,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('CW',FMT_DP,NF,PCW,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('SW',FMT_DP,NF,PSW,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C      and for the frequencies
C
      CALL GETDYN('FQ',FMT_DP,NF,PFQ,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C      Now decide on the MODE
C
      CALL WRUSER('Now select the mode ',STATUS)
      CALL WRUSER('1 means compute the Power Spectrum',STATUS)
      CALL WRUSER('2 means compute the Window Function',STATUS)
      CALL WRUSER('3 means compute both',STATUS)
      ITYP=1
      CALL RDKEYI('MODE',.TRUE.,1,ITYP,I,STATUS)
C
C      Now obtain output datasets (1 or 2 depending on the mode)
C
      AXO(1)=2
      AXO(2)=NF
      IF (ITYP.EQ.1.OR.ITYP.EQ.3) THEN
         CALL WRIMAG('POWER',FMT_DP,AXO,2,PPOWD,STATUS)
         CALL PER_STOP(STATUS,'ERROUT')
      END IF
      IF (ITYP.EQ.2.OR.ITYP.EQ.3) THEN
         CALL WRIMAG('WINDOW',FMT_DP,AXO,2,PPOWW,STATUS)
         CALL PER_STOP(STATUS,'ERROUT')
      END IF
C
C     Now we are able to do something useful!
C
      CALL PER_SLOWFT(%VAL(PX),%VAL(PY),AX(2),%VAL(PCD),%VAL(PSD),
     :            %VAL(PCW),%VAL(PSW),%VAL(PFQ),F(1),F(2),NF,ITYP)
C
C       Convert the Fourier transforms into Power Spectra
C
      IF (ITYP.EQ.1.OR.ITYP.EQ.3) THEN
         CALL PER_CALCPOW(%VAL(PFQ),%VAL(PCD),%VAL(PSD),%VAL(PPOWD),
     :              AXO(1),AXO(2))
      END IF
      IF (ITYP.EQ.2.OR.ITYP.EQ.3) THEN
         CALL PER_CALCPOW(%VAL(PFQ),%VAL(PCW),%VAL(PSW),%VAL(PPOWW),
     :              AXO(1),AXO(2))
      END IF
C
C      Tidy up and go home
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
