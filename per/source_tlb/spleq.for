      PROGRAM SPLEQ
C+
C
C   Program SPLEQ
C
C   This resamples a set of irregularly spaced data at equal
C   intervals, to prepare for analysis by the applications
C   which require such data (MEMPOW, FILLGAP etc.)
C
C   It works by fitting a spline to the data and then using
C   the fit to interpolate new values. The flexibility of
C   the spline fit is defined by the number of knots specified -
C   the larger this number the more wiggles in the curve, and
C   the closer it is to passing through every input data sample.
C
C   Written by C.D.Pike
C   (Tidied by K.F.Hartley at RGO on 1-Jan-1984
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER PIN1,AXES(2),STATUS,POUT,AXOUT(2)
      CHARACTER*72 TEXT
  100 CONTINUE
C
C   Obtain an input dataset
C
      CALL RDIMAG('INPUT',FMT_DP,2,AXES,I,PIN1,STATUS)
C
C   Stop and exit on null or error
C
      IF (STATUS.NE.ERR_NORMAL) GO TO 999
C
C   Obtain workspace
C
      NPMAX=AXES(2)
      CALL WRUSER('Enter maximum possible number of output samples',
     :            STATUS)
      CALL RDKEYI('MAXNUM',.TRUE.,1,NPMAX,I,STATUS)
      CALL GETDYN('X',FMT_DP,NPMAX,IPX,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('DATA',FMT_DP,NPMAX,IPD,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('XS',FMT_DP,NPMAX,IPXS,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('YAPR',FMT_DP,NPMAX,IPY,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('TEMP',FMT_R,NPMAX,IPT,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C   Call this subroutine to do all the hard work
C
      CALL PER_PANDF(%VAL(PIN1),AXES,%VAL(IPX),%VAL(IPD),%VAL(IPXS),
     :                %VAL(IPY),%VAL(IPT),NPMAX,NPTSOUT)
C
C   Obtain an output dataset
C
      AXOUT(1) = 3
      AXOUT(2) = NPTSOUT
      CALL WRIMAG('OUTPUT',FMT_DP,AXOUT,2,POUT,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
         CALL PER_FITOUT(%VAL(POUT),AXOUT,%VAL(IPXS),%VAL(IPY))
         TEXT=' '
         CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
         CALL WRDSCR('OUTPUT','TITLE',TEXT,1,STATUS)
      ELSE
         CALL WRERR('HELLOUT',STATUS)
      END IF
      CALL CNPAR('INPUT',STATUS)
      CALL CNPAR('MAXNUM',STATUS)
      CALL CNPAR('X',STATUS)
      CALL CNPAR('DATA',STATUS)
      CALL CNPAR('XS',STATUS)
      CALL CNPAR('YAPR',STATUS)
      CALL CNPAR('TEMP',STATUS)
      CALL CNPAR('OUTPUT',STATUS)
      CALL FRDATA(' ',STATUS)
C
C   Go back for another dataset.
C
      GO TO 100
  999 CONTINUE
C
C   Close Simpleplot and exit
C
      CALL ENDPLT
      CALL EXIT
      END
