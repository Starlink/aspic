      PROGRAM SETLEV

C+
C    SETLEV
C
C	part of calibration suite.
C	enter known calibration levels from .kb
C	entered as VALUE,INTENSITY terminating with "END"
C	and with INTENSITY>=0
C	outputs LEVELS file with calib. levels
C	copies LEVS-->LEVELS to get output array exact size
C
C    Given (.KB)
C	observed values and TRUE intensities
C    Returned (program parameter)
C	LEVELS (RA)	array of .kb input
C
C    D. Tudhope/ROE/Feb 1983
C-

C*  maximum size of LEVELS array
      INTEGER MAXLEVS
      PARAMETER (MAXLEVS=50)
C*  number of columns in LEVELS (must be <MAXKEY in READROW) - here 2 for VALUE,INTENSITY
      INTEGER NCOLS
      PARAMETER (NCOLS=2)
      REAL LEVS(MAXLEVS,NCOLS)
      INTEGER NLEVS,NPLEV,NAXLEV(2),ISTATUS

      CALL WRUSER('enter known calibration levels as pairs,',ISTATUS)
      CALL WRUSER('OBSERVED VALUE,TRUE INTENSITY,',ISTATUS)
      CALL WRUSER('terminating with "END".',ISTATUS)

      CALL READTAB(MAXLEVS,NCOLS,LEVS,NLEVS)
      IF (NLEVS.GE.MAXLEVS) THEN
        CALL WRERR('TOOMANY')
      ELSE
        IF (NLEVS.EQ.0) THEN
          CALL WRERR('TOOFEW')
        ELSE
          NAXLEV(1)=NLEVS
          NAXLEV(2)=NCOLS
          CALL OUTPICR('LEVELS','ENTER NAME OF LEVELS FILE',
     :                  2,NAXLEV,NPLEV,ISTATUS)
          IF (ISTATUS.NE.0) THEN
            CALL WRERR('MISWRITE')
          ELSE
            CALL COPY(MAXLEVS,NCOLS,LEVS,NLEVS,NCOLS,%VAL(NPLEV))
          ENDIF
        ENDIF
      ENDIF
      END
