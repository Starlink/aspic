      PROGRAM SLAFIT

C+
C    SLAFIT
C
C	part of calibration suite.
C	From known conversions in LEVELS, calculates intensity
C	conversion lookup table LOOKUP using a slalom fit devised
C	by John Cooke (UOE). This 'slalom' fit attempts to give an
C	'eye' fit to a fairly small number of points on a fairly
C	smooth curve. It was developed for fitting to the flux
C	sensitivity curve in ESP.
C	LOOKUP contains true intensities and is indexed by observed
C	values. To get index to desired true intensity, need to
C	convert observed value into 1..NTAB range using TABS+TABF
C	and use that as index.
C	calls SLAFITS to do the work.
C
C    Given (Program Parameter)
C	LEVELS (RA)		array of calibration levels
C	NTAB (I)		length of lookup table
C
C    Returned (program parameters)
C	LOOKUP (RA)		intensity look up table
C	TABS,TABF (R)		min+max for observed values range,
C				in LOOKUP descriptor
C
C    Work (program parameters)
C	WORK (RA)		work array for slalom in slafits
C	WORK2 (RA)		work array for sorted levels in SLAFITS
C
C    D. Tudhope/ROE/Mar 1983
C-

	INTEGER NPLEV,NAXLEV(2),NPTAB,NPWK,NAXZK(2),NPZK
	REAL TABS,TABF
	INTEGER NTAB
	INTEGER ISTATUS
        CHARACTER*20 CTABS,CTABF

      ISTATUS=0
      CALL INPICR('LEVELS','ENTER NAME OF LEVELS FILE',
     :            2,NAXLEV,NPLEV,ISTATUS)
      CALL READI('NTAB',' ',1024,1,10000,NTAB,ISTATUS)
      CALL READR('TABS','enter minimum observed pixel value',
     :           0.0,0.0,1.0E19,TABS,ISTATUS)
      CALL READR('TABF','enter maximum observed pixel value',
     :           TABS+255.0,TABS,1.0E19,TABF,ISTATUS)
      CALL OUTPICR('LOOKUP','ENTER NAME FOR LOOK UP TABLE',
     :              1,NTAB,NPTAB,ISTATUS)
      CALL OUTPICR('WORK',' ',1,NTAB,NPWK,ISTATUS)
      NAXZK(1)=NAXLEV(1)
      NAXZK(2)=NAXLEV(2)
      CALL OUTPICR('WORK2',' ',2,NAXZK,NPZK,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        CALL SLAFITS(NAXLEV(1),%VAL(NPLEV),%VAL(NPZK),TABS,TABF,NTAB,
     :              %VAL(NPWK),%VAL(NPTAB))
C*  output TABS,TABF as descriptor items
        WRITE(CTABS,'(F20.5)') TABS
        WRITE(CTABF,'(F20.5)') TABF
        CALL WRDSCR('LOOKUP','TABS',CTABS,1,ISTATUS)
        CALL WRDSCR('LOOKUP','TABF',CTABF,1,ISTATUS)
        IF (ISTATUS.NE.0) CALL WRERR('MISWRITE')
      ENDIF
      END
