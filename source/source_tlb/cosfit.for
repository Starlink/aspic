      PROGRAM COSFIT

C+
C    COSFIT
C
C	part of calibration suite.
C	From known conversions in LEVELS, calculates COSMOS-style
C	intensity conversion lookup table LOOKUP
C	LOOKUP contains true intensities and is indexed by observed
C       values. To get index to desired true intensity, need to convert
C       observed value into 1..NTAB range using TABS+TABF and use
C       that as index.
C	This is for COSMOS data.
C	calls COSFITS to do the work.
C
C    Given (Program Parameter)
C	LEVELS (RA)		array of calibration levels
C	TCLEAR,TBLACK (I)	cosmos tclear,tblack
C	NTAB (I)		length of lookup table
C	TABS,TABF (R)		strart+finish of observed value range
C
C    Returned (program parameters)
C	LOOKUP (RA)		intensity look up table
C	TABS,TABF (R)		min+max for observed values range,
C                               in LOOKUP descriptor
C
C    Work (program parameters)
C	WORK (RA)		work array for drawfit
C
C    D. Tudhope/ROE/Oct 1982
C-

	INTEGER NPLEV,NAXLEV(2),NPTAB,NPWK
	REAL TABS,TABF
	INTEGER TCLEAR,TBLACK,NTAB
	INTEGER ISTATUS
      CHARACTER*20 CTABS,CTABF

      ISTATUS=0
      CALL INPICR('LEVELS','ENTER NAME OF LEVELS FILE',
     :            2,NAXLEV,NPLEV,ISTATUS)
      CALL READI('NTAB',' ',1024,1,10000,NTAB,ISTATUS)
      CALL READI('TCLEAR','enter COSMOS TCLEAR',4000,0,100000,
     :            TCLEAR,ISTATUS)
      CALL READI('TBLACK','enter COSMOS TBLACK',0,-100,TCLEAR,
     :            TBLACK,ISTATUS)
      CALL READR('TABS','enter minimum observed pixel value',
     :           0.0,0.0,1.0E19,TABS,ISTATUS)
      CALL READR('TABF','enter maximum observed pixel value',
     :           TABS+255.0,TABS,1.0E19,TABF,ISTATUS)
      CALL OUTPICR('LOOKUP','ENTER NAME FOR LOOK UP TABLE',
     :              1,NTAB,NPTAB,ISTATUS)
      CALL OUTPICR('WORK',' ',1,NTAB,NPWK,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        CALL COSFITS(NAXLEV(1),%VAL(NPLEV),TCLEAR,TBLACK,TABS,TABF,
     :               NTAB,%VAL(NPWK),%VAL(NPTAB))
C*  output TABS,TABF as descriptor items
        WRITE(CTABS,'(F20.5)') TABS
        WRITE(CTABF,'(F20.5)') TABF
        CALL WRDSCR('LOOKUP','TABS',CTABS,1,ISTATUS)
        CALL WRDSCR('LOOKUP','TABF',CTABF,1,ISTATUS)
        IF (ISTATUS.NE.0) CALL WRERR('MISWRITE')
      ENDIF
      END
