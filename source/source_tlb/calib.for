      PROGRAM CALIB
C+
C   CALIB
C
C	part of calibration suite
C       applies intensity conversion from lookup table LOOKUP
C	changing input image of observed values to output image
C       of 'true intensities' assumed that previously one of
C       the calibration suite of programs has been run to
C       generate LOOKUP.
C
C    Given (program parameters)
C	INPIC1  (RA)	input image of observed values
C	LOOKUP  (RA)	look up table for intensity conversion
C	TABS,TABF (R)	min and max of conversion - from header
C                       of LOOKUP
C
C    Returned (program parameters)
C	OUTPIC1 (RA)	output image after intensity conversion
C
C     [JAC]/UOE/1981
C     D. Tudhope/ROE/Nov 1982
C-

      REAL TABF,TABS
      INTEGER NAXIS(2),NPTR1,NPTR2,IST
      INTEGER NTAB,NPTAB
      CHARACTER*20 CTABS,CTABF

      IST=0

      CALL INPICR('INPIC1','GIVE INPUT IMAGE',2,NAXIS,NPTR1,IST)
      CALL OUTPICR('OUTPIC1','GIVE OUTPUT IMAGE',2,NAXIS,NPTR2,IST)
      CALL INPICR('LOOKUP','GIVE LOOKUP TABLE FOR INTENSITY CONVERSION',
     &            1,NTAB,NPTAB,IST)
C*  get from header TABS,TABF
      CALL RDDSCR('LOOKUP','TABS',1,CTABS,JDUM,IST)
      CALL RDDSCR('LOOKUP','TABF',1,CTABF,JDUM,IST)
      READ(CTABS,'(BN,F20.0)') TABS
      READ(CTABF,'(BN,F20.0)') TABF

      IF (IST.EQ.0.) THEN
        CALL CALIBS(NAXIS(1),NAXIS(2),%VAL(NPTR1),TABS,TABF,NTAB,
     :              %VAL(NPTAB),%VAL(NPTR2))
      ELSE
        CALL WRERR('MISREAD')
      ENDIF

      CALL CLEARIM('INPIC1')
      CALL CLEARIM('OUTPIC1')
      CALL CLEARIM('LOOKUP')

      END
