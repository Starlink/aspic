      PROGRAM LISTLEV

C+
C    LISTLEV
C
C	part of calibration suite.
C	lists levels file on lineprinter
C
C    Given (program parameter)
C	LEVELS (RA)	array of levels
C
C    D. Tudhope/ROE/Oct 1982
C-

      INTEGER NPLEV,NAXLEV(2),ISTATUS

      ISTATUS=0
      CALL INPICR('LEVELS','ENTER NAME OF LEVELS FILE TO BE LISTED',
     :            2,NAXLEV,NPLEV,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
       CALL LISTLEVS(NAXLEV(1),NAXLEV(2),%VAL(NPLEV))
      ENDIF
      END
