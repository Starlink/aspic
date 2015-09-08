      PROGRAM GETLEV

C+
C    GETLEV
C
C	part of calibration suite.
C	enter known calibration levels by args box cursor on a
C	stepwedge frame, assumed that a stepwedge has previously
C	been drawn on args, type in original intensity and get
C	observed value from mean of box on stepwedge
C	calls GETLEVS to do the work.
C	outputs LEVELS file with calib. levels
C	copies LEVS-->LEVELS to get output array exact size
C
C    Given (Program Parameter)
C	WEDGE (RA)		stepwedge image frame
C    Returned (program parameter)
C	LEVELS (RA)		array of calibration levels
C
C    D. Tudhope/ROE/Oct 1982
C-

      INTEGER NAXWED(2),NPWED
      INTEGER MAXLEVS
      PARAMETER (MAXLEVS=50)
      REAL LEVS(MAXLEVS,2)
      INTEGER NLEVS,NPLEV,NAXLEV(2),ISTATUS

      ISTATUS=0
      CALL INPICR('WEDGE','ENTER NAME OF STEPWEDGE',
     :            2,NAXWED,NPWED,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        CALL GETLEVS(NAXWED(1),NAXWED(2),%VAL(NPWED),MAXLEVS,LEVS,NLEVS)
        IF (NLEVS.GE.MAXLEVS) THEN
          CALL WRERR('TOOMANY')
        ELSE
          NAXLEV(1)=NLEVS
          NAXLEV(2)=2
          CALL OUTPICR('LEVELS','ENTER NAME OF LEVELS FILE',
     :                  2,NAXLEV,NPLEV,ISTATUS)
          IF (ISTATUS.NE.0) THEN
            CALL WRERR('MISWRITE')
          ELSE
            CALL COPY(MAXLEVS,2,LEVS,NLEVS,2,%VAL(NPLEV))
          ENDIF
        ENDIF
      ENDIF
      END
