      PROGRAM EDITLEV

C+
C    EDITLEV
C
C	part of calibration suite.
C	edit existing LEVELS interactively from keyboard to
C	produce a new LEVELS table with calib. levels
C	by editing a row (ie a VALUE,INTENSITY pair) at a time.
C	note INTENSITY>=0.0
C	copies LEVS-->LEVELS to get output array exact size
C
C    Given (program parameter)
C	LEVOLD (RA)     original LEVELS array
C	observed values and original intesnities
C    Returned (program parameter)
C	LEVELS (RA)	array of edited levels
C
C    D. Tudhope/ROE/Oct 1982
C-

C*  maximum number of rows allowed in table
      INTEGER MAXLEVS
      PARAMETER (MAXLEVS=50)
C*  number of columns in table - <=MAXKEY IN RDROW = here 2 for VALUE,INTENSITY
      INTEGER NCOLS
      PARAMETER (NCOLS=2)
C*  intermediate levels array
      REAL LEVS(MAXLEVS,NCOLS)
      INTEGER NPOLD,NAXOLD(2),NLEVS,NPLEV,NAXLEV(2),ISTATUS

      ISTATUS=0
      CALL INPICR('LEVOLD','ENTER NAME OF OLD LEVELS FILE TO BE EDITED',
     :            2,NAXOLD,NPOLD,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
       CALL EDITLEVS(NAXOLD(1),NAXOLD(2),%VAL(NPOLD),MAXLEVS,LEVS,NLEVS)
       IF (NLEVS.GT.0) THEN
         NAXLEV(1)=NLEVS
         NAXLEV(2)=NCOLS
         CALL OUTPICR('LEVELS','ENTER NAME OF NEW LEVELS FILE',
     :                 2,NAXLEV,NPLEV,ISTATUS)
         IF (ISTATUS.NE.0) THEN
           CALL WRERR('MISWRITE')
         ELSE
           CALL COPY(MAXLEVS,NCOLS,LEVS,NLEVS,NCOLS,%VAL(NPLEV))
         ENDIF
       ENDIF
      ENDIF
      END
