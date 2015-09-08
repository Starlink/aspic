      PROGRAM EDITTAB

C+
C    EDITTAB
C
C	part of general 2-d table package
C	edit existing TABLE interactively from keyboard
C
C    Given (program parameter)
C	TABOLD (RA)     original TABLE array
C    Returned (program parameter)
C	TABLE (RA)
C    Work (program parameters)
C	WORK (RA)	intermediate table
C
C    D. Tudhope/ROE/April 1983
C-

C*  maximum number of rows allowed in table
      INTEGER MAXTABS
      PARAMETER (MAXTABS=50)
C*  number of columns in table
      INTEGER NCOLS
      INTEGER NPOLD,NAXOLD(2),NTABS,NPTAB,NAXTAB(2),NPWK,NAXWK(2)
      INTEGER ISTATUS

      ISTATUS=0
      CALL INPICR('TABOLD','ENTER NAME OF OLD TABLE FILE TO BE EDITED',
     :            2,NAXOLD,NPOLD,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
       NCOLS=NAXOLD(2)
       NAXWK(1)=MAXTABS
       NAXWK(2)=NCOLS
       CALL OUTPICR('WORK',' ',2,NAXWK,NPWK,ISTATUS)
       IF (ISTATUS.EQ.0) THEN
       CALL EDITTABS(NAXOLD(1),NCOLS,%VAL(NPOLD),
     :               MAXTABS,%VAL(NPWK),NTABS)
       IF (NTABS.GT.0) THEN
         NAXTAB(1)=NTABS
         NAXTAB(2)=NCOLS
         CALL OUTPICR('TABLE','ENTER NAME OF NEW TABLE FILE',
     :                 2,NAXTAB,NPTAB,ISTATUS)
         IF (ISTATUS.NE.0) THEN
           CALL WRERR('MISWRITE')
         ELSE
           CALL COPY(MAXTABS,NCOLS,%VAL(NPWK),NTABS,NCOLS,%VAL(NPTAB))
         ENDIF
       ENDIF
       ENDIF
      ENDIF
      END
