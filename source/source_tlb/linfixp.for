C+
C  program LINFIXP
C
C    modifies indicated horizontal line (CY) to value NEWVAL
C    expected that CY will be given by ARGSCUR_CY
C    first copies INPICT to OUTPICT and then modifies OUTPICT
C    by calling LINFIXSUB
C
C  Given (program parameters)
C    INPICT (RA)   image to be modified
C    CY      (R)   coord of line to be altered
C    NEWVAL  (R)   new value of line
C
C  Returned (program parameters)
C    OUTPICT  (RA)  modified image
C
C  D. Brownrigg, D. Tudhope  ROE  1981
C-

      REAL CY,NEWVAL
      INTEGER NAXIN(2),NAXOUT(2),NPIN,NPOUT,ISTATUS

      ISTATUS=0
      CALL INPICR('INPICT','GIVE INPUT IMAGE',2,NAXIN,NPIN,ISTATUS)
      NAXOUT(1)=NAXIN(1)
      NAXOUT(2)=NAXIN(2)
      CALL OUTPICR('OUTPICT','GIVE OUTPUT IMAGE',2,NAXOUT,NPOUT,ISTATUS)
      CALL READR('CY','GIVE Y-COORD',0.0,0.0,1000000.0,CY,ISTATUS)
      CALL READR('NEWVAL','NEW VALUE',0.0,-1.0E19,1.0E19,NEWVAL,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        CALL COPY(NAXIN(1),NAXIN(2),%VAL(NPIN),
     &     NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
        CALL LINFIXSUB(CY,NEWVAL,NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
      ENDIF
      END
