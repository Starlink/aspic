C+
C  program RECFILP
C
C    sets pixels inside a rectangle to newval
C    rectangle specified by opposite corners (XO,YO) (XT,YT)
C    first copies INPICT to OUTPICT and then modifies OUTPICT
C    by calling RECFILSUB
C
C  Given (program parameters)
C    INPICT (RA)   image to be modified
C    XO,YO   (R)   corner of rectangle
C    XT,YT   (R)   corner of rectangle
C    NEWVAL  (R)   new value for area inside rectangle
C
C  Returned (program parameters)
C    OUTPICT  (RA)  modified image
C
C  D. Brownrigg, D. Tudhope  ROE  1981
C-

      REAL XO,YO,XT,YT,NEWVAL
      INTEGER NAXIN(2),NAXOUT(2),NPIN,NPOUT,ISTATUS

      ISTATUS=0
      CALL INPICR('INPICT','GIVE INPUT IMAGE',2,NAXIN,NPIN,ISTATUS)
      NAXOUT(1)=NAXIN(1)
      NAXOUT(2)=NAXIN(2)
      CALL OUTPICR('OUTPICT','GIVE OUTPUT IMAGE',2,NAXOUT,NPOUT,ISTATUS)
      CALL READR('XO','GIVE X-LEFT',0.0,0.0,1000000.0,XO,ISTATUS)
      CALL READR('YO','GIVE Y-BOTTOM',0.0,0.0,1000000.0,YO,ISTATUS)
      CALL READR('XT','GIVE X-RIGHT',0.0,0.0,1000000.0,XT,ISTATUS)
      CALL READR('YT','GIVE Y-TOP',0.0,0.0,1000000.0,YT,ISTATUS)
      CALL READR('NEWVAL','NEW VALUE',0.0,-1.0E19,1.0E19,NEWVAL,ISTATUS)
      IF (ISTATUS.NE.0) THEN
         CALL WRERR('MISREAD')
      ELSE

      CALL COPY(NAXIN(1),NAXIN(2),%VAL(NPIN),
     &     NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
      CALL RECFILSUB(XO,YO,XT,YT,NEWVAL,NAXOUT(1),NAXOUT(2),%VAL(NPOUT))

      ENDIF
      END
