C+
C  program SYMDISP
C
C    Remove 4 symmetrically placed disk-shaped areas from an fft image
C    given centre (CX,CY) and diameter (DIAM) of 1 area (from ARGSCIR ?),
C    compute other 3 by reflection in lines x=ixext/2-1 and y=iyext/2.
C    fft images assumed to have been produced by DFFTASP as here zero
C    frequency is assumed to lie at IXEXT/2-1,IYEXT/2 in args coords 0..511
C    This is for removing noise that appears as 4 symmetric clusters,
C    one in each quadrant of fft image.
C    Call CIRFIL to fill in areas with NEWVAL.
C
C  Given (program parameters)
C    INPICT (RA)   image to be modified
C    CX,CY   (R)   coords of centre of 1st circle
C    DIAM    (R)   diameter of 1st circle
C    NEWVAL  (R)   new value for area inside circle
C
C  Returned (program parameters)
C    OUTPICT  (RA)  modified image
C
C  D. Brownrigg, D. Tudhope  ROE  1981
C-

      REAL CX,CY,DIAM,NEWVAL
      INTEGER NAXIN(2),NAXOUT(2),NPIN,NPOUT,ISTATUS
      REAL X1,Y1,X2,Y2,RAD

      ISTATUS=0
      CALL INPICR('INPICT','GIVE INPUT IMAGE',2,NAXIN,NPIN,ISTATUS)
      NAXOUT(1)=NAXIN(1)
      NAXOUT(2)=NAXIN(2)
      CALL OUTPICR('OUTPICT','GIVE OUTPUT IMAGE',2,NAXOUT,NPOUT,ISTATUS)
      CALL READR('CX','GIVE X-CENTRE',0.0,0.0,1000000.0,CX,ISTATUS)
      CALL READR('CY','GIVE Y-CENTRE',0.0,0.0,1000000.0,CY,ISTATUS)
      CALL READR('DIAM','GIVE DIAMETER',0.0,0.0,1000000.0,DIAM,ISTATUS)
      CALL READR('NEWVAL','NEW VALUE',0.0,-1.0E19,1.0E19,NEWVAL,ISTATUS)
      IF (ISTATUS.NE.0) THEN
         CALL WRERR('MISREAD')
      ELSE
        CALL COPY(NAXIN(1),NAXIN(2),%VAL(NPIN),
     &     NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
C*  get reflections of CX,CY (remembering CIRFIL will add 1 to x,y for array coords)
        X1=CX
        X2=NAXOUT(1) - X1 - 2.0
        Y1=CY
        Y2=NAXOUT(2) - Y1 - 1.0
        RAD=DIAM/2
        CALL CIRFIL(X1,Y1,RAD,NEWVAL,NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
        CALL CIRFIL(X1,Y2,RAD,NEWVAL,NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
        CALL CIRFIL(X2,Y1,RAD,NEWVAL,NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
        CALL CIRFIL(X2,Y2,RAD,NEWVAL,NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
      ENDIF
      END
