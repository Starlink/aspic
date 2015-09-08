C+
C  program HIFREQP
C
C    Eliminates high frequencies from fft mode image
C    fft images should be produced by DFFTASP as this is geared for its output
C    viz zero frequency assumed to lie at IXEXT/2-1,IYEXT/2 in args coords 0..511
C    OUTSETSUB called to put everything outside circle to 0
C    Size of circle derived from DIAM (probly from ARGSCIR,with CX,CY ignored)
C
C  Given (program parameters)
C    INPICT (RA)   image to be modified
C    DIAM    (R)   diameter of circle
C
C  Returned (program parameters)
C    OUTPICT  (RA)  modified image
C
C  D. Brownrigg, D. Tudhope  ROE  1981
C-

      REAL DIAM
      INTEGER NAXIN(2),NAXOUT(2),NPIN,NPOUT,ISTATUS
      REAL XCN,YCN

      ISTATUS=0
      CALL INPICR('INPICT','GIVE INPUT IMAGE',2,NAXIN,NPIN,ISTATUS)
      NAXOUT(1)=NAXIN(1)
      NAXOUT(2)=NAXIN(2)
      CALL OUTPICR('OUTPICT','GIVE OUTPUT IMAGE',2,NAXOUT,NPOUT,ISTATUS)
      CALL READR('DIAM','GIVE DIAMETER',0.0,0.0,1000000.0,DIAM,ISTATUS)
      IF (ISTATUS.NE.0) THEN
         CALL WRERR('MISREAD')
      ELSE
       CALL COPY(NAXIN(1),NAXIN(2),%VAL(NPIN),
     &     NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
       XCN=NAXOUT(1)/2.0 - 1.0
       YCN=NAXOUT(2)/2.0
       CALL OUTSETSUB(XCN,YCN,DIAM,0.0,NAXOUT(1),NAXOUT(2),%VAL(NPOUT))
      ENDIF
      END
