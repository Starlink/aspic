      SUBROUTINE PLOT
C+
C   P L O T
C
C   Plots an image to the ARGS
C    scales up or down to fit whole image onto args screen (or larger dim if not square)
C    thus images not 512 square are handled, by calling generalised plot routine, PLOTS
C    if scaling is involved coords returned from the cursor will only be approx.
C    and should not be used for accurate work - when a 1-1 window should be used.
C    when scaling down, averaging not done properly, - only roughly over a 2*2 area (to be quick)
C
C   Subroutines called :
C
C	E2DLIB	:	AUTRNG,PLOTS
C	E2DFACE	:	CLEARIM,INPICR,OUTPICI2,READI,
C			READR,WRITER
C	STARLINK:	WRUSER
C
C	B.D.Kelly/ROE/1981
C	B.V. McNally/ROE/JUN/1982
C       D. Tudhope/ROE/JUL/1982
C-

      INTEGER NAXIS(2),NPTR1,NAXWK(2),NPWK,IST,ISTAT,NAUTO
      REAL TZEROD,TMAXD
C*  to hold screen dims, as image may not be square and aspect ratio is preserved
      REAL VDX,VDY
      REAL XEXT,YEXT

      IST=0
      CALL INPICR('INPIC1','GIVE IMAGE FRAME',2,NAXIS,NPTR1,IST)
      CALL READI('NAUTO_','GIVE AUTOSCALE FLAG',-1,0,1,NAUTO,IST)
      IF(IST.EQ.0) THEN
        IF(NAUTO.EQ.0) THEN
          CALL AUTRNG(NAXIS(1),NAXIS(2),%VAL(NPTR1),TZEROD,TMAXD)
        ELSE
          CALL CNPAR('TZEROD_',IST2)
          CALL READR('TZEROD_','GIVE LOWEST VALUE TO BE PLOTTED',
     :               -1.0E20,-1.0E19,1.0E19,TZEROD,IST)
          CALL CNPAR('TMAXD_',IST2)
          CALL READR('TMAXD_','GIVE HIGHEST VALUE TO BE PLOTTED',
     :               -1.0E20,-1.0E19,1.0E19,TMAXD,IST)
        ENDIF
C*  write back to environment to help a possible APLOTRNG later
        CALL WRITER('TZEROD_',TZEROD,IST)
        CALL WRITER('TMAXD_',TMAXD,IST)

C  Obtain a work array from OUTPICI2. The routine PLOTS will use
C  the work array as an INTEGER*2 array

C*  cos inpict may not be 512*512
        NAXWK(1)=512
        NAXWK(2)=512
        CALL OUTPICI2('WORK1','WORK ARRAY',2,NAXWK,NPWK,IST)

C*  get correct aspect ratio for inpict with larger side fitting into 512
        XEXT=REAL(NAXIS(1))
        YEXT=REAL(NAXIS(2))
        IF (XEXT.GE.YEXT) THEN
C*  coords in range 0.0 ..... 511.0
          VDX=511.0
          VDY=512.0*YEXT/XEXT - 1.0
        ELSE
          VDY=511.0
          VDX=512.0*XEXT/YEXT - 1.0
        ENDIF
        CALL PLOTS(0.0,XEXT-1.0,0.0,YEXT-1.0,NAXIS(1),NAXIS(2),
     :      %VAL(NPTR1),TZEROD,TMAXD,0.0,VDX,0.0,VDY,NAXWK(1),NAXWK(2),
     :      %VAL(NPWK),IST)
        CALL CLEARIM('WORK1')
        CALL WRITER('TZEROD_',TZEROD,IST)
        CALL WRITER('TMAXD_',TMAXD,IST)
      ELSE
        CALL WRUSER('ERROR IN PLOT',ISTAT)
      ENDIF
      CALL CLEARIM('INPIC1')
      END
