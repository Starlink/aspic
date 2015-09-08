      SUBROUTINE PLOTQ
C+
C   P L O T Q
C
C   Plots an image to a quadrant of the ARGS (similar PLOT but to a quadrant)
C    scales up or down to fit whole image onto quadrant (or larger dim if not square)
C    thus images not 512 square are handled, by calling generalised plot routine, PLOTS
C    if scaling is involved coords returned from the cursor will only be approx.
C    and should not be used for accurate work - when a 1-1 window should be used.
C    when scaling down, averaging not done properly, - only roughly over a 2*2 area (to be quick)
C
C   Subroutines called :
C
C	E2DLIB	:	AUTRNG,PLOTS,QUAD2
C	E2DFACE	:	CLEARIM,INPICR,OUTPICI2,READI,
C			READR,WRITER
C	STARLINK:	WRUSER
C
C	B.D.Kelly/ROE/1981
C	B.V. McNally/ROE/JUN/1982
C       D. Tudhope/ROE/JUL/1982
C-

      INTEGER NAXIS(2),NPTR1,NAXWK(2),NPWK,IST,ISTAT,NAUTO,IXR,IYR,IDUM
      REAL TZEROD,TMAXD
C*  dims of args pict - as inpict may not be square and so only one dim will be 255
      REAL VDX,VDY
C*  coords of bottom left hand corner of quadrant
      REAL VXO,VXT
      REAL XEXT,YEXT

      IST=0
      CALL INPICR('INPIC1','GIVE IMAGE FRAME',2,NAXIS,NPTR1,IST)
      CALL READI('NAUTO_','GIVE AUTOSCALE FLAG',-1,0,1,NAUTO,IST)

      IF(IST.EQ.0) THEN
        IF(NAUTO.EQ.0) THEN
          CALL AUTRNG(NAXIS(1),NAXIS(2),%VAL(NPTR1),TZEROD,TMAXD)
        ELSE
          CALL READR('TZEROD_','GIVE LOWEST VALUE TO BE PLOTTED',
     :               -1.0E20,-1.0E19,1.0E19,TZEROD,IST)
          CALL READR('TMAXD_','GIVE HIGHEST VALUE TO BE PLOTTED',
     :               -1.0E20,-1.0E19,1.0E19,TMAXD,IST)
        ENDIF
C  Obtain a work array from OUTPICI2. The routine PLOTS will use
C  the work array as an INTEGER*2 array
        NAXWK(1)=256
        NAXWK(2)=256
        CALL OUTPICI2('WORK1','WORK ARRAY',2,NAXWK,NPWK,IST)
C*  get correct aspect ratio for inpict with larger side fitting into 256
        XEXT=REAL(NAXIS(1))
        YEXT=REAL(NAXIS(2))
        IF (XEXT.GE.YEXT) THEN
C*  coords in range 0.0 ..... 255.0
          VDX=255.0
          VDY=256.0*YEXT/XEXT - 1.0
        ELSE
          VDY=255.0
          VDX=256.0*XEXT/YEXT - 1.0
        ENDIF
C*  get quadrant to plot in, (vxo,vyo) is bottom left hand corner
        CALL QUAD(IXR,IYR,IDUM)
        VXO=IXR
        VYO=IYR
        CALL PLOTS(0.0,XEXT-1.0,0.0,YEXT-1.0,NAXIS(1),NAXIS(2),
     :      %VAL(NPTR1),TZEROD,TMAXD,VXO,VXO+VDX,VYO,VYO+VDY,
     :      NAXWK(1),NAXWK(2),%VAL(NPWK),IST)
        CALL CLEARIM('WORK1')
        CALL WRITER('TZEROD_',TZEROD,IST)
        CALL WRITER('TMAXD_',TMAXD,IST)
      ELSE
        CALL WRUSER('ERROR IN PLOT',ISTAT)
      ENDIF
      CALL CLEARIM('INPIC1')
      END
