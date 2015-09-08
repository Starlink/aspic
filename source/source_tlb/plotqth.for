      PROGRAM PLOTQTH
C+
C   P L O T Q T H
C
C    Thresholds an image and plots it to a quadrant of the args.
C    used with IAM suite of programs.
C    scales up or down to fit whole image onto quadrant (or larger
C    dim if not square) thus images not 512 square are handled, by
C    calling generalised plot routine, PLOTSTH if scaling is involved
C    coords returned from the cursor will only be approx. and should
C    not be used for accurate work - when a 1-1 window should be used.
C    when scaling down, averaging not done properly, - only roughly
C    over a 2*2 area (to be quick)
C	if thrld not set then output error meg.
C
C  Given (program parameters)
C    INPUTIMG (RA)   image whose histogram is to be plotted
C    NAUTO (I)    autoscaling flag (0 if TZEROD and TMAXD not
C		  calculated yet)
C    TZEROD (R)   lowest intensity in image
C    TMAXD (R)    highest intesnity in image
C    THRLD (R)    threshold cut
C    REPLY (CH)    quadrant histo plotted in (from quad)
C    WORK (I*2A)    work array used to plot image
C
C  Returned (program parameters)
C    TZEROD,TMAXD (R)       sometimes
C
C  D. Tudhope/ROE/Sept 1982
C-

      INTEGER NAXIS(2),NPTR1,NAXWK(2),NPWK,IST,ISTAT,NAUTO,IXR,IYR
      REAL TZEROD,TMAXD,THRLD
C*  dims of args pict - as inpict may not be square and so only one dim will be 255
      REAL VDX,VDY
C*  coords of bottom left hand corner of quadrant
      INTEGER VXO,VYO
      REAL XEXT,YEXT

      IST=0
      CALL INPICR('INPUTIMG',' ',2,NAXIS,NPTR1,IST)
      CALL READR('THRLD',' ',0.0,-1.0E19,1.0E19,THRLD,IST)
      CALL READI('NAUTO_','GIVE AUTOSCALE FLAG',-1,0,1,NAUTO,IST)

      IF(IST.EQ.0) THEN
C*  first check thrld assigned
       IF (THRLD.EQ.-99999.0) THEN
         CALL WRERR('NOT')
       ELSE
C*  ok - proceed

        IF(NAUTO.EQ.0) THEN
          CALL AUTRNG(NAXIS(1),NAXIS(2),%VAL(NPTR1),TZEROD,TMAXD)
        ELSE
          CALL READR('TZEROD_','GIVE LOWEST VALUE TO BE PLOTTED',
     :               -1.0E20,-1.0E19,1.0E19,TZEROD,IST)
          CALL READR('TMAXD_','GIVE HIGHEST VALUE TO BE PLOTTED',
     :               -1.0E20,-1.0E19,1.0E19,TMAXD,IST)
        ENDIF

C  Obtain a work array from OUTPICI2.
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
C*  get bottom left hand corner of quadrant
        CALL QUAD(IXR,IYR,IDUM)
        VXO=IXR
        VYO=IYR
        CALL PLOTSTH(THRLD,0.0,XEXT-1.0,0.0,YEXT-1.0,NAXIS(1),NAXIS(2),
     :     %VAL(NPTR1),TZEROD,TMAXD,REAL(VXO),VXO+VDX,REAL(VYO),VYO+VDY,
     :     NAXWK(1),NAXWK(2),%VAL(NPWK),IST)
        CALL CLEARIM('WORK1')
        CALL WRITER('TZEROD_',TZEROD,IST)
        CALL WRITER('TMAXD_',TMAXD,IST)
       ENDIF
      ELSE
        CALL WRUSER('ERROR IN PLOT',ISTAT)
      ENDIF

      CALL CLEARIM('INPUTIMG')

      END
