      SUBROUTINE PLOTS(WXMIN,WXMAX,WYMIN,WYMAX,INXEXT,INYEXT,INPICT,
     :TZEROD,TMAXD,VXMIN,VXMAX,VYMIN,VYMAX,WKXEXT,WKYEXT,WKPICT,ISTATUS)

C+
C    PLOTS
C
C    generalised plotting subroutine to plot an image on the args
C    scales a window on the image onto a specified area of screen.
C    scaling up if window smaller than screen area (viewport) and down if vice versa.
C
C    window on image in WXMIN..WYMAX is plotted on viewport on ARGS VXMIN..VXMAX
C ***********
C    args data base is updated. now because data base cannot handle windows
C    on image to be plotted, it is told that what is plotted is the whole image
C    when in fact it is just a window on it. This means that cursor coords
C    from such a plot will only be relative to the window and not the whole
C    image. Until this defect in the data base is rectified, it is not
C    proposed to inform users about the windowing capability of this routine
C    which will be hidden in higher level calling subroutines.
C ***********
C   Also performs intensity scaling.
C    The scale factors for X (XF) and Y (YF) in going from window to viewport are calculated
C    and used to determine how to access INPICT. They also determine the 4
C    possible cases for shrinking, depending whether or not shrinking is to be
C    done in the X or Y dimensions.
C    the variables WX,WY scan thru INPICT at a resolution of XF and YF
C    and the pixels they land on will be transferred to WKPICT, perhaps
C    averaged on the small nearby neighbourhood, of side 1 or 2
C    ************* when shrinking an image down, it must be remembered that the
C    ************* plotted image is not exact in either intensities or x,y coords
C    ************* cursor values from such a reduced image, although in the coord
C    ************* space of the original image will not be exact.
C    ************* if the user wants accurate coords returned, then a 1-1 plot
C    ************* of the correct window in the original image must be used.
C    ************* the shrunk one is just for a rough look at the whole image.
C    checks all window/viewport args for error
C    originally window/vierport in reals in coords 0.0 .. whatever
C    they are translated to integers, but because window refers to
C    array (starts at 1) and viewport to screen (starts at 0), 1 is added to window, but not vport.
C
C    given (arguments)
C      WXMIN,WYMIN,WXMAX,WYMAX  (R)  window on image (pixel coords 0.0 .. inxyext-1)
C      INXEXT,INYEXT            (I)  dimensions of INPICT
C      TZEROD,TMAXD             (R)  minimum and maximum intensities in image
C      INPICT                   (RA)  image to be plotted
C      VXMIN,VXMAX,VYMIN,VYMAX  (R)  viewport on ARGS where plot to go (args coords 0.0 .. 511.0)
C      WKXEXT,WKYEXT            (I)  dimensions of WKPICT
C
C    returned (arguments)
C      WKPICT                   (I2A)  work area to hold screen output
C      ISTATUS                  (I)    status - 0 is ok
C
C      D. Tudhope    ROE   July 1982
C-



      REAL WXMIN,WXMAX,WYMIN,WYMAX,VXMIN,VXMAX,VYMIN,VYMAX
      INTEGER INXEXT,INYEXT,WKXEXT,WKYEXT,ISTATUS
      REAL TZEROD,TMAXD,INPICT(INXEXT,INYEXT)
      INTEGER*2 WKPICT(WKXEXT,WKYEXT)
C*  local variables
C*  integer window in 1..512 (or more) type coords
      INTEGER IWXO,IWYO,IWXT,IWYT
C*  integer viewport in 0..511 args coords
      INTEGER IVXO,IVYO,IVXT,IVYT
C*  dimensions of window and viewport
      INTEGER WDX,WDY,VDX,VDY
C*  above dimensions adjusted for args data base (because it's silly)
      INTEGER AWDX,AWDY,AVDX,AVDY
C*  scale factors in X and Y from window to viewport
      REAL XF,YF
C*  indices to window in INPICT, except these are reals dependant on XF,YF
      REAL WX,WY
C*  and their corresponding integer values (rounded down)
      INTEGER IWX,IWY
C*  indices to viewport in array OUTPICT
      INTEGER VX,VY
C*  dimension of args screen
      INTEGER ARGSDIM
      PARAMETER (ARGSDIM=511)
C*  dummy var for low level plotting routine SRPXI2
      INTEGER*2 DUM(1)





C*  statement function to scale a pixel intensity (A) to use full intensity range
C*  defined by minimum (TZEROD) and maximum (TMAXD) intensities in picture.
      INTEGER*2 SCALE
      SCALE(A)=MIN(255,MAX(0,NINT(255.0*(A-TZEROD)/(TMAXD-TZEROD))))





C*  initialise args data base
      CALL SRINIT(0,.FALSE.,ISTATUS)
C*  translate window to integer, adding 1 cos arrays start at 1
      IWXO=NINT(WXMIN+1)
      IWXT=NINT(WXMAX+1)
      IWYO=NINT(WYMIN+1)
      IWYT=NINT(WYMAX+1)
C*  translate viewport to integer, not adding 1 cos stays in args coords
      IVXO=NINT(VXMIN)
      IVXT=NINT(VXMAX)
      IVYO=NINT(VYMIN)
      IVYT=NINT(VYMAX)
C*  get dimensions of window
      WDX=IWXT-IWXO+1
      WDY=IWYT-IWYO+1
      VDX=IVXT-IVXO+1
      VDY=IVYT-IVYO+1
C*  now check all window/viewport arguments
       ISTATUS=1
       IF ((IVXO.GE.0).AND.(IVYO.GE.0)) THEN
       IF ((IVXT.GT.IVXO).AND.(IVYT.GT.IVYO)) THEN
       IF ((IVXT.LE.ARGSDIM).AND.(IVYT.LE.ARGSDIM)) THEN
C*  .gt.0 here cos coords start at 1
       IF ((IWXO.GT.0).AND.(IWYO.GT.0)) THEN
       IF ((IWXT.GT.IWXO).AND.(IWYT.GT.IWYO)) THEN
       IF ((IWXT.LE.INXEXT).AND.(IWYT.LE.INYEXT)) THEN
       ISTATUS=0
C*  all checks ok
C*  get scale factors -- window-->viewport
      XF=WDX/REAL(VDX)
      YF=WDY/REAL(VDY)
C*  now double loop plotting to WKPICT
C*  WX,WY point to area in INPICT and VX,VY point to pixel in WKPICT
      WY=IWYO
      DO VY=1,VDY
       IWY=INT(WY)
       WX=IWXO
       DO VX=1,VDX
        IWX=INT(WX)
C*  see which of 4 cases for possible shrinking (XF or YF > 1) it is
        IF (YF.GT.1.0) THEN
         IF (XF.GT.1.0)  THEN
C*  shrink x and y 2 by 2 neighbourhood
           WKPICT(VX,VY)=SCALE((INPICT(IWX,IWY)+INPICT(IWX+1,IWY)
     :                   +INPICT(IWX,IWY+1)+INPICT(IWX+1,IWY+1))/4.0)
         ELSE
C*  shrink y only
           WKPICT(VX,VY)=SCALE((INPICT(IWX,IWY)+INPICT(IWX,IWY+1))/2.0)
         ENDIF
        ELSE
         IF (XF.GT.1.0) THEN
C*  shrink x only
           WKPICT(VX,VY)=SCALE((INPICT(IWX,IWY)+INPICT(IWX+1,IWY))/2.0)
         ELSE
C*  no shrinking at all
           WKPICT(VX,VY)=SCALE(INPICT(IWX,IWY))
         ENDIF
        ENDIF
        WX=WX+XF
       ENDDO
       WY=WY+YF
      ENDDO
C*  plot picture starting at IVXO,IVYO - last 4 args are normal defaults
      CALL SRPXI2(WKPICT,WKXEXT,VDX,VDY,IVXO,IVYO,16,.FALSE.,DUM,1)
C*  enter plotted picture in args data base after adjusting dims. cos of bug there $$**$$
      CALL DBADJUST(WDX,VDX,AWDX,AVDX)
      CALL DBADJUST(WDY,VDY,AWDY,AVDY)
      CALL ARGS_WRIM(IVXO+VDX/2,IVYO+VDY/2,AVDX,AVDY,AWDX,AWDY,ISTATUS)
       ENDIF
       ENDIF
       ENDIF
       ENDIF
       ENDIF
       ENDIF
      END
