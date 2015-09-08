      subroutine drawsc
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To draw boxs on the IKON or ARGS round the area effected by
*       given scans on an image containing several scans.
*
*SOURCE
*       DRAWSC.FOR in DRAWSCAN.TLB
*
*METHOD
*       Calculates the transformation from the image containing the
*       single scan to the composite image, and then applies this
*       transformation to each of the corners of the single scan image
*       to get their positions within the composite scan.
*             ARGSLIB routines are then used to draw a box in an
*       colour selected by the user. Routines from the GKS 7.2 version
*       of EDRS are used together with GKS and SGS for drawing on the
*       IKON.
*
*ARGUMENTS
*       none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               rdimds,wrerr,gtstrn,irastr
*       EDRS:
*               gtdscr,getpar,defdev,ikonop,ikoncl,ikon_ovop
*       SGS:
*               sgs_widen,sgs_flush
*       GKS:
*               gpl
*       ARGSLIB:
*               srinit,srsend
*       ARGS DATABASE:
*               args_numim,args_polyl
*       ROE ARGSLIB (IN ASPDIR):
*               args_ovcol,args_ovclr,args_dspall,args_ovwrt,args_allwrt
*       LIBDIR:ARGS.TLB:
*               args_ovcg
*
*STARLINK PARAMETERS
*       DEVICE/read/    Name of image device, ARGS or IKON
*       IMAGE/read/     Composite image
*       LOOP/read/      True if more than 1 box is to be displayed
*       OVERLAY/read/   If true then the boxes are overlayed over
*                       whatever is already in the overlay planes,
*                       otherwise the overlays are cleared first.
*       SCAN/read/      Image to be outlined
*       COLOUR/read/    Colour for graphics: red,green,blue,yellow,cyan,
*                       magenta,black,white.
*       NOCOMB/error/   Accessed if either image was produced by COMBINE
*                       but has non-zero CROTA1
*       NOIMAGE/error/  Accessed if there are no images currently
*                       displayed on the ARGS
*       NOARGS/error/   Accessed if ARGS cannot be allocated
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 30/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      real      c(6)    ! The required co-efficients
      real      cdlt1a  ! The size (in x) of a pixel in composite image
                        ! in degrees
      real      cdlt2a  ! The size (in y) of a pixel in composite image
                        ! in degrees
      real      cdlt1b  ! The size (in x) of a pixel in scan image in
                        ! degrees
      real      cdlt2b  ! The size (in y) of a pixel in scan image in
                        ! degrees
      logical   clear   ! If true then overlays are cleared at start
      character col(8:15)*1 ! Colours of 8 overlay planes
      character colour*7! Colour to plot in
      real      crot1a  ! Clockwise angle from north to composite image
                        ! -ve Y axis
      real      crot1b  ! Clockwise angle from north to scan image -ve
                        ! Y axis
      integer   crpx1a  ! The x value of the reference pixel in
                        ! composite image
      integer   crpx2a  ! The y value of the reference pixel in
                        ! composite image
      integer   crpx1b  ! The x value of the reference pixel in scan
                        ! image
      integer   crpx2b  ! The y value of the reference pixel in scan
                        ! image
      real      crvl1a  ! The RA (in degrees, +/- 180) of composite
                        ! image ref pixel
      real      crvl2a  ! The DEC (in degrees, +/- 90) of composite
                        ! image ref pixel
      real      crvl1b  ! The RA (in degrees, +/- 180) of scan image
                        ! ref pixel
      real      crvl2b  ! The DEC (in degrees, +/- 90) of scan image
                        ! ref pixel
      character cval*1  ! Dummy character argument
      character device*30! Name of image device, IKON or ARGS
      integer   i       ! Implicit loop count
      integer   icol    ! Position of colour in list of colours
      integer   iconid  ! Connection identifier of selected device
      integer   idev    ! Position of selected device in device list
      integer   ierr    ! Status return
      integer   iloop   ! Position of option within list in GTSTRN
      character instra*30 ! INSTRUME descriptor from composite image
      character instrb*30 ! INSTRUME descriptor from scan image
      integer   iplane  ! No. of overlay plane for plotting
      integer   itype   ! SGS workstation type of selected device
      integer   ival    ! Dummy integer argument
      integer   lcol    ! Length of colour string
      logical   loop    ! True if more than 1 box is to be displayed
      integer   ioverl  ! Position of option within list in GTSTRN
      integer   nim     ! Identifier of last image displayed on ARGS
      integer   nlin    ! No of lines in scan image
      integer   npix    ! No of pixels in each line in scan image
      real      rval    ! dummy real argument
      integer   switch  ! Contains flag bits for each plane of the ARGS
      real      x(5)    ! X co-ords of vertices of poly line outlining
                        ! the scan
      real      y(5)    ! Y co-ords of vertices of poly line outlining
                        ! the scan

*
* COLOUR DATA FOR OVERLAYS
*
      data col/'W','R','G','B','Y','C','M','L'/

*
* GET THE NECESSARY DESCRIPTORS FROM COMPOSITE IMAGE (SEE LOCAL VARIABLE
* DECLARATIONS FOR A DESCRIPTION OF EACH)
*
      call rdimds('IMAGE',.false.,crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,
     :             cdlt2a,crot1a,instra,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) goto 999

*
* SEE IF USER WISHES TO DISPLAY BOXES OVER WHATEVER IS ALREADY IN THE
* OVERLAY PLANES
*
      ioverl=2
      call gtstrn('OVERLAY',.true.,'TRUE,FALSE,YES,NO.',1,ioverl,cval,
     :            ival,ierr)
      if(ioverl.eq.1.or.ioverl.eq.3) then
         clear=.false.
      else
         clear=.true.
      endif

*
* SEE IF USER WISHES TO DISPLAY MORE THAN ONE BOX
*
      iloop=1
      call gtstrn('LOOP',.true.,'TRUE,FALSE,YES,NO.',1,iloop,cval,ival,
     :            ierr)
      if(iloop.eq.1.or.iloop.eq.3) then
         loop=.true.
      else
         loop=.false.
      endif

*
* FIND OUT WHICH DEVICE TO USE: ARGS OR IKON
*
      call defdev(device)
      call sgs_widen(device,itype,iconid,ierr)
      if(itype.eq.3200) then
         idev=2
      else
         idev=1
      endif
      call gtstrn('DEVICE',.true.,'ARGS,IKON.',1,idev,device,ival,ierr)

*
* ARGS SPECIFIC SECTION.....
*
      if(device.eq.'ARGS') then

*
* ALLOCATE ARGS
*
         call srinit(0,.false.,ierr)
         if(ierr.ne.0) then
            call wrerr('NOARGS')
            goto 999
         endif

*
* CHECK THAT THERE IS AN IMAGE ON THE ARGS AND GET ITS IDENTIFIER
*
         call args_numim(nim)
         if(nim.eq.0) then
            call wrerr('NOIMAGE')
            goto 999
         endif

*
* SET UP COLOURS FOR OVERLAY PLANES. CLEAR EACH PLANE IF REQUIRED.
* ENSURE ALL PLANES ARE VISIBLE AND ALL GUNS ENABLED
*
         do iplane=8,15
            call args_ovcol(iplane,col(iplane))
            if(clear) call args_ovclr(iplane)
         enddo
         call args_dspall(switch)
         call args_ovcg('W')

*
* END OF ARGS SECTION. NOW CALL IKON OP TO DO IKON SPECIFIC START UP
*
      else
         call ikonop(.false.,clear,ierr)
         if(ierr.ne.0) goto 999
      endif

*
* IF REQUIURED, LOOP UNTIL A NULL SCAN IMAGE IS GIVEN
*
      icol=2
   10 continue

*
* GET THE NECESSARY DESCRIPTORS FROM SCAN IMAGE
*
      call rdimds('SCAN',.true.,crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,
     :                  cdlt2b,crot1b,instrb,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) goto 999

*
* GET THE SIZE OF THE SCAN IMAGE
*
      call gtdscr('SCAN','NAXIS1','INTEGER',npix,rval,cval,ierr)
      if(ierr.ne.0) goto 999
      call gtdscr('SCAN','NAXIS2','INTEGER',nlin,rval,cval,ierr)
      if(ierr.ne.0) goto 999

*
* CALL IRASTR TO CALCULATE THE CO-EFFICIENTS OF THE LINEAR
* TRANSFORMATION FROM SCAN IMAGE TO COMPOSITE IMAGE
*
      call irastr(crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,cdlt2b,crot1b,
     :            crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,cdlt2a,crot1a,c)

*
* CALCULATE THE CO-ORDINATES WITHIN THE COMPOSITE IMAGE OF THE SCAN
* IMAGE CORNERS
*
      x(1)=c(1)+c(2)+c(3)
      x(2)=c(1)+c(2)+c(3)*nlin
      x(3)=c(1)+c(2)*npix+c(3)*nlin
      x(4)=c(1)+c(2)*npix+c(3)
      x(5)=x(1)
      y(1)=c(4)+c(5)+c(6)
      y(2)=c(4)+c(5)+c(6)*nlin
      y(3)=c(4)+c(5)*npix+c(6)*nlin
      y(4)=c(4)+c(5)*npix+c(6)
      y(5)=y(1)

*
* GET COLOUR TO DRAW BOX IN
*
      call gtstrn('COLOUR',.true.,'WHITE,RED,GREEN,BLUE,YELLOW,CYAN,'
     :            //'MAGENTA,BLACK.',1,icol,colour,lcol,ierr)

*
* GET OVERLAY PLANE WITH REQUESTED COLOUR
*
      iplane=icol+7

*
* DO ARGS SPECIFIC DRAWING
*
      if(device.eq.'ARGS') then

*
* OPEN THE SELECTED OVERLAY PLANE FOR GRAPHICS OUTPUT
*
         call args_ovwrt(iplane)

*
* DRAW THE BOX
*
         call args_polyl(nim,5,x,y,ierr)

*
* FLUSH THE ARGS BUFFER
*
         call srsend

*
* DO IKON SPECIFIC DRAWING
*
      else
         call ikon_ovop(iplane,colour)
         call gpl(5,x,y)
         call sgs_flush
      endif

*
* GO ROUND FOR NEXT SCAN IF REQUIRED
*
      if(loop) then
         call cnpar('COLOUR',ierr)
         call cnpar('SCAN',ierr)
         goto 10
      endif

*
* TIDY UP
*
  999 continue

*
* DO ARGS SPECIFIC SHUTDOWN
*
      if(device.eq.'ARGS') then

*
* RE-ENABLE IMAGE AND OVERLAY PLANES
*
         call args_allwrt

*
* DO IKON SPECIFIC SHUTDOWN
*
      else
         call ikoncl
      endif

*
* FINISH
*
      end
