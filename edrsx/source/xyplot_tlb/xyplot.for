      subroutine xyplot
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To draw polygons on the IKON or ARGS joining the entries
*       in a given XY list.
*
*SOURCE
*       XYPLOT.FOR in XYPLOT.TLB
*
*ARGUMENTS
*       none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr,gtstrn,gtbool,gtwork
*       EDRS:
*               gtxylr,extlst,gtdscr,getpar,defdev,ikonop,ikoncl,
*		ikon_ovop
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
*	CLOSE/read/     True if polygon is to be closed
*       DEVICE/read/    Name of image device, ARGS or IKON
*       INPUT/read/     XY list
*       OVERLAY/read/   If true then the plot is overlayed over
*                       whatever is already in the overlay planes,
*                       otherwise the overlays are cleared first.
*       COLOUR/read/    Colour for graphics: red,green,blue,yellow,cyan,
*                       magenta,black,white.
*       NOIMAGE/error/  Accessed if there are no images currently
*                       displayed on the ARGS
*       NOARGS/error/   Accessed if ARGS cannot be allocated
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/3/90
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      logical   clear   ! If true then overlays are cleared at start
      logical	close	! If true then polygon is closed
      character col(8:15)*1 ! Colours of 8 overlay planes
      character colour*7! Colour to plot in
      character cval*1  ! Dummy character argument
      character device*30! Name of image device, IKON or ARGS
      integer   i       ! Implicit loop count
      integer   icol    ! Position of colour in list of colours
      integer   iconid  ! Connection identifier of selected device
      integer   idev    ! Position of selected device in device list
      integer   ierr    ! Status return
      integer   iloop   ! Position of option within list in GTSTRN
      integer	ipin	! Pointer to input XY list
      integer	ipx	! Pointer to X values from input XY list
      integer	ipy	! Pointer to Y values from input XY list
      integer   iplane  ! No. of overlay plane for plotting
      integer   itype   ! SGS workstation type of selected device
      integer   ival    ! Dummy integer argument
      integer   lcol    ! Length of colour string
      logical   loop    ! True if more than 1 box is to be displayed
      integer	lstlen	! No. of entries in the XY list 
      integer   nim     ! Identifier of last image displayed on ARGS
      integer	nitem	! No. of items associated with each list entry
      integer   nlin    ! No of lines in scan image
      integer   npix    ! No of pixels in each line in scan image
      logical	poly	! True if polygon required
      real      rval    ! dummy real argument
      integer   switch  ! Contains flag bits for each plane of the ARGS

*
* COLOUR DATA FOR OVERLAYS
*
      data col/'W','R','G','B','Y','C','M','L'/

*
* OBTAIN THE INPUT XY LIST
*
      call gtxylr('INPUT',.true.,nitem,lstlen,ipin,ierr)
      if(ierr.ne.0) goto 999
 
*
* OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD X AND Y VALUES
*
      call gtwork('X','INTEGER',lstlen+1,ipx,ierr)
      if(ierr.eq.0) call gtwork('Y','INTEGER',lstlen+1,ipy,ierr)
      if(ierr.ne.0) goto 999 
 
*
* COPY INPUT LIST DATA TO WORKSPACE
*
      call extlst(%val(ipin),nitem,lstlen,%val(ipx),21,24)
      call extlst(%val(ipin),nitem,lstlen,%val(ipy),25,28)
 
*
* SEE IF USER WISHES TO DISPLAY GRAPHICS OVER WHATEVER IS ALREADY IN THE
* OVERLAY PLANES
*
      clear=.true.
      call gtbool('OVERLAY',.true.,clear,ierr)
      clear=.not.clear

*
* SEE IF USER WISHES TO PLOT A POLYGON OR ISOLATED CROSSES
*
      poly=.true.
      call gtbool('POLY',.true.,poly,ierr)

*
* IF PLOTTING A POLYGON, SEE IF USER WISHES TO JOIN THE LAST POINT 
* UPTO THE FIRST POINT
*
      if(poly) then
         close=.true.
         call gtbool('CLOSE',.true.,close,ierr)
         if(close) then
            lstlen=lstlen+1
            call closit(%val(ipx),%val(ipy),lstlen)
         endif
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
* GET COLOUR TO DRAW IN
*
      icol=2
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
* DRAW A POLYGON IF REQUIRED, OTHERWISE PRODUCE INDIVIDUAL MARKERS
*
         if(poly) then
            call args_polyl(nim,lstlen,%val(ipx),%val(ipy),ierr)
         else
            call amark(nim,lstlen,%val(ipx),%val(ipy),'ARGS')
         endif

*
* FLUSH THE ARGS BUFFER
*
         call srsend

*
* DO IKON SPECIFIC DRAWING
*
      else
         call ikon_ovop(iplane,colour)
         if(poly) then
            call gpl(lstlen,%val(ipx),%val(ipy))
         else
            call amark(nim,lstlen,%val(ipx),%val(ipy),'IKON')
         endif
         call sgs_flush
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





*-------------------------------------------------------------------
      subroutine closit(x,y,lstlen)
      implicit none
      integer	lstlen
      real	x(lstlen),y(lstlen)

      x(lstlen)=x(1)
      y(lstlen)=y(1)

      end

*---------------------------------------------------------------------
      subroutine amark(nim,lstlen,x,y,device)
      integer	nim,lstlen
      real	x(lstlen),y(lstlen)
      character device*(*)

      do i=1,lstlen
         call ovcros(x(i),y(i),3,3,device)
      enddo

      end

