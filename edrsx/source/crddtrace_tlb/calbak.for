      subroutine calbak(prouse,dsize,x,y,grad,size,inval,pixel,ilin,
     :                  data,npix,nlin,slope,const)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Calculates the slope and offset of a linear background below a
*       peak in the CRDD.
*
*SOURCE
*       CALBAK.FOR in CRDDTRACE.TLB
*
*METHOD
*       The gradient of the CRDD at each pixel within a section of CRDD
*       near the supplied peak centre, is calculated. For a typical peak
*       the gradient will start low and reach a maximum just below the
*       peak centre. It will then drop to a minimum just above the peak
*       centre and then slowly rise back to the original value. The
*       maximum and minimum positions and values are found. The mean
*       gradient is defined as the mean of the maximum and the minimum
*       gradient values (this will be close to the gradient at the peak)
*       and upeer and lower gradient limits defined as 0.1 of the
*       difference between max and min gradient, above and below the
*       mean gradient. The x positions at which the gradient reaches
*       these limits outside the interval between the maximum and the
*       minimum, are then found and used as the two points to define
*       the linear background.
*               This all amounts to finding the positions in the CRDD
*       which have nearly the same gradient as the peak and which lie
*       at the bottom of the peak, and using them to define the back-
*       ground.
*
*ARGUMENTS
*   INPUTS:
*       prouse          integer The width of the non-zero part of a
*                               peak profile, in data pixels
*       dsize           real    The size of a CRDD pixel in arcmins
*       x(size)         real    Real workspace for the x positions
*                               of each valid point within the CRDD
*                               section used to determine the background
*       y(size)         real    Real workspace for the y positions
*                               of each valid point within the CRDD
*                               section used to determine the background
*       grad(size)      real    Real workspace for the gradient of the
*                               CRDD at each valid point within the CRDD
*                               section used to determine the background
*       size            integer The size of arrays x,y and grad
*       data(npix,nlin) real    The unscaled CRDD
*       npix,nlin       integersThe dimensions of the CRDD
*       ilin            integer The line of CRDD to use in
*                               determining the background
*       inval           integer The invalid pixel value for CRDD
*       pixel           integer The nearest pixel to the cursor
*   OUTPUTS:
*       slope           real    Slope of background in unscale
*                               data units per arcmin
*       const           real    The value of the background at
*                               the cursor in unscaled units
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 15/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   prouse,size,inval,pixel,ilin,npix,nlin
      real      x(size),y(size),grad(size)
      real      dsize,data(npix,nlin),slope,const
*
* DECLARE LOCAL VARIABLES
*
      integer   dif     ! Difference in entry no. between maximum and peak
      logical   donemn  ! True when minimum immediately above maximum
                        ! has been found
      integer   entlo   ! Entry no. of lowest data value
      integer   entry   ! No. of valid CRDD pixel in section of CRDD used
      integer   entzer  ! Entry no. of peak centre
      real      factor  ! Stores no. of gradient values in average
      real      gmax    ! Value of gradient at maximum closest to peak
      real      gmin    ! Value of gradient at minimum closest to peak
      real      gmn(10) ! Value of gradient at all minima found in gradien
      real      gmx(10) ! Value of gradient at all maxima found in gradien
      real      gradhi  ! Gradient above CRDD pixel
      real      gradlo  ! Gradient below CRDD pixel
      real      grado   ! Estimate of gradient at the peak
      real      hilim   ! Upper limit on gradient of background defining points
      logical   incres  ! True if CRDD is increasing (i.e. +ve gradient)
      integer   j       ! Entry counter
      integer   j0      ! Entry no. of lower point defining background
      integer   j1      ! Entry no. of upper point defining background
      integer   jhi     ! Entry no. of gradient minimum
      integer   jlo     ! Entry no. of gradient maximum
      integer   jmn(10) ! Entry no.s of all minima in gradient
      integer   jmx(10) ! Entry no.s of all maxima in gradient
      real      lolim   ! Lower limit on gradient of background defining points
      integer   nmn     ! No. of minima found in gradient
      integer   nmx     ! No. of maxima found in gradient
      real      x0      ! X position of lower point defining background
      real      x1      ! X position of upper point defining background
      real      xhi     ! The position of the maximum value gmax
      real      xlo     ! The position of the minimum value gmin
      real      xmn(10) ! Position of all minima found in gradient
      real      xmx(10) ! Position of all maxima found in gradient
      real      y0      ! Y position of lower point defining background
      real      y1      ! Y position of upper point defining background
      real      ymin    ! Minimum data value in section used
*
* SET UP X VALUES IN ARCMINS FROM CURSOR, Y VALUES IN UNSCALED DATA
* UNITS, FIND NO. OF VALID POINTS AND LOWEST Y VALUE
*
      ymin=1.0e30
      entry=0
*
* LOOP THROUGHT A SECTION TWICE THE WIDTH OF A POINT SOURCE
*
      do j=-prouse,prouse
*
* IGNORE PIXELS PAST THE END OF THE IMAGE OR INVALID PIXELS
*
         if(j+pixel.ge.1.and.j+pixel.le.npix) then
            if(data(j+pixel,ilin).ne.inval) then
*
* INCREMENT VALID ENTRY COUNTER
*
               entry=entry+1
*
* LOCATE WHICH ENTRY IS NEAREST TO THE CURSOR POSITION
*
               if(j.le.0) entzer=entry
*
* STORE X AND Y FOR THIS ENTRY
*
               x(entry)=j*dsize
               y(entry)=data(j+pixel,ilin)
*
* UPDATE MINIMUM DATA VALUE IN SECTION IF NECESSARY
*
               if(y(entry).lt.ymin) then
                  ymin=y(entry)
                  entlo=entry
               endif
            endif
         endif
      enddo
*
* FORM THE GRADIENT OF THE DATA
*
      do j=1,entry
         factor=0.5
*
* CALCULATE GRADIENT ABOVE CURRENT POSITION
*
         if(j.lt.entry) then
            gradhi=(y(j+1)-y(j))/(x(j+1)-x(j))
         else
            gradhi=0
            factor=1.0
         endif
*
* CALCULATE GRADIENT BELOW CURRENT POSITION
*
         if(j.gt.1) then
            gradlo=(y(j)-y(j-1))/(x(j)-x(j-1))
         else
            gradlo=0
            factor=1.0
         endif
*
* STORE AVERAGE GRADIENT
*
         grad(j)=factor*(gradlo+gradhi)
      enddo
*
* LOCATE ALL MAXIMA AND MINIMA IN GRADIENT DATA
*
      nmx=0
      nmn=0
*
* INITIALIZE THE INCRESING/DECREASING FLAG SO THAT INITIAL ENTRY WILL
* NOT BE TAKEN AS A TURNING POINT
*
      if(grad(2).lt.grad(1)) then
         incres=.false.
      else
         incres=.true.
      endif

*
* LOOP THROUGHT REST OF GRADIENT DATA
*
      do j=2,entry
*
* IF GRADIENT IS DECREASING BUT PREVIOUSLY WAS INCRESING, A MAXIMUM
* HAS BEEN FOUND. INCREMENT NO. FOUND AND STORE THE GRADIENT, THE X
* POSITION AND THE ENTRY NO
*
         if(grad(j).lt.grad(j-1)) then
            if(incres) then
               nmx=nmx+1
               gmx(nmx)=grad(j-1)
               xmx(nmx)=x(j-1)
               jmx(nmx)=j-1
            endif
*
* FLAG THAT GRADIENT IS NOW DECREASING
*
            incres=.false.
*
* LIKEWISE FOR MINIMA
*
         else
            if(.not.incres) then
               nmn=nmn+1
               gmn(nmn)=grad(j-1)
               xmn(nmn)=x(j-1)
               jmn(nmn)=j-1
            endif
            incres=.true.
         endif
      enddo
*
* IF AT LEAST ON MAX AND ONE MIN WAS FOUND...
*
      if(nmn.gt.0.and.nmx.gt.0) then
*
* LOCATE THE MAXIMUM CLOSEST TO THE CURSOR POSITION
*
         dif=3*entry
         do j=1,nmx
            if(abs(jmx(j)-entzer).lt.dif) then
               dif=abs(jmx(j)-entzer)
               xlo=xmx(j)
               gmax=gmx(j)
               jlo=jmx(j)
            endif
         enddo
*
* FIND THE ADJACENT MINIMUM  ABOVE THE MAXIMUM JUST FOUND
*
         jhi=0
         do j=nmn,1,-1
            if(jmn(j).gt.jlo) then
               xhi=xmn(j)
               gmin=gmn(j)
               jhi=jmn(j)
            endif
         enddo
      endif
*
* LOCATE THE X POSITIONS AT WHICH THE GRADIENT BECOMES NEARLY EQUAL
* TO THE GRADIENT AT THE PEAK. INITIALISE ENTRIES TO INDICATE NO SUCH
* X POSITIONS FOUND
*
      j0=0
      j1=0
*
* IF THERE WERE NO MAXIMA OR MINIMA FOUND IN THE GRADIENT DATA,
* OR IF THE ONES THAT WERE FOUND WERE VERY CLOSE TO THE PEAK,
* OR IF NO MINIMUM WAS FOUND ABOVE THE MAXIMUM CLOSEST TO THE PEAK,
* THE BACKGROUND IS NOT WELL DEFINED.
*
      if(nmn.gt.0.and.nmx.gt.0.and.xhi.gt.0.4*x(entry).and.
     :    xlo.lt.0.4*x(1).and.jhi.ne.0) then
*
* CALCULATE GRADIENT AT PEAK AND LIMITS WHICH DEFINE THE GRADIENT
* AT THE REQUIRED X POSITIONS
*
         grado=0.5*(gmax+gmin)
         hilim=grado+0.1*(gmax-gmin)
         lolim=grado-0.1*(gmax-gmin)
*
* FIND THE X POSITIONS BY LINEAR INTERPOLATION BETWEEN ENTRIES
* WHOSE GRADIENT DATA STRADDLE THE LIMITS
*
         donemn=.false.
         do j=1,entry
*
* THE X POSITIONS MUST LIE FURTHER FROM THE PEAK THAN THE TURNING
* POINTS IN THE GRADIENT DATA
*
            if(x(j).lt.xlo) then
               if(grad(j).ge.hilim.and.grad(j-1).lt.hilim) then
                  x0=x(j-1)+(x(j)-x(j-1))*(hilim-grad(j-1))/
     :               (grad(j)-grad(j-1))
                  j0=j
               endif
            else if(x(j).gt.xhi) then
               if(.not.donemn) then
                  if(grad(j).ge.lolim.and.grad(j-1).lt.lolim) then
                     x1=x(j-1)+(x(j)-x(j-1))*(lolim-grad(j-1))/
     :                  (grad(j)-grad(j-1))
                     j1=j
                     donemn=.true.
                  endif
               endif
            endif
         enddo
      endif
*
* CALCULATE THE Y VALUES AT THESE X POSITIONS BY LINEAR INTERPOLATION
*
      if(j0.ne.0.and.j1.ne.0) then
         y0=((x(j0)-x0)*y(j0-1)+(x0-x(j0-1))*y(j0))/(x(j0)-x(j0-1))
         y1=((x(j1)-x1)*y(j1-1)+(x1-x(j1-1))*y(j1))/(x(j1)-x(j1-1))
      else
         x0=0
         x1=0
         y0=y(entlo)
      endif
*
* CALCULATE SLOPE AND CONSTANT TERM OF STRAIGHT LINE JOINING THE
* CALCULATED (X,Y) POSITIONS
*
      if(x0.ne.x1) then
         slope=(y1-y0)/(x1-x0)
      else
         slope=0
      endif
      const=y0-x0*slope
*
* FINISH
*
      end

