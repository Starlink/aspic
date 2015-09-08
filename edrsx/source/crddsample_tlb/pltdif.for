      subroutine pltdif(data,nsamp,rinval,m,c,x,y,ball)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Plots the given data and overlays a straight line defined
*	by M and C.
*
*SOURCE
*       PLTDIF.FOR in CRDDSAMPLE.TLB
*
*ARGUMENTS       
*   INPUT:
*	data(nsamp)	real	The data to be plotted
*	nsamp		integer	No. of data values
*	rinval		real	Flag for invalid data samples
*	m		real	Straight line gradient
*	c		real	straight line intercept
*	ball		integer	The detector Ball no.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,gtbool,ncrbck
*       EDRS:
*              lbgone
*	INTERIM:
*	       cnpar
*       NCAR:
*	       agsetf,agcurv,line,plotit
*	SGS:
*	       sgs_flush
*              
*STARLINK PARAMETERS
*	PLOT	If NO then do not produce a plot for this detector
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/2/90
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	nsamp,ball
      real	data(nsamp),rinval,m,c,x(2*nsamp),y(2*nsamp)

*
* DECLARE LOCAL VARIABLES
*
      character cball*2 ! Character version of detector Ball no.
      real	datval	! Single data value
      integer	istat	! Temporary status value
      logical	plot	! If true then the data is plotted
      real	range	! Range of y values
      integer	samp	! Sample counter
      real	xmax	! X axis maximum value
      real	xmin	! X axis minimum value
      real	ymax	! Y axis maximum value
      real	ymin	! Y axis minimum value

*
* SEE IF USER WANTS TO PLOT THIS DETECTOR
*
      write(cball,'(I2)') ball
      call lbgone(cball)
      call wruser('  Produce plot of fit for detector #'//cball//' ?',
     :             istat)

      plot=.true.
      call gtbool('PLOT',.true.,plot,istat)
      call cnpar('PLOT',istat)
      if(plot) then

*
* STORE THE HISTOGRAM COORDINATES IN THE COORDINATE ARRAYS
*
         ymax=-1.0E32
         ymin=1.0E32

         do samp=1,nsamp
            datval=data(samp)

            if(datval.eq.rinval) then
               y(samp*2-1)=rinval
               y(samp*2)=rinval

            else
               y(samp*2-1)=datval
               y(samp*2)=datval
               ymax=max(ymax,datval)
               ymin=min(ymin,datval)

            endif

            x(samp*2-1)=samp-0.5
            x(samp*2)=samp+0.5

         enddo

*
* MODIFY THE Y AXIS LIMITS TO INCLUDE ALL OF THE STRAIGHT LINE
*
         ymax=max(ymax,m*nsamp+c,m+c)
         ymin=min(ymin,m*nsamp+c,m+c)

         range=ymax-ymin
         ymax=ymax+0.2*range
         ymin=ymin+0.2*range

         xmax=nsamp+0.5
         xmin=0.5

*
* TELL NCAR TO CLEAR THE SCREEN BEFORE PLOTTING
*
         call agsetf('FRAME.',3.0)

*
* TELL NCAR WHAT THE INVALID VALUE IS
*
         call agsetf('NULL/1.',rinval)

*
* PLOT THE BACKGROUND
*
         call ncrbck(xmin,xmax,ymin,ymax,'Fit for detector #'//cball,
     :               'Sample number','Data residuals (Jy)')

*
* PLOT THE DATA
*
         call agcurv(x,1,y,1,2*nsamp,1)

*
* PLOT THE FIT
*
         call line(1.0,m+c,real(nsamp),m*nsamp+c)

*
* FLUSH THE GRAPHICS BUFFERS
*
         call plotit(0,0,0)
         call sgs_flush

      endif

*
* FINISH
*
      end
