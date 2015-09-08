      subroutine drebar(x,y,error,npts)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Draws a series of error bars over a NCAR background.
*
*METHOD
*       Draw a diamond centred on each point, then draw a vertical
*       line through the diamond. Emulates the old HIGR_DREBAR routine.
*       
*ARGUMENTS       
*   INPUTS:
*       npts		integer		No. of error bars to plot
*       x(npts) 	real		X coords of error bars
*       y(npts) 	real		Y coords of error bars
*       error(npts)	real		Length of error bars
*
*SUBROUTINES CALLED
*	NCAR:
*		cfux,cfuy,curve,line
*              
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/7/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer	npts
      real	x(npts),y(npts),error(npts)
*
* DECLARE LOCAL VARIABLES
*
      real	cfux	! NCAR fractional to user coords conversion x
      real	cfuy	! NCAR fractional to user coords conversion y
      real	dx	! Half width of a diamond in user coords
      real	dy	! Half height of a diamond in user coords
      integer	i	! loop count
      real	tx(5)	! X coords of diamonf vertices
      real	ty(5)	! Y coords of diamonf vertices
*
* DETERMINE SIZE OF DIAMOND IN USER CO-ORDS
*
      dx=0.004*(cfux(1.0)-cfux(0.0))
      dy=0.0066*(cfuy(1.0)-cfuy(0.0))
*
* LOOP ROUND EACH POINT
*
      do i=1,npts
*
* SET UP COORDS OF DIAMOND VERTICES, CENTRED ON CURRENT COORDS
*
         tx(1)=x(i)
         tx(2)=x(i)-dx
         tx(3)=x(i)
         tx(4)=x(i)+dx
         tx(5)=x(i)
         ty(1)=y(i)-dy
         ty(2)=y(i)
         ty(3)=y(i)+dy
         ty(4)=y(i)
         ty(5)=y(i)-dy
*
* DRAW THE DIAMOND
*
         call curve(tx,ty,5)
*
* DRAW THE ERROR BAR AS A STRAIGHT LINE LIKE HIGR_DREBAR DOES IT
*
         if(error(i).gt.dy) then
            call line(x(i),y(i)-error(i),x(i),y(i)+error(i))
         else
            call line(x(i),y(i),x(i),y(i)+dy)
         endif
      enddo
*
* FLUSH NCAR BUFFERS
*
      call flush
*
* FINISH
*
      end
