      subroutine scplot(xim,npixx,nlinx,invalx,scalex,zerox,
     :                  yim,npixy,nliny,invaly,scaley,zeroy,
     :                  work,wksize,xlims,ylims)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Plots points in a scatter plot of corrsponding pixel values
*       from two images.
*
*SOURCE
*       SCPLOT.FOR in SCATTER.TLB
*
*METHOD
*       The overlap region of the two images is scanned, and the data
*       value of corresponding valid pixels stored in the work array.
*       All the points are then plotted using GKS routine GPM.
*       
*ARGUMENTS       
*   INPUTS:
*       xim(npixx,nlinx) I*2	  Image for x axis
*       npixx,nlinx	 Integer  Size of x image
*       invalx		 Integer  Flag for invalid pixels in x image
*       scalex		 Real	  Scale factor for x image
*       zerox            Real	  Zero offset for x image
*       yim(npixy,nliny) I*2	  Image for y ayis
*       npixy,nliny	 Integer  Size of y image
*       invaly		 Integer  Flag for invalid piyels in y image
*       scaley		 Real	  Scale factor for y image
*       zeroy            Real	  Zero offset for y image
*	work(2,wksize)	 Real	  Work space to hold plot coords
*	wksize		 Integer  Size of work array
*	xlims(2)	 Real	  Limits of X axis
*	ylims(2)	 Real	  Limits of Y axis
*   OUTPUTS:
*       (none)
*
*SUBROUTINES CALLED
*       EDRS:
*              wrerr
*       GKS
*	       gpm,gsasf,gqasf,gsmk
*              
*STARLINK PARAMETERS
*	NOPOINTS(error)	Accessed if there are no data points to plot
*
*VAy SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/11/89
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	npixx,nlinx,invalx,npixy,nliny,invaly,wksize
      integer*2	xim(npixx,nlinx),yim(npixy,nliny)      
      real	scalex,zerox,scaley,zeroy,work(2,wksize),xlims(2),
     :          ylims(2)
      
*
* DECLARE LOCAL VARIABLES
*
      integer	count	! No. of points to plot
      integer	ierr	! Error status
      integer	jasf(13)! GKS aspect source flags
      integer	lin	! Image line counter
      integer	pix	! Image pixel counter
      real	x	! Scaled pixel value for X axis
      real	y	! Scaled pixel value for Y axis

*
* INCLUDE INFO ABOUT GRAPHICS DEVICE
*
      include 'UTILITIES(GR_COM)'

*
* INITIALISE COUNT OF PLOTABLE POINTS
*
      count=0

*
* LOOP ROUND ALL PIXELS IN THE OVERLAP REGION OF THE TWO IMAGES
*
      do lin=1,min(nlinx,nliny)
         do pix=1,min(npixx,npixy)

*
* IF EITHER IMAGE IS INVALID AT THE CURRENT PIXEL, THEN DON'T PLOT IT
*
            if(xim(pix,lin).ne.invalx.and.
     :         yim(pix,lin).ne.invaly) then

*
* CALCULATE THE SCALED PIXEL VALUES
*
               x=scalex*xim(pix,lin)+zerox
               y=scaley*yim(pix,lin)+zeroy

*
* STORE THE SCALED PIXEL VALUES IF THEY ARE WITHIN THE PLOT RANGE
*
               if(x.ge.xlims(1).and.x.le.xlims(2).and.
     :            y.ge.ylims(1).and.y.le.ylims(2)) then
                  count=count+1
                  work(1,count)=x
                  work(2,count)=y
               endif
            endif

*
* DO NEXT PIXEL
*
         enddo
      enddo

*
* PLOT POINTS USING FULL STOPS AS MARKERS (GKS MARKER TYPE 1)
*
      if(count.gt.0) then
         call gqasf(ierr,jasf)
         jasf(4)=1
         call gsasf(jasf)
         call gsmk(1)
         call gpm(count,work(1,1),work(2,1))
      else
         call wrerr('NOPOINTS')
      endif

*
* FINISH
*
      end
