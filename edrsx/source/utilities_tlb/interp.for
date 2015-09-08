      subroutine interp(data,npix,nlin,inval,x,y,value)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the value of an image at a fractional pixel location
*       using bi-linear interpolation.
*
*SOURCE
*       INTERP.FOR in UTILITIES.TLB
*
*METHOD
*       The output value is the weighted sum of the 4 pixels closest
*       to the requested pixel location. The weights are proportional
*       to the fraction of each pixel lying within an identical pixel
*       centred at the given fractional position. This method works
*       ok for pixels which cover non-square areas on the sky.
*
*ARGUMENTS
*   INPUTS:
*       data(npix,nlin) integer*2       The image to be interpolated
*       npix,nlin       integer         The size of the image in pixels
*                                       and lines
*       inval           integer         The flag for invalid pixels
*       x               real            The fractional x pixel location
*                                       at which the value of data is
*                                       required.
*       y               real            The fractional y pixel location.
*   OUTPUTS:
*       value           real            The (fractional) value of data
*                                       at the requested location. This
*                                       will take the value of the
*                                       argument inval, if the data is
*                                       invalid at the that location.
*
*USED BY
*       SIMCRDD
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/1/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval
      integer*2 data(npix,nlin)
      real      x,y,value
*
* DECLARE LOCAL VARIABLES
*
      real      dx      ! Shift in x from given position to next lower
                        ! pixel
      real      dy      ! Shift in y from given position to next lower
                        ! line
      integer   i       ! Pixel counter
      integer   j       ! Line counter
      real      sum     ! Sum of weighted input pixel values
      real      wt      ! Wieght for input pixel value
      real      wtsum   ! Sum of weights
      integer   xcen    ! Centre of nearest pixel in x
      integer   ycen    ! Centre of nearest pixel in y
*
* FIND NEAREST PIXEL LOCATION
*
      xcen=nint(x)
      ycen=nint(y)
*
* IF NEAREST PIXEL LIES OUTSIDE INPUT IMAGE, OUTPUT VALUE IS INVALID
* OTHERWISE CONTINUE WITH INTERPOLATION
*

      if((xcen.lt.1).or.(xcen.gt.npix).or.(ycen.lt.1).or.
     :   (ycen.gt.nlin)) then
         value=inval
      else
*
* OUTPUT VALUE IS INVALID IF NEAREST INPUT PIXEL IS INVALID. OTHERWISE
* CONTINUE WITH INTERPOLATION
*
         if(data(xcen,ycen).eq.inval) then
            value=inval
         else
*
* FIND SHIFT FROM NEXT LOWEST PIXEL,LINE LOCATION
*
            i=x
            j=y
            dx=x-i
            dy=y-j
*
* INITIALLISE SUMS FOR FORMING WEIGHTED MEAN
*
            sum=0.0
            wtsum=0.0
*
* FORM WEIGHTED MEAN OF ADJACENT 4 PIXELS, CHECKING THAT EACH LIES
* WITHIN THE INPUT IMAGE AND IS NOT INVALID
*
            if(j.ge.1) then
               if(i.ge.1) then
                  if(data(i,j).ne.inval) then
*
* WEIGHT IS CALCULATED FROM THE X,Y SHIFT FROM INTEGER PIXEL LOCATIONS
*
                     wt=(1.0-dx)*(1.0-dy)
                     sum=sum+data(i,j)*wt
                     wtsum=wtsum+wt
                  endif
               endif
               if(i+1.le.npix) then
                  if(data(i+1,j).ne.inval) then
                     wt=dx*(1.0-dy)
                     sum=sum+data(i+1,j)*wt
                     wtsum=wtsum+wt
                  endif
               endif
            endif
            if(j+1.le.nlin) then
               if(i.ge.1) then
                  if(data(i,j+1).ne.inval) then
                     wt=(1.0-dx)*dy
                     sum=sum+data(i,j+1)*wt
                     wtsum=wtsum+wt
                  endif
               endif
               if(i+1.le.npix) then
                  if(data(i+1,j+1).ne.inval) then
                     wt=dx*dy
                     sum=sum+data(i+1,j+1)*wt
                     wtsum=wtsum+wt
                  endif
               endif
            endif
*
* ASSIGN WEIGHTED MEAN TO OUTPUT PIXEL (WTSUM CANNOT BE ZERO, SINCE
* AT LEAST 1 INPUT PIXEL MUST BE VALID)
*
            value=sum/wtsum
         endif
      endif
*
* FINISH
*
      end
