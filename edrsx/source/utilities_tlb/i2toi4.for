      subroutine i2toi4(in,npix,nlin,scale,zero,inval,out,invalo,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Convert an I*2 image to an I*4 image including scaling, and
*       replacing invalid pixels in the input with a specified value.
*
*SOURCE
*       I2TOI4.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       npix,nlin       integers        Dimensions of arrays in and out
*       in(npix,nlin)   I*2             Input image
*       scale           real            Scale factor for input data
*       zero            real            Zero offset for input data
*       inval           integer         Invalid pixel value in input
*       invalo          integer         Invalid pixel value in output
*   OUTPUTS:
*       out(npix,nlin)  integer         Output array
*       ierr            integer         Error status 0 - success
*                                                    1 - No valid data
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/5/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval,invalo,ierr
      integer   out(npix,nlin)
      integer*2 in(npix,nlin)
      real      scale,zero
*
* DECLARE LOCAL VARIABLES
*
      integer   line    ! Line counter
      integer   pixel   ! Pixel counter
*
* JUST DO IT
*
      ierr=1
      do line=1,nlin
         do pixel=1,npix
            if(in(pixel,line).ne.inval) then
               out(pixel,line)=scale*in(pixel,line)+zero
               ierr=0
            else
               out(pixel,line)=invalo
            endif
         enddo
      enddo
*
* FINISH
*
      end
