      subroutine repinv(in,npix,nlin,inval,out,outval)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Changes all pixels with a given value to another value.
*
*SOURCE
*       REPINV.FOR in UTILITIES.TLB
*
*METHOD
*       I*2 images are used.
*
*ARGUMENTS
*   INPUTS:
*       in(npix,nlin)   Integer*2       The input image
*       npix,nlin       integers        The dimensions of i/p and o/p
*       inval           integer         The pixel value to be replaced
*       outval          integer         The value to replace them with
*   OUTPUTS:
*       out(npix,nlin)  Integer*2       The output image
*
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
*       D.S. Berry (MAVAD::DSB) 10/4/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval,outval
      integer*2 in(npix,nlin),out(npix,nlin)
*
* DECLARE LOCAL VARIABLES
*
      integer   lin     ! Line counter
      integer   pix     ! Pixel counter
      integer   pixval  ! Current pixel value
*
* JUST DO IT
*
      do lin=1,nlin
         do pix=1,npix
            pixval=in(pix,lin)
            if(pixval.eq.inval) then
               out(pix,lin)=outval
            else
               out(pix,lin)=pixval
            endif
         enddo
      enddo
*
* FINISH
*
      end
