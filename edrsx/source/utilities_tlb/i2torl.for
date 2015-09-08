      subroutine i2torl(iin,npix,nlin,scale,zero,inval,rinval,rout)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To copy an integer*2 image to a real image including scaling.
*
*SOURCE
*       I2TORL.FOR in UTILITIES.TLB
*
*
*ARGUMENTS
*   INPUTS:
*       npix,nlin       integer The size of the images
*       iin(npix,nlin)  integer*2       The I*2 input image
*       inval           integer The flag for invalid pixels in iin
*       scale           real    The scale factor for converting from
*                               integer to real data representation
*       zero            real    The zero offset for converting from
*                               integer to real data representation
*       rinval          real    The flag for invalid pixels in rout
*   OUTPUTS:
*       rout(npix,nlin) real    The output real image
*
*USED BY
*       CONVOLVE
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
*       D.S. Berry (MAVAD::DSB) 13/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   nlin,npix,inval
      integer*2 iin(npix,nlin)
      real      rout(npix,nlin),scale,zero,rinval
*
* DECLARE LOCAL VARIABLES
*
      integer   lin     ! Image line counter
      integer   pix     ! Image pixel counter
*
* JUST DO IT
*
      do lin=1,nlin
         do pix=1,npix
            if(iin(pix,lin).ne.inval) then
               rout(pix,lin)=scale*iin(pix,lin)+zero
            else
               rout(pix,lin)=rinval
            endif
         enddo
      enddo
*
* FINISH
*
      end
