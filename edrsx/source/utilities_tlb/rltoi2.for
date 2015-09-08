      subroutine rltoi2(iout,rin,npix,nlin,inval,rinval,scale,zero)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To copy a real image to an integer*2 image scaling the data
*       values at the same time.
*
*SOURCE
*       RLTOI2.FOR in UTILITIES.TLB
*
*METHOD
*       The input data values are scaled unless they have the value of
*       the rinval argument. In this case the output data value is
*       given the inval value.
*               If the value of scale is zero, then it is changed
*       to 1 and all integer values set to 0.
*
*ARGUMENTS
*   INPUTS:
*       npix,nlin       integer The size of the images
*       rin(npix,nlin)  real    The real input image
*       inval           integer The flag for invalid pixels in output
*       rinval          real    The flag for invalid pixels in input
*       scale           real    The scale factor for converting from
*                               real to integer data representation
*       zero            real    The zero offset for converting from
*                               real to integer data representation
*   OUTPUTS:
*       iout(npix,nlin) integer*2 The output integer*2 image
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
*       D.S. Berry (MAVAD::DSB) 10/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   nlin,npix,inval
      integer*2 iout(npix,nlin)
      real      rin(npix,nlin),scale,zero,rinval
*
* DECLARE LOCAL VARIABLES
*
      integer   lin     ! Image line counter
      integer   pix     ! Image pixel counter
      real      rscale  ! Reciprocal of scale factor
*
* JUST DO IT
*
      if(scale.ne.0) then
         rscale=1.0/scale
      else
         rscale=0
         scale=1
      endif
      do lin=1,nlin
         do pix=1,npix
            if(rin(pix,lin).ne.rinval) then
               iout(pix,lin)=nint((rin(pix,lin)-zero)*rscale)
            else
               iout(pix,lin)=inval
            endif
         enddo
      enddo
*
* FINISH
*
      end
