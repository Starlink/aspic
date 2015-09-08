      subroutine edtoir(in,out,npix,nlin,inval,blank,bscale,bzero)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Copies an I*2 image to an I*4 image, adding an offset to
*       the integers to ensure that the output value of BZERO is
*       zero. Also replaces INVALid pixels by BLANK pixels.
*
*SOURCE
*       EDTOIR.FOR in IRASIN.TLB
*
*ARGUMENTS
*   INPUTS:
*       in(npix,nlin)   I*2     Input image
*       npix,nlin       integer Image dimensions
*       inval           integer Value stored at invalid pixels
*       blank           integer Value stored at blank pixels
*       bscale          real    Scale factor for input image
*       bzero           real    Zero offset for input image
*   OUTPUTS:
*       out(npix,nlin)  I*4     Output image
*       bscale          real    Scale factor for output image
*       bzero           real    Zero offset for output image
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
*       D.S. Berry (MAVAD::DSB) 28/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval,blank,out(npix,nlin)
      integer*2 in(npix,nlin)
      real      bscale,bzero
*
* DECLARE LOCAL VARIABLES
*
      real      factor  ! A factor to multiply the input integers by
                        ! before adding the offset
      integer   lin     ! Line counter
      integer   offset  ! The value to be added to the input integers to
                        ! ensure that the output bzero value is zero
      integer   pix     ! Pixel counter
*
* CALCULATE THE VALUE WHICH MUST BE ADDED TO THE INPUT UNSCALED INTEGERS
* TO MAKE THE VALUE OF BZERO IN THE OUTPUT, ZERO.
*
      if(bscale.ne.0) then
         offset=nint(bzero/bscale)
         factor=1.0
      else
         bscale=bzero
         offset=1
         factor=0.0
      endif
*
* SET OUTPUT BZERO TO 0
*
      bzero=0.0
*
* COPY THE INPUT TO THE OUTPUT
*
      do lin=1,nlin
         do pix=1,npix
            if(in(pix,lin).ne.inval) then
               out(pix,lin)=factor*in(pix,lin)+offset
            else
               out(pix,lin)=blank
            endif
         enddo
      enddo
*
* FINISH
*
      end
