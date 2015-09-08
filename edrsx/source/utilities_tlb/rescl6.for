      subroutine rescl6(iin,npix,nlin,bscale,bzero,blank,iout,pinval,
     :                  allrej)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To copy values from an unsigned byte array into a signed word array,
*       replacing FITS blank pixels by EDRS invalid pixels.
*
*SOURCE
*       RESCL6.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       iin     byte array      The input image
*       npix    integer         No. of pixels in each line of the input
*       nlin    integer         No. of lines in the input
*       bscale  real            Scale factor (redundant)
*       bzero   real            Zero level (redundant)
*       blank   real            Blank pixel value
*       pinval  logical         If true, then there are some blank pixels
*   OUTPUTS:
*       iout    integer*2 array The output image
*       allrej  logical         Set true if there are no valid pixels in input
*
*USED BY
*       EDRSIN
*
*SUBROUTINE CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               CVUBSW
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/8/87
*-------------------------------------------------------------------------
*
      parameter (inval=-32767)
      byte      iin(npix,nlin)
      integer*2 iout(npix,nlin),cvubsw
      logical   pinval,allrej
*
* CONVERT UNSIGNED BYTE INPUTS TO SIGNED WORD AND COPY VALUES TO OUTPUT,
* REPLACING BLANK PIXELS BY EDRS INVALID PIXELS
*
      allrej=.true.
      do j=1,nlin
         do i=1,npix
            ival=cvubsw(iin(i,j))
            if(ival.eq.blank.and.pinval) then
               iout(i,j)=inval
            else
               iout(i,j)=ival
               allrej=.false.
            endif
         enddo
      enddo
      blank=inval
      end
