      subroutine prepim(iin,npixi,nlini,inval,scale,zero,out,npixo,
     :                  nlino,fill,flags)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To copy an integer*2 image to a REAL*4 image including
*       scaling, and replace invalid pixels by value given by argument
*       'fill'. If the input image is not as big as the output, it is
*       padded with pixels with value of argument 'fill'. If no input
*       image is given (as signalled by argument FLAGS), then the output
*       image is filled with zeros.
*
*SOURCE
*       PREPIM.FOR in FOURIER.TLB
*
*ARGUMENTS
*   INPUTS:
*       npixi,nlini     integer The size of the input image
*       iin(npixi,nlini)integer*2     The I*2 input image
*       inval           integer The flag for invalid pixels in iin
*       fill            real    The value to fill data 'holes' with
*       scale           real    The scale factor for converting from
*                               integer to real data representation
*       zero            real    The zero offset for converting from
*                               integer to real data representation
*       npixo,nlino     integer The size of the output image
*       flags           integer If =1 then output image is filled with
*                               zeros
*   OUTPUTS:
*       out(npixo,npixo) real   The output real image
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
*       D.S. Berry (MAVAD::DSB) 18/4/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer   nlini,npixi,inval,npixo,nlino,flags
      integer*2 iin(npixi,nlini)
      real      scale,zero,fill
      real      out(npixo,nlino)

*
* DECLARE LOCAL VARIABLES
*
      integer   lin     ! Output image line counter
      integer   pix     ! Output image pixel counter

*
* JUMP FORWARD IF OUTPUT IS TO BE FILLED WITH ZEROS
*
      if(flags.eq.0) then

         do lin=1,nlino
            do pix=1,npixo

               if(pix.le.npixi.and.lin.le.nlini) then

                  if(iin(pix,lin).ne.inval) then
                     out(pix,lin)=scale*iin(pix,lin)+zero

                  else
                     out(pix,lin)=fill
                  endif

               else
                  out(pix,lin)=fill
               endif

            enddo
         enddo

*
* NOW DEAL WITH CASE WHERE OUTPUT IS FILLED WITH ZEROS
*
      else
         do lin=1,nlino
            do pix=1,npixo
               out(pix,lin)=0
            enddo
         enddo
      endif

*
* FINISH
*
      end
