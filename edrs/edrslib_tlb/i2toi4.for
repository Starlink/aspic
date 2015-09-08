      subroutine i2toi4(in,npix,nlin,scale,zero,inval,out,npixo,nlino,
     :                  invalo,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Convert an I*2 image to an I*4 image including scaling, and
*       replacing invalid pixels in the input with a specified value.
*       The input and output arrays can be different sizes, in which 
*       case the input image is magnified using nearest neighbour 
*       interpolation.
*
*ARGUMENTS
*   INPUTS:
*       npix,nlin       integers        Dimensions of input image.
*       in(npix,nlin)   I*2             Input image
*       scale           real            Scale factor for input data
*       zero            real            Zero offset for input data
*       inval           integer         Invalid pixel value in input
*       invalo          integer         Invalid pixel value in output
*       npixo,nlino     integer         Dimensions of output image.
*   OUTPUTS:
*       out(npixo,nlino)integer         Output array
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
      integer   npix,nlin,inval,invalo,npixo,nlino,ierr
      integer   out(npixo,nlino)
      integer*2 in(npix,nlin)
      real      scale,zero
*
* DECLARE LOCAL VARIABLES
*
      real	c1,c2,c4,c6 ! Coefficients of output to input transformation.
      integer   ilin    ! Input line number
      integer   ipix    ! Input pixel number
      integer   line    ! Output line number
      integer   pixel   ! Output pixel number

*
* CALCULATE THE COEFFECIENTS REQUIRED TO PRODUCE A MAGNIFICATION WHICH 
* WILL PRODUCE AN OUTPUT IMAGE OF THE SPECIFIED SIZE.
*
      c2 = real(npix)/real(npixo)
      c1 = 0.5*(1.0-c2)      
      c6 = real(nlin)/real(nlino)
      c4 = 0.5*(1.0-c6)      

*
* LOOP ROUND EACH PIXEL IN THE OUTPUT IMAGE.
*
      ierr=1
      do line=1,nlino
         do pixel=1,npixo

*
* CALCULATE THE CORRESPONDING INPUT PIXEL.
*
            ipix = nint( c1+c2*pixel )
            ilin = nint( c4+c6*line )
*
* IF THE INPUT PIXEL IS NOT INVALID STORE IT IN THE OUTPUT PIXEL.
*
            if(in(ipix,ilin).ne.inval) then
               out(pixel,line)=scale*in(ipix,ilin)+zero
               ierr=0
*
* OTHERWISE STORE THE SUPPLIED OUTPUT INVALID FLAG VALUE.
*
            else
               out(pixel,line)=invalo
            endif

         enddo
      enddo
*
* FINISH
*
      end
