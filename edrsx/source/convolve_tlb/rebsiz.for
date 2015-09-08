      subroutine rebsiz(c,nlini,npixi,npixo,nlino,xcen,ycen,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the size of an output image formed by rebining an
*       input image using a linear transformation and then
*       shifting it so that it lies exactly in the bottom left
*       corner of the output image. Also returns the position in
*       output image of pixel (1,1) in the transformed input image.
*
*SOURCE
*       REBSIZ.FOR in CONVOLVE.TLB
*
*METHOD
*       The inverse transformation is calculated and used to find
*       the co-ordinates of the input image corners in the transformed
*       input image. A shift is then applied to the transformed
*       input image to make it lie in the bottom left corner of the
*       output image and the given transformation co-efficients modified
*       to represent the transformation from this output image to the
*       input image.
*
*ARGUMENTS
*   INPUTS:
*       c(6)    real    The transformation co-efficients from
*                       transformed input to input
*       npixi   integer No. of pixels in each line of input image
*       nlini   integer No. of lines in input image
*   OUTPUTS:
*       npixo   integer No. of pixels in each line of output image
*       nlino   integer No. of lines in output image
*       xcen    integer The pixel in the output image corresponding to
*                       to pixel (1,1) of the transformed input image
*       ycen    integer The line in the output image corresponding to
*                       to pixel (1,1) of the transformed input image
*       c(6)    real    The transformation co-efficients from output to
*                       input
*       ierr    integer Error status: 0 - Success
*                                     1 - Transformation is singular
*                                               (Reported)
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               invert,hint,lint
*
*STARLINK PARAMETERS
*       SINGULAR/error/ Accessed if the given transformation cannot
*                       be inverted
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 21/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npixi,nlini,npixo,nlino,xcen,ycen,ierr
      real      c(6)
*
* DECLARE LOCAL VARIABLES
*
      real      d(6)    ! The inverse of the given transformation
      integer   hint    ! A function returning next higher integer
      integer   lint    ! A function returning next lower integer
      real      x1      ! The x co-ord of bottom left corner of input
                        ! image in output image
      real      x2      ! The x co-ord of top left corner of input
                        ! image in output image
      real      x3      ! The x co-ord of top right corner of input
                        ! image in output image
      real      x4      ! The x co-ord of bottom right corner of input
                        ! image in output image
      integer   xmax    ! Max x co-ord of transformed input image
      integer   xmin    ! Min x co-ord of transformed input image
      real      y1      ! The y co-ord of bottom left corner of input
                        ! image in output image
      real      y2      ! The y co-ord of top left corner of input
                        ! image in output image
      real      y3      ! The y co-ord of top right corner of input
                        ! image in output image
      real      y4      ! The y co-ord of bottom right corner of input
                        ! image in output image
      integer   ymax    ! Max y co-ord of transformed input image
      integer   ymin    ! Min y co-ord of transformed input image
*
* CALCULATE THE INVERSE TRANSFORMATION FROM INPUT TO OUTPUT
*
      call invert(c,d,ierr)
      if(ierr.ne.0) then
         call wrerr('SINGULAR')
         goto 999
      endif
*
* CALCULATE POSITIONS OF CORNERS OF TRANSFORMED INPUT IMAGE IN OUTPUT
* IMAGE
*
      x1=d(1)+d(2)+d(3)
      x2=d(1)+d(2)+d(3)*nlini
      x3=d(1)+d(2)*npixi+d(3)*nlini
      x4=d(1)+d(2)*npixi+d(3)
      y1=d(4)+d(5)+d(6)
      y2=d(4)+d(5)+d(6)*nlini
      y3=d(4)+d(5)*npixi+d(6)*nlini
      y4=d(4)+d(5)*npixi+d(6)
*
* FIND THE EXTREMAL LIMITS OF THE TRANSFORMED INPUT IMAGE IN THE OUTPUT
* IMAGE
*
      xmin=lint(min(x1,x2,x3,x4))
      xmax=hint(max(x1,x2,x3,x4))
      ymin=lint(min(y1,y2,y3,y4))
      ymax=hint(max(y1,y2,y3,y4))
*
* CALCULATE THE SIZE OF THE OUTPUT IMAGE WHICH WILL JUST ENCLOSE THE
* TRANSFORMED INPUT IMAGE
*
      npixo=xmax-xmin+1
      nlino=ymax-ymin+1
*
* MODIFY THE TRANSFORMATION SO THAT IT REPRESENTS THE TRANSFORMATION
* FROM INPUT IMAGE TO OUTPUT IMAGE
*
      d(1)=d(1)-xmin+1
      d(4)=d(4)-ymin+1
      call invert(d,c,ierr)
*
* CALCULATE THE POSITION OF PIXEL (1,1) OF THE TRANSFORMED IMAGE IN THE
* OUTPUT IMAGE
*
      xcen=2-xmin
      ycen=2-ymin
*
* FINISH
*
  999 continue

      end
