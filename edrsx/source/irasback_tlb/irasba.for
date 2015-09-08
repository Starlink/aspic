      subroutine irasba
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Calculates the IRAS descriptors of an output image which
*       would include the areas covered by a set of input images.
*       These descriptors are added to a dummy file containing
*       just one pixel. This file can then be used as a background
*       image to which the input images can be aligned using IRASCOEF
*       and IRASSHIFT. This will result in none of the input images
*       going of the bottom or left of the final stacked image.
*
*SOURCE
*       IRASBA.FOR in IRASBACK.TLB
*
*METHOD
*       Get the IRAS descriptors from the first input image.
*       Loop round all the other input images. For each one,
*       calculate the transformation from itself to the first
*       input image. Use this transformation to find out where
*       the pixel (1,1) of each input image lies in the frame
*       of the first input image. Find the minimum of such x
*       and y coords over all input images. The dummy output
*       image is considered to have its bottom left corner
*       (pixel 1,1) at the minimum  x and y. Calculate where
*       the reference pixel of the first input image lies in
*       the reference frame of the output image. Copy the
*       IRAS descriptors from the first input image to the output
*       changing CRPIX1 and CRPIX2 to be the coords of the reference
*       pixel in the reference frame of the output image.
*
*ARGUMENTS
*       none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              rdimds,wrimds,irastr,gt2diw,wrerr
*       EDRS:
*              lbgone,ptdscr,imgset
*       INTERIM:
*              frdata
*STARLINK PARAMETERS
*       IMAGE1/read/    First input image
*       ......
*       ......
*       IMAGE20/read/   20th input image
*       OUTPUT/read/    Output image
*       NOCOMB/error/
*       NOCOMB/error/   Accessed if any input was produced by COMBINE
*                       but has non-zero CROTA1
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 14/7/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      real      c(6)    ! Transformation co-efficients
      real      cdlt1a  ! The size (in x) of a pixel in 1st image in degrees
      real      cdlt2a  ! The size (in y) of a pixel in 1st image in degrees
      real      cdlt1b  ! The size (in x) of a pixel in nth image in degrees
      real      cdlt2b  ! The size (in y) of a pixel in nth image in degrees
      real      crot1a  ! Clockwise angle from north to 1st image -ve Y axis
      real      crot1b  ! Clockwise angle from north to nth image -ve Y axis
      integer   crpx1a  ! The x value of the reference pixel in 1st image
      integer   crpx2a  ! The y value of the reference pixel in 1st image
      integer   crpx1b  ! The x value of the reference pixel in nth image
      integer   crpx2b  ! The y value of the reference pixel in nth image
      real      crvl1a  ! The RA (in degrees, +/- 180) of 1st image ref pixel
      real      crvl2a  ! The DEC (in degrees, +/- 90) of 1st image ref pixel
      real      crvl1b  ! The RA (in degrees, +/- 180) of nth image ref pixel
      real      crvl2b  ! The DEC (in degrees, +/- 90) of nth image ref pixel
      character cval*1  ! Dummy character argument
      integer   i       ! Implicit loop count
      integer   ierr    ! Status return
      integer   ilevel  ! Level of information to display on screen
      character instra*30 ! INSTRUME descriptor from 1st image
      character instrb*30 ! INSTRUME descriptor from nth image
      integer   ipout   ! Pointer to output image
      integer   ival    ! Dummy integer argument
      integer   n       ! No. of input image currently being processed
      character name*7  ! Buffer for input image parameter name
      character prbuf*80! Buffer for text destined for users screen
      real      rval    ! dummy real argument
      real      xmin    ! Minimum x coords of bottom left corners
      real      ymin    ! Minimum x coords of bottom left corners

*
* GET THE NECESSARY DESCRIPTORS FROM THE FIRST IMAGE (SEE LOCAL VARIABLE
* DECLARATIONS FOR A DESCRIPTION OF EACH)
*
      call rdimds('IMAGE1',.false.,crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,
     :             cdlt2a,crot1a,instra,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) goto 999

*
* LOOP UNTIL A NULL IMAGE IS ENTERED OR 20TH IMAGE REACHED
*
      xmin=1
      ymin=1
      n=2
  10  continue

*
* GET THE NECESSARY DESCRIPTORS FROM nth image
*
      write(name,20) n
  20  format('IMAGE',I2)
      call lbgone(name(6:))
      call rdimds(name,.true.,crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,
     :                  cdlt2b,crot1b,instrb,ierr)
      if(ierr.eq.-1) then
         call wrerr('NOCOMB')
         goto 999
      endif
      if(ierr.eq.0) then

*
* CALL IRASTR TO CALCULATE THE REQUIRED TRANSFORMATION CO-EFFICIENTS
*
         call irastr(crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,cdlt2b,crot1b,
     :               crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,cdlt2a,crot1a,c)

*
* CALCULATE COORDS OF PIXEL (1,1) OF NTH INPUT IN THE FRAME OF INPUT 1
* AND UPDATE MIN VALUES
*
         xmin=min(xmin,c(1)+c(2))
         ymin=min(ymin,c(4)+c(6))

*
* GO ROUND FOR NEXT INPUT
*
         n=n+1
         if(n.lt.20) goto 10
      endif

*
* WHEN A NULL ENTRY IS MADE OR N EXCEEDS 20 ARRIVE HERE
* CALCULATE COORDS OF REFERENCE PIXEL FROM 1ST INPUT IN A FRAME
* IN WHICH THE MINIMUM BOTTOM LEFT CORNER COORDS ARE (1,1)
*
      crpx1a=crpx1a+1.0-xmin
      crpx2a=crpx2a+1.0-ymin

*
* AQUIRE AN OUTPUT IMAGE HOLDING ONLY ONE PIXEL OF VALUE ZERO
*
      call gt2diw('OUTPUT',102,.false.,1,1,ipout,ierr)
      if(ierr.eq.0) then
         call imgset(%val(ipout),1,1,0)

*
* WRITE DESCRIPTORS DESCRIBING IMAGE SIZE
*
         call ptdscr('OUTPUT','NAXIS','INTEGER',2,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS1','INTEGER',1,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS2','INTEGER',1,rval,cval,ierr)
         call ptdscr('OUTPUT','BSCALE','REAL',ival,1.0,cval,ierr)
         call ptdscr('OUTPUT','BZERO','REAL',ival,0.0,cval,ierr)
         call ptdscr('OUTPUT','INVAL','INTEGER',-32767,rval,cval,ierr)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,
     :               'Output from IRASBACK',ierr)
         call ptdscr('OUTPUT','INSTRUME','CHARACTER',ival,rval,
     :               'SURVEY',ierr)

*
* ADD IRAS DESCRIPTORS
*
         call wrimds('OUTPUT',crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,
     :                cdlt2a,crot1a,ierr)

      endif

*
* FINISH
*
  999 call frdata(' ',ierr)

      end
