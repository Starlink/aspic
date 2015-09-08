      subroutine irascr
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Modifies the descriptors of an IRAS image to take into account
*       the effect of a linear transformation on the orientation of the
*       scan, the location of the reference pixel and the pixel sizes.
*       This program should be run after RESAMPLE or BIN (for instance)
*       to ensure that other IRAS software will position the image
*       correctly on the sky.
*
*SOURCE
*       IRASCR.FOR in IRASCORR.TLB
*
*METHOD
*       The reference pixel of the shifted image is set to be the middle
*       pixel in the shifted image. The location of that pixel in the
*       unshifted image is then found by using the linear transformation
*       provided by the user (the one used to shift the image). The
*       descriptors in the unshifted image are then used to calculate
*       the ra and dec of the shifted image reference pixel. These are
*       stored as the descriptor values CRVAL1 and CRVAL2 for the
*       shifted image.
*               The scan orientation is found by applying the inverse
*       transformation to a point in the unshifted image positioned
*       above the location of the shifted image reference pixel. The
*       angle between the y axis and the line joining the transformed
*       point to the reference point in the shifted image, is added onto
*       the scan orientation of the unshifted image to get the scan
*       orientation of the shifted image.
*               The pixel size along x in the shifted image is found
*       by transforming the point which is one pixel further along the
*       line from the reference pixel, and finding the distance between
*       the transformed point and the transformed reference pixel using
*       the unshfted image descriptors. The size of a pixel along y is
*       found in a similar way using the pixel above the reference
*       pixel instead.
*
*ARGUMENTS
*       none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gt2did,rdimds,wrerr,xytord,wrimds
*       EDRS:
*               gtdscr
*       INTERIM:
*               rdkeyr
*
*STARLINK PARAMETERS
*       OUTPUT/read,write/      The shifted image
*       INPUT/read/             The unshfted image
*       TRCOEFFS/read/          The transformation co-efficients
*       NOCHANGE/error/         Accessed if the shifted image descriptors
*                               are not changed.
*       NOCOMB/error/           Accessed if the image to be corrected
*                               was created by COMBINE but has not got
*                               a CROTA1 value of zero.
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 20/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      real      c(6)    ! Transformation co-efficients
      real      cdlt1a  ! CDELT1 value for shifted image
      real      cdlt2a  ! CDELT2 value for shifted image
      real      cdlt1b  ! CDELT1 value for unshifted image
      real      cdlt2b  ! CDELT2 value for unshifted image
      real      crotaa  ! CROTA1 value for shifted image
      real      crotab  ! CROTA1 value for unshifted image
      integer   crpx1a  ! CRPIX1 value for shifted image
      integer   crpx2a  ! CRPIX2 value for shifted image
      integer   crpx1b  ! CRPIX1 value for unshifted image
      integer   crpx2b  ! CRPIX2 value for unshifted image
      real      crvl1a  ! CRVAL1 value for shifted image
      real      crvl2a  ! CRVAL2 value for shifted image
      real      crvl1b  ! CRVAL1 value for unshifted image
      real      crvl2b  ! CRVAL2 value for unshifted image
      character cval*1  ! Dummy character argument
      integer   ierr    ! Error status
      character instrb*30 ! Value of INSTRUME descriptor from input
      integer   ival    ! Dummy integer argument
      real      midxb   ! X centre of image a ref. pixel in input
      real      midyb   ! Y centre of image a ref. pixel in input
      integer   nlin    ! No. of lines in shifted image
      integer   npix    ! No. of pixels in a line in shifted image
      real      rval    ! Dummy real argument
*
* SET UP PARAMETER ASSOCIATION FOR SHIFTED IMAGE AND GET IMAGE SIZE
*
      call gt2did('OUTPUT',.false.,ierr)
      if(ierr.ne.0) goto 999
      call gtdscr('OUTPUT','NAXIS1','INTEGER',npix,rval,cval,ierr)
      call gtdscr('OUTPUT','NAXIS2','INTEGER',nlin,rval,cval,ierr)
*
* GET THE IMAGE IRAS DESCRIPTORS FROM UNSHIFTED IMAGE
*
      call rdimds('INPUT',.false.,crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,
     :             cdlt2b,crotab,instrb,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) goto 999
*
* GET THE TRANSFORMATION CO-EFFICIENTS WHICH PRODUCED THE SHIFT
*
      call rdkeyr('TRCOEFFS',.false.,6,c,ival,ierr)
      if(ierr.ne.0) then
         call wrerr('NOCHANGE')
         goto 999
      endif
*
* THE SHIFTED IMAGE REFERENCE PIXEL IS THE CENTRE PIXEL
*
      crpx1a=int(0.5*(npix+1))
      crpx2a=int(0.5*(nlin+1))
*
* SEE WHERE THIS PIXEL WAS IN THE UNSHIFTED IMAGE
*
      midxb=c(1)+c(2)*crpx1a+c(3)*crpx2a
      midyb=c(4)+c(5)*crpx1a+c(6)*crpx2a
*
* FIND THE RA AND DEC OF THIS PIXEL POSITION USING THE DESCRIPTORS IN
* THE UNSHIFTED IMAGE. THESE ARE THE VALUES OF CRVAL1 AND CRVAL2 IN THE
* SHIFTED IMAGE.
*
      call xytord(midxb,midyb,crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,cdlt2b,
     :            crotab,crvl1a,crvl2a)
*
* CALCULATE THE X SIZE OF A PIXEL OF THE SHIFTED IMAGE IN DEGREES OF ARC
*
      cdlt1a=sqrt(cdlt1b*cdlt1b*c(2)*c(2)+cdlt2b*cdlt2b*c(5)*c(5))
      cdlt2a=sqrt(cdlt1b*cdlt1b*c(3)*c(3)+cdlt2b*cdlt2b*c(6)*c(6))
*
* CALCULATE THE ANTI-CLOCKWISE ANGLE BETWEEN NORTH AND THE NEGATIVE
* Y AXIS FOR THE SHIFTED IMAGE
*
      crotaa=crotab+atan2d(c(3),c(2))
      if(crotaa.lt.0) crotaa=crotaa+360.0
      if(crotaa.ge.360) crotaa=crotaa-360.0
*
* THE VALUE OF CROTA1 STORED IN IMAGEA WILL USE THE DEFINITION USED BY
* CRDIMAGE (CLOCKWISE ANGLE FROM NORTH TO -VE Y AXIS). THEREFORE ENSURE
* THAT THE FILE IS RECOGNIZED AS A CRDIMAGE OUTPUT.
*
      call ptdscr('OUTPUT','INSTRUME','CHARACTER',ival,rval,'SURVEY',
     :             ierr)
*
* WRITE OUT THE NEW VALUES FOR THE SHIFTED IMAGE DESCRIPTORS
*
      call wrimds('OUTPUT',crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,cdlt2a,
     :             crotaa,ierr)
*
* FINISH
*
  999 continue

      end
