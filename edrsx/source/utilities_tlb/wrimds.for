      subroutine wrimds(image,crval1,crval2,crpix1,crpix2,cdelt1,cdelt2,
     +                  crota1,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Updates the descriptors describing the position on the sky of
*       an IRAS image. It is assumed that the parameter association has
*       already been set up. If it hasn't, this routine will set it up
*       by prompting the user, etc, but no error checking will be
*       performed. GT2DID should be called before WRIMDS to set up the
*       paremeter association cleanly.
*
*SOURCE
*       WRIMDS.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS
*       IMAGE   character The parameter name pointing to the image
*   OUTPUTS
*       CRVAL1  real    The RA of the ref pixel in degrees
*       CRVAL2  real    The DEC of the ref pixel in degrees
*       CRPIX1  integer The cross scan pixel location of the ref pixel,
*                       increasing west.
*       CRPIX2  integer The in scan pixel location of the ref pixel.
*       CDELT1  real    The cross scan size of a pixel in degrees of arc
*       CDELT2  real    The in scan size of a pixel in degrees of arc
*       CROTA1  real    Angle anti-clockwise from north to -ve in scan axis
*                       in degrees.
*       IERR    integer Status return. 0 - Success.
*
*USED BY
*       IRASCOEF,IRASSHIFT
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               WRERR
*       EDRS:
*               PTDSCR
*
*STARLINK PARAMETERS
*       'image'/read/   The name of the image is contained in character variable
*                       image.
*       BADDESC/error/  Accessed if any of the descriptors could not be updated
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 20/10/87
*----------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real      crval1,crval2,cdelt1,cdelt2,crota1
      integer   ierr,crpix1,crpix2
      character image*(*)
*
* DECLARE LOCAL VARIABLES
*
      character cval*1  ! Dummy character argument
      integer   ival    ! Dummy integer argument
      real      rval    ! dummy real argument
*
* WRITE THE DESCRIPTORS TO THE IMAGE
*
      call ptdscr(image,'CRPIX1','INTEGER',crpix1,rval,cval,ierr)
      if(ierr.eq.0) call ptdscr(image,'CRPIX2','INTEGER',crpix2,rval,
     :                          cval,ierr)
      if(ierr.eq.0) call ptdscr(image,'CRVAL1','REAL',ival,crval1,cval,
     :                          ierr)
      if(ierr.eq.0) call ptdscr(image,'CRVAL2','REAL',ival,crval2,cval,
     :                          ierr)
      if(ierr.eq.0) call ptdscr(image,'CROTA1','REAL',ival,crota1,cval,
     :                          ierr)
      if(ierr.eq.0) call ptdscr(image,'CDELT1','REAL',ival,cdelt1,cval,
     :                          ierr)
      if(ierr.eq.0) call ptdscr(image,'CDELT2','REAL',ival,cdelt2,cval,
     :                          ierr)
      if(ierr.ne.0) call wrerr('BADDESC')
*
* FINISH
*
      end
