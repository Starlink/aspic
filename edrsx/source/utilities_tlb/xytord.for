      subroutine xytord(sample,line,crval1,crval2,crpix1,crpix2,cdelt1,
     :                  cdelt2,crota,ra,dec)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Converts a position given as a line and sample no. to RA
*       and DEC.
*
*SOURCE
*       XYTORD.FOR in UTILITIES.TLB
*
*METHOD
*       The input position is rotated so that north is in the direction
*       of increasing y and east in the direction of descreasing x.
*       The pixel plane is considered to be tangential to a unit
*       celestial sphere at the ra and dec of the reference pixel.
*       The equations describing the projection are taken from the IRAS
*       explanatory supplement page X-31 (Eqn. X.D.3).
*
*ARGUMENTS
*   INPUTS:
*       sample  real    The sample no. of the position to be converted
*       line    real    The line no. of the position to be converted
*       CRVAL1  real    The RA of the ref pixel in degrees
*       CRVAL2  real    The DEC of the ref pixel in degrees
*       CRPIX1  integer The cross scan pixel location of the ref pixel,
*                       increasing west.
*       CRPIX2  integer The in scan pixel location of the ref pixel.
*       Cdelt1  real    The cross scan size of a pixel in degrees of arc
*       Cdelt2  real    The in scan size of a pixel in degrees of arc
*       CROTA1  real    Angle clockwise from north to -ve in scan axis
*                       in degrees.
*   OUTPUTS:
*       ra      real    The RA of the position given by sample and line
*       dec     real    The DEC of the position given by sample and line
*
*USED BY
*       IRASCORR
*
*SUBROUTINES CALLED
*       none
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
* DECLARE ARGUMENTS
*
      integer   crpix1,crpix2
      real      sample,line,crval1,crval2,cdelt1,cdelt2,crota,ra,dec
*
* DECLARE LOCAL VARIABLES
*
      real*8    a       ! A constant: Pi/180
      real*8    beta    ! Angle from south to the line joining pixel to
                        ! reference pixel.From south to east is positive
      real*8    delta   ! Angle (at centre of celestial sphere) between
                        ! the pixel and the reference pixel. (Degrees)
      real*8    cosbet  ! Cos(beta)
      real*8    cosdel  ! Cos(delta)
      real*8    cosrot  ! Cos(crota)
      real*8    cosvl1  ! Cos(crval1)
      real*8    cosvl2  ! Cos(crval2)
      real*8    linrad  ! Line as radians relative to reference pixel
      real*8    mod     ! Square of angular distance from pixel to ref.
                        ! pixel (in radians).
      real*8    samrad  ! Sample as radians relative to reference pixel
      real*8    sinbet  ! Sin(beta)
      real*8    sindel  ! Sin(delta)
      real*8    sinrot  ! Sin(crota)
      real*8    sinvl1  ! Sin(crval1)
      real*8    sinvl2  ! Sin(crval2)
      real*8    x       ! Distance in rads of the pixel east of the
                        ! reference pixel
      real*8    xx      ! Temporary storage
      real*8    y       ! Distance in rads of the pixel north of the
                        ! reference pixel
      real*8    yy      ! Temporary storage
*
* DEFINE THE CONSTANT PI/180
*
      a=dasin(1.0D0)/90.0
*
* CALCULATE OFTEN USED VALUES
*
      sinvl1=dsind(dble(crval1))
      cosvl1=dcosd(dble(crval1))
      sinvl2=dsind(dble(crval2))
      cosvl2=dcosd(dble(crval2))
      cosrot=dcosd(dble(crota))
      sinrot=dsind(dble(crota))
*
* CONVERT SAMPLE AND LINE TO RADIANS AWAY FROM THE REFERENCE PIXEL
*
      linrad=cdelt2*(line-crpix2)*a
      samrad=cdelt1*(sample-crpix1)*a
*
* ROTATE SAMPLE AND LINE SO THAT Y IS NORTH AND X IS EAST
*
      y=-linrad*cosrot+samrad*sinrot
      x=samrad*cosrot+linrad*sinrot
*
* DO ALGORITHM FROM IRAS EXPLANATORY SUPPLEMENT PAGE X-31 (EQN. X.D.3)
*
      mod=x*x+y*y
      if(mod.ge.1.0e-20) then
         delta=datand(sqrt(mod))
         beta=datan2d(x,-y)
         cosdel=dcosd(delta)
         sindel=dsind(delta)
         cosbet=dcosd(beta)
         sinbet=dsind(beta)
         xx=sinvl2*sindel*cosbet+cosvl2*cosdel
         yy=sindel*sinbet
*
* CALCULATE THE RA AND DEC OF THIS RADIUS VECTOR
*
         ra=crval1+real(datan2d(yy,xx))
         if(ra.lt.0) ra=360.0+ra
         dec=real(dasind(sinvl2*cosdel-cosvl2*sindel*cosbet))
      else
         ra=crval1
         dec=crval2
      endif
*
* FINISH
*
      end
