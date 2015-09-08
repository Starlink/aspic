      subroutine rdtoxy(sample,line,crval1,crval2,crpix1,crpix2,cdelt1,
     :                  cdelt2,crota,ra,dec)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Converts a position given as RA and DEC to a line and sample
*       no (i.e pixel co-ordinates).
*
*SOURCE
*       RDTOXY.FOR in UTILITIES.TLB
*
*METHOD
*       The pixel plane is considered to be tangential to a unit
*       celestial sphere at the ra and dec of the reference pixel.
*       The equations describing the projection are taken from the IRAS
*       explanatory supplement page X-30 (Eqn.s X.D.1 and X.D.2).
*       The input position is finally rotated so that north is in the
*       direction determined by argument crota.
*
*ARGUMENTS
*   INPUTS:
*       ra      real    The RA of the required pixel location
*       dec     real    The DEC of the  required pixel location
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
*       sample  real    The sample no. of the required pixel
*       line    real    The line no. of the required pixel
*
*USED BY
*       RADEC
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/3/88
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
      real*8    a       ! See Explanatory Supplement p X-30
      real*8    b       ! A constant: Pi/180
      real*8    cosdec  ! Cos(dec)
      real*8    cosrot  ! Cos(crota)
      real*8    cosvl2  ! Cos(crval2)
      real*8    f       ! See Explanatory Supplement p X-30
      real*8    linrad  ! Line as radians relative to reference pixel
      real*8    samrad  ! Sample as radians relative to reference pixel
      real*8    sindec  ! Sin(dec)
      real*8    sinrot  ! Sin(crota)
      real*8    sinvl2  ! Sin(crval2)
      real*8    x       ! Distance in rads of the pixel east of the
                        ! reference pixel
      real*8    y       ! Distance in rads of the pixel north of the
                        ! reference pixel
*
* DEFINE THE CONSTANT PI/180
*
      b=dasin(1.0D0)/90.0
*
* CALCULATE OFTEN USED VALUES
*
      cosdec=dcosd(dble(dec))
      sindec=dsind(dble(dec))
      sinvl2=dsind(dble(crval2))
      cosvl2=dcosd(dble(crval2))
      cosrot=dcosd(dble(crota))
      sinrot=dsind(dble(crota))
*
* DO ALGORITHM FROM IRAS EXPLANATORY SUPPLEMENT PAGE X-30 (EQN.S X.D.1
* AND X.D.2)
*
      a=cosdec*dcosd(dble(ra-crval1))
      f=sinvl2*sindec+a*cosvl2
      if(abs(f).gt.1.0e-20) then
         y=-(a*sinvl2-cosvl2*sindec)/f
         x=cosdec*dsind(dble(ra-crval1))/f
*
* ROTATE x AND y SO THAT LINRAD IS PARALLEL TO IMAGE Y AXIS AND SAMRAD
* IS PARALLEL TO IMAGE X AXIS
*
         linrad=x*sinrot-y*cosrot
         samrad=x*cosrot+y*sinrot
*
* CONVERT RADIANS AWAY FROM THE REFERENCE PIXEL TO SAMPLE AND LINE NO.S
*
         line=crpix2+real((linrad/b)/dble(cdelt2))
         sample=crpix1+real((samrad/b)/dble(cdelt1))
      else
*
* IF THE CONSTANT F WAS TOO SMALL SET THE PIXEL CO-ORDS VERY LARGE
*
         line=1.0e30
         sample=1.030
      endif
*
* FINISH
*
      end
