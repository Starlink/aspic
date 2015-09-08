      subroutine irastr(crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,cdlt2a,
     :                  crot1a,crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,
     :                  cdlt2b,crot1b,c)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To calculate the transformation co-efficients which transform
*       pixel positions in image a to the pixel position in image b
*       which has the same RA and DEC.
*
*SOURCE
*       IRASTR.FOR in UTILITIES.TLB
*
*METHOD
*       To avoid having a cos(dec) factor, the co-ords of
*       each reference pixel are converted to a system in which the
*       reference pixel of image 1 is the origin, and the great circle
*       joining that pixel to the reference pixel of image 2 is the
*       x axis.
*
*ARGUMENTS
*   INPUTS:
*       cdlt1a  real    The size (in x) of a pixel in image A in degrees
*       cdlt2a  real    The size (in y) of a pixel in image A in degrees
*       cdlt1b  real    The size (in x) of a pixel in image B in degrees
*       cdlt2b  real    The size (in y) of a pixel in image B in degrees
*       crot1a  real    Clockwise angle from image A -ve Y axis to north(degs)
*       crot1b  real    Clockwise angle from image B -ve Y axis to north(degs)
*       crpx1a  integer The x value of the reference pixel in image A
*       crpx2a  integer The y value of the reference pixel in image A
*       crpx1b  integer The x value of the reference pixel in image B
*       crpx2b  integer The y value of the reference pixel in image B
*       crvl1a  real    The RA (in degrees, +/- 180) of image A ref pixel
*       crvl2a  real    The DEC (in degrees, +/- 90) of image A ref pixel
*       crvl1b  real    The RA (in degrees, +/- 180) of image B ref pixel
*       crvl2b  real    The DEC (in degrees, +/- 90) of image B ref pixel
*   OUTPUTS:
*       c(6)    real    The linear transformation co-efficients such that:
*                               Xb=c(1)+c(2)*Xa+c(3)*Ya
*                               Yb=c(4)+c(5)*Xa+c(6)*Ya
*
*USED BY
*       IRASCOEF,DRAWSCAN
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real      crvl1a,crvl2a,cdlt1a,cdlt2a,crot1a
      real      crvl1b,crvl2b,cdlt1b,cdlt2b,crot1b,c(6)
      integer   crpx1a,crpx2a,crpx1b,crpx2b
*
* DECLARE LOCAL VARIABLES
*
      real*8    alpha   ! The clockwise rotation from image B to image A (DEGS)
      real*8    cosalp  ! Cos of alpha
      real*8    cosapb  ! Cos of alpha minus psia
      real*8    cosdca  ! Cos of dec of image a ref pixel
      real*8    cosdcb  ! Cos of dec of image b ref pixel
      real*8    cosraa  ! Cos of ra of image a ref pixel
      real*8    cosrab  ! Cos of ra of image b ref pixel
      real*8    cospha  ! Cos of PHA
      real*8    cosphb  ! Cos of PHB
      real*8    costhe  ! Cos of angle between two ref pixels
      real*8    crossp(3)! Cross product
      real*8    gcsep   ! Distance from image A ref pix to image B ref pix
                        ! along great circle joining the two ref pixels
      real*8    modngc  ! Modulus of vector ngc
      real*8    mx      ! Reciprocal of x size of pixels in image B
      real*8    my      ! Reciprocal of y size of pixels in image B
      real*8    ngc(3)  ! The normal to plane containing great circle
                        ! joining the two reference pixels
      real*8    pa(3)   ! The position vector of image a reference pixel
      real*8    pb(3)   ! The position vector of image b reference pixel
      real*8    pha     ! Clkwise angle from north at ref pix A, to great circle
      real*8    phb     ! Clkwise angle from north at ref pix B, to great circle
      real*8    psia    ! anticlocwise angle from image A y axis to great circle
      real*8    sinalp  ! Sin of alpha
      real*8    sinapb  ! Sin of alpha plus psia
      real*8    sindca  ! Sin of dec of image a ref pixel
      real*8    sindcb  ! Sin of dec of image b ref pixel
      real*8    sinraa  ! Sin of ra of image a ref pixel
      real*8    sinrab  ! Sin of ra of image b ref pixel
      real*8    sinpha  ! Sin of PHA
      real*8    sinphb  ! Sin of PHB
      real*8    sinthe  ! Sin of angle between two ref pixels
*
* CALCULATE COMMONLY USED FACTORS
*
      mx=1.0/cdlt1b
      my=1.0/cdlt2b
      cosdca=cosd(dble(crvl2a))
      sindca=sind(dble(crvl2a))
      cosraa=cosd(dble(crvl1a))
      sinraa=sind(dble(crvl1a))
      cosdcb=cosd(dble(crvl2b))
      sindcb=sind(dble(crvl2b))
      cosrab=cosd(dble(crvl1b))
      sinrab=sind(dble(crvl1b))
*
* CALCULATE THE POSITON VECTOR OF REFERENCE PIXEL FOR IMAGE A ON THE
* CELESTIAL SPHERE IN 3D CARTESIAN CO-ORDS, SCALED UP BY MX TO REDUCE
* ROUNDING ERRORS LATER ON. THE CO-ORD SYSTEM USED IS:
* X POSITIVE TOWARDS ZERO RA
* Y POSITIVE NORTHWARDS
* Z POSITIVE WEST OF X
*
      pa(1)=cosdca*cosraa*mx
      pa(2)=sindca*mx
      pa(3)=-cosdca*sinraa*mx
*
* CALCULATE THE POSITON VECTOR OF REFERENCE PIXEL FOR IMAGE B ON A
* UNIT CELESTIAL SPHERE, SCALED UP BY MX TO REDUCE
* ROUNDING ERRORS LATER ON.
*
      pb(1)=cosdcb*cosrab*mx
      pb(2)=sindcb*mx
      pb(3)=-cosdcb*sinrab*mx
*
* CALCULATE THE NORMAL TO THE PLANE CONTAINING THE GREAT CIRCLE JOINING
* THE TWO REFERENCE PIXELS
*
      ngc(1)=pa(2)*pb(3)-pa(3)*pb(2)
      ngc(2)=pa(3)*pb(1)-pa(1)*pb(3)
      ngc(3)=pa(1)*pb(2)-pa(2)*pb(1)
*
* CONVERT TO A UNIT VECTOR (IF THE VECTOR IS OF ZERO LENGTH THE REFERENCE
* PIXELS OF THE TWO IMAGES ARE CO-INCIDENT AND USE THE MERIDIAN AS THE
* GREAT CIRCLE)
*
      modngc=sqrt(ngc(1)*ngc(1)+ngc(2)*ngc(2)+ngc(3)*ngc(3))
      if(modngc.eq.0) then
         ngc(1)=sinraa
         ngc(2)=0
         ngc(3)=cosraa
      else
         ngc(1)=ngc(1)/modngc
         ngc(2)=ngc(2)/modngc
         ngc(3)=ngc(3)/modngc
      endif
*
* CALC DISTANCE BETWEEN REF PIX A AND B ALONG GREAT CIRCLE
*
      if(ngc(2).gt.0.or.(ngc(2).eq.0.and.crvl2b.gt.crvl2a)) then
         sinthe=modngc
      else
         sinthe=-modngc
      endif
      costhe=pa(1)*pb(1)+pa(2)*pb(2)+pa(3)*pb(3)
      gcsep=ABS(atan2d(sinthe,costhe))
*
* CALC CROSS PRODUCT OF MERIDIAN NORMAL AND GREAT CIRCLE NORMAL AT IMAGE A
* REF PIX
*
      crossp(1)=-ngc(2)*cosraa
      crossp(2)=ngc(1)*cosraa-ngc(3)*sinraa
      crossp(3)=ngc(2)*sinraa
*
* CALC SIN OF ANGLE BETWEEN MERIDIAN AND GREAT CIRCLE AT IMAGE A REF PIX
*
      if(pa(1).ne.0) then
         sinpha=-crossp(1)/pa(1)
      else if(pa(2).ne.0) then
         sinpha=-crossp(2)/pa(2)
      else
         sinpha=-crossp(3)/pa(3)
      endif
*
* CORRECT SIN PHA FOR THE SCALING FACTOR OF MX INCLUDED IN PA
*
      sinpha=sinpha*mx
*
* CALC COS OF THE SAME ANGLE FROM THE DOT PRODUCT
*
      cospha=ngc(1)*sinraa+ngc(3)*cosraa
*
* CALC PHA IN RANGE -180 DEGS TO +180 DEGS
*
      pha=atan2d(sinpha,cospha)
*
* CALC CROSS PRODUCT OF MERIDIAN NORMAL AND GREAT CIRCLE NORMAL AT IMAGE B
* REF PIX
*
      crossp(1)=-ngc(2)*cosrab
      crossp(2)=ngc(1)*cosrab-ngc(3)*sinrab
      crossp(3)=ngc(2)*sinrab
*
* CALC SIN OF ANGLE BETWEEN MERIDIAN AND GREAT CIRCLE AT IMAGE B REF PIX
*
      if(pb(1).ne.0) then
         sinphb=-crossp(1)/pb(1)
      else if(pb(2).ne.0) then
         sinphb=-crossp(2)/pb(2)
      else
         sinphb=-crossp(3)/pb(3)
      endif
*
* CORRECT SIN PHB FOR THE SCALING FACTOR OF MX INCLUDED IN PB
*
      sinphb=sinphb*mx
*
* CALC COS OF THE SAME ANGLE FROM THE DOT PRODUCT
*
      cosphb=ngc(1)*sinrab+ngc(3)*cosrab
*
* CALC PHA IN RANGE -180 DEGS TO +180 DEGS
*
      phb=atan2d(sinphb,cosphb)
*
* CALCULATE COS AND SIN OF ANGLE BETWEEN Y AXES OF THE TWO IMAGES
*
      alpha=-crot1a-pha+crot1b+phb
      cosalp=cosd(alpha)
      sinalp=sind(alpha)
*
* CALCULATE COS AND SIN OF (ANGLE BETWEEN IMAGE A Y AXIS AND GREAT CIRCLE
* PLUS ANGLE BETWWEN IMAGES)
*
      psia=-crot1a-pha+180
      cosapb=cosd(alpha-psia)
      sinapb=sind(alpha-psia)
*
* CALCULATE THE SIX CO-EFFICENTS OF THE TRANSFORMATION
*
      c(1)=crpx1b+mx*(-cdlt1a*crpx1a*cosalp+cdlt2a*crpx2a*sinalp
     :     +gcsep*sinapb)
      c(2)=mx*cdlt1a*cosalp
      c(3)=-mx*cdlt2a*sinalp
      c(4)=crpx2b+my*(-cdlt1a*crpx1a*sinalp-cdlt2a*crpx2a*cosalp
     :     -gcsep*cosapb)
      c(5)=my*cdlt1a*sinalp
      c(6)=my*cdlt2a*cosalp
*
* FINISH
*
      end
