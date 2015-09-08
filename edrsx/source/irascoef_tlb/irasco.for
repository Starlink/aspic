      subroutine irasco
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To produce a set of 6 transformation co-efficients
*       which will transform one IRAS image to align with another
*       IRAS image. EDRS program RESAMPLE can be used to do the
*       alignment. The transformation is of the form
*
*              Xb=c(1)+c(2)*Xa+c(3)*Ya
*              Yb=c(4)+c(5)*Xa+c(6)*Ya
*
*SOURCE
*       IRASCO.FOR in IRASCOEF.TLB
*
*METHOD
*       The RA, DEC and XY location of the reference pixel from each
*       image is read from the image descriptors, together with the
*       image rotation angles and pixel sizes. A "flat sky
*       is assumed, i.e. a pixel is the same size (in RA and DEC)
*       whereever it is on the image. This could introduce errors
*       in large fields but should be ok for fields less then 3
*       degrees.
*               The 6 co-efficients are then calculated so that they
*       transform a pixel xy pair in image A to the xy pair in image
*       B which has the same RA and DEC as the point in image A.
*               To avoid having a cos(dec) factor, the co-ords of
*       each reference pixel are converted to a system in which the
*       reference pixel of image 1 is the origin, and the great circle
*       joining that pixel to the reference pixel of image 2 is the
*       x axis.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               RDIMDS,WRERR,IRASTR
*       EDRS:
*               TRCOUT,GETPAR
*       INTERIIM
*               WRUSER
*
*STARLINK PARAMETERS
*       IMAGEA/read/    Source image
*       IMAGEB/read/    Destination image
*       ILEVEL/read/    User information level
*       TOOBAD/error/   Accessed if too many bad values are given for ILEVEL
*       TRCOEFFS/write/ 6 co-efficients describing transformation
*       NOTRC/error/    Accessed if parameter TRCOEFFS could not be written
*       NOCOMB/error/   Accessed if either input was produced by COMBINE
*                       but has non-zero CROTA1
*
*AUTHOR
*       D.S.  Berry (MAVAD::DSB) 25/8/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      real      c(6)    ! The required co-efficients
      real      cdlt1a  ! The size (in x) of a pixel in image A in degrees
      real      cdlt2a  ! The size (in y) of a pixel in image A in degrees
      real      cdlt1b  ! The size (in x) of a pixel in image B in degrees
      real      cdlt2b  ! The size (in y) of a pixel in image B in degrees
      real      crot1a  ! Clockwise angle from north to image A -ve Y axis
      real      crot1b  ! Clockwise angle from north to image B -ve Y axis
      integer   crpx1a  ! The x value of the reference pixel in image A
      integer   crpx2a  ! The y value of the reference pixel in image A
      integer   crpx1b  ! The x value of the reference pixel in image B
      integer   crpx2b  ! The y value of the reference pixel in image B
      real      crvl1a  ! The RA (in degrees, +/- 180) of image A ref pixel
      real      crvl2a  ! The DEC (in degrees, +/- 90) of image A ref pixel
      real      crvl1b  ! The RA (in degrees, +/- 180) of image B ref pixel
      real      crvl2b  ! The DEC (in degrees, +/- 90) of image B ref pixel
      integer   i       ! Implicit loop count
      integer   ierr    ! Status return
      integer   ilevel  ! Level of information to display on screen
      character instra*30 ! INSTRUME descriptor from IMAGEA
      character instrb*30 ! INSTRUME descriptor from IMAGEB
      character prbuf*80! Buffer for text destined for users screen
      real      rval    ! dummy real argument
*
* GET INFORMATION DISPLAY LEVEL
*
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.false.,ilevel,
     :            rval,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
*
* GET THE NECESSARY DESCRIPTORS FROM IMAGE A (SEE LOCAL VARIABLE
* DECLARATIONS FOR A DESCRIPTION OF EACH)
*
      call rdimds('IMAGEA',.false.,crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,
     :             cdlt2a,crot1a,instra,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) goto 999
*
* GET THE NECESSARY DESCRIPTORS FROM IMAGE B
*
      call rdimds('IMAGEB',.false.,crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,
     :                  cdlt2b,crot1b,instrb,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) goto 999
*
* CALL IRASTR TO CALCULATE THE REQUIRED TRANSFORMATION CO-EFFICIENTS
*
      call irastr(crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,cdlt2a,crot1a,
     :            crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,cdlt2b,crot1b,c)
*
* IF REQUIRED DISPLAY VALUES OF CO-EFFICIENTS
*
      if(ilevel.ge.2) then
         call wruser(' ',ierr)
         call wruser(' Transformation co-efficients are:',ierr)
         call wruser(' ',ierr)
         write(prbuf,10) (c(i),i=1,3)
  10     format('   C(1)=',G13.6,' C(2)=',G13.6,' C(3)=',G13.6)
         call wruser(prbuf,ierr)
         write(prbuf,20) (c(i),i=4,6)
  20     format('   C(4)=',G13.6,' C(5)=',G13.6,' C(6)=',G13.6)
         call wruser(prbuf,ierr)
         call wruser(' ',ierr)
      endif
*
* WRITE OUT THE CO-EFFS TO THE ENVIRONMENT
*
      call trcout('TRCOEFFS',c,6,ierr)
      if (ierr.ne.0) then
         call wrerr('NOTRC')
      endif
*
* FINISH
*
 999  continue

      end
