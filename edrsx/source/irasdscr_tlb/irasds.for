      subroutine irasds
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Calculates and stores IRAS image descriptors given the RA
*       and DEC of 2 pixels, assuming that the pixels are square.
*       The descriptors produced are CRVAL1,CRVAL2,CRPIX1,CRPIX2,
*       CDELT1,CDELT2,CROTA1,INSTRUME.
*
*SOURCE
*       IRASDS.FOR in IRASDSCR.TLB
*
*METHOD
*       The user is prompted for the X,Y,RA and DEC of two pixels. The
*       first pixel is used as the image reference pixel. The scan
*       direction and pixel size are then calculated assuming flat
*       geometry and square pixels. The descriptor values are then
*       displayed on the terminal screen and the user prompted for an
*       image with which to store the descriptors. Note, the I_CRDIMAGE
*       convention is used for CROTA1, rather than the I_COMBINE 
*       convention. This is indicated by setting the INSTRUME descriptor
*       to SURVEY (the value produced by I_CRDIMAGE).
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gt2did,wrerr
*       EDRS:
*               getpar,ptdscr
*       INTERIM:
*               rdkeyr,wruser,frdata
*
*STARLINK PARAMETERS
*       XY1/read/       The pixel co-ords of pixel 1
*       RA1_HRS/read/   Hours field of RA of pixel 1
*       RA1_MINS/read/  Minutes field of RA of pixel 1
*       RA1_SECS/read/  Seconds field of RA of pixel 1
*       DEC1_DEG/read/  Degrees field of DEC of pixel 1
*       DEC1_MIN/read/  Minutes field of DEC of pixel 1
*       DEC1_SEC/read/  Seconds field of DEC of pixel 1
*       XY2/read/       The pixel co-ords of pixel 2
*       RA2_HRS/read/   Hours field of RA of pixel 2
*       RA2_MINS/read/  Minutes field of RA of pixel 2
*       RA2_SECS/read/  Seconds field of RA of pixel 2
*       DEC2_DEG/read/  Degrees field of DEC of pixel 2
*       DEC2_MIN/read/  Minutes field of DEC of pixel 2
*       DEC2_SEC/read/  Seconds field of DEC of pixel 2
*       ILEVEL/read/    User information level. A value of 2 or higher
*                       produces a listing of descriptor values on the
*                       screen.
*       IMAGE/read/     A 2d image to which the calculated descriptors
*                       are to be added
*       EQUCORDS/error/ Accessed if the X or Y coordinates of the two
*                       pixels are equal
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       REAL*8 data type
*       degrees mode trig functions
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 16/12/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      real*8    alpha   ! Clockwise angle from north to +ve Y (degs)
      character bunit*30! String defining data units
      real      cdelt1  ! Size of image pixel along x in degrees of arc
      real      cdelt2  ! Size of image pixel along y in degrees of arc
      real      crota1  ! Angle from north to -ve Y thru +ve X
      integer   crpix1  ! X co-ord of image reference pixel
      integer   crpix2  ! Y co-ord of image reference pixel
      real      crval1  ! RA of image reference pixel (in degrees east)
      real      crval2  ! DEC of image reference pixel (in degrees east)
      character cval*1  ! Dummy character argument
      real      dec1    ! Dec of 1st pixel
      real      dec2    ! Dec of 2nd pixel
      real*8    deast   ! Arcmins east from pixel 2 to pixel 1
      real*8    dnorth  ! Arcmins north from pixel 2 to pixel 1
      real      dx      ! Pixels in x from pixel 2 to pixel 1
      real*8    dxarc   ! Arcmins in x from pixel 2 to pixel 1
      real      dy      ! Pixels in y from pixel 2 to pixel 1
      real*8    dyarc   ! Arcmins in y from pixel 2 to pixel 1
      integer   ierr    ! Error status
      integer   ilevel  ! User information level
      integer   ival    ! Dummy integer argument
      integer   nlin    ! No. of lines in image
      integer   npix    ! No. of pixels per line in image
      character prbuf*50! Buffer for output to terminal screen
      real      ra1     ! RA of 1st pixel
      real      ra2     ! RA of 2nd pixel
      real      rval    ! Dummy real argument
      real      xy1(2)  ! XY co-ords of 1st pixel
      real      xy2(2)  ! XY co-ords of 2nd pixel
*
* GET XY CO-ORDS OF 1ST PIXEL
*
      call rdkeyr('XY1',.false.,2,xy1,ival,ierr)
      if(ierr.ne.0) goto 999
*
* GET RA AND DEC OF FIRST PIXEL (TO BE THE IMAGE REFERENCE PIXEL
*
      call gtradc('1',ra1,dec1,.false.,.false.,ierr)
*
* GET XY, RA AND DEC OF 2ND PIXEL (NB NEITHER X OR Y VALUES ARE
* ALLOWED TO BE EQUAL TO THE FIRST POINT)
*
      call rdkeyr('XY2',.false.,2,xy2,ival,ierr)
      if(ierr.ne.0) goto 999
      if(xy1(1).eq.xy2(1).or.xy1(2).eq.xy2(2)) then
         call wrerr('EQUCORDS')
         goto 999
      endif
      call gtradc('2',ra2,dec2,.false.,.false.,ierr)
      if(ierr.ne.0) goto 999

*
* CALCULATE THE DEVIATION NORTH AND EAST FROM PIXEL 2 TO PIXEL 1 IN
* DEGREES OF ARC, AND THE DEVIATION IN PIXELS FROM PIXEL 2 TO PIXEL 1
*
      dnorth=dble(dec1-dec2)
      deast=15.0*dble(ra1-ra2)*dcosd(dble(dec1))
      dx=xy1(1)-xy2(1)
      dy=xy1(2)-xy2(2)
*
* CALCULATE THE CLOCKWISE ANGLE FROM NORTH TO +VE Y AXIS, USING FLAT
* GEOMETRY
*
      alpha=datan2d(-dnorth*dx-deast*dy,-deast*dx+dnorth*dy)
*
* CALCULATE THE DEVIATION FROM PIXEL 2 TO PIXEL 1 IN ARCMINS PARALLEL
* TO X AND Y AXES
*
      dxarc=-(dnorth*dsind(alpha)+deast*dcosd(alpha))
      dyarc=dnorth*dcosd(alpha)-deast*dsind(alpha)
*
* CALCULATE DESCRIPTOR VALUES
*
      crpix1=xy1(1)
      crpix2=xy1(2)
      crval1=15*ra1
      crval2=dec1
      cdelt1=dxarc/dx
      cdelt2=dyarc/dy
      crota1=180+alpha
*
* IF REQUIRED DISPLAY DESCRIPTORS ON TERMINAL
*
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.false.,ilevel,rval,ierr)
      if(ilevel.gt.1) then
         call wruser(' ',ierr)
         call wruser(' Decsriptor values:',ierr)
         call wruser(' ',ierr)
         write(prbuf,10) crpix1
   10    format('    CRPIX1=',I4)
         call wruser(prbuf,ierr)
         write(prbuf,20) crpix2
   20    format('    CRPIX2=',I4)
         call wruser(prbuf,ierr)
         write(prbuf,30) crval1
   30    format('    CRVAL1=',G13.6)
         call wruser(prbuf,ierr)
         write(prbuf,40) crval2
   40    format('    CRVAL2=',G13.6)
         call wruser(prbuf,ierr)
         write(prbuf,50) cdelt1
   50    format('    CDELT1=',G13.6)
         call wruser(prbuf,ierr)
         write(prbuf,60) cdelt2
   60    format('    CDELT2=',G13.6)
         call wruser(prbuf,ierr)
         write(prbuf,70) crota1
   70    format('    CROTA1=',g13.6)
         call wruser(prbuf,ierr)
         call wruser(' ',ierr)
      endif
*
* GET IMAGE TO WHICH DESCRIPTORS ARE TO BE ADDED WITHOUT MAPPING DATA
*
      call gt2did('IMAGE',.true.,ierr)
*
* IF AN IMAGE WAS GIVEN ADD THE CALCULATED DESCRIPTORS TO THE IMAGE
*
      if(ierr.eq.0) then
         call ptdscr('IMAGE','CRVAL1','REAL',ival,crval1,cval,ierr)
         call ptdscr('IMAGE','CRVAL2','REAL',ival,crval2,cval,ierr)
         call ptdscr('IMAGE','CDELT1','REAL',ival,cdelt1,cval,ierr)
         call ptdscr('IMAGE','CDELT2','REAL',ival,cdelt2,cval,ierr)
         call ptdscr('IMAGE','CROTA1','REAL',ival,crota1,cval,ierr)
         call ptdscr('IMAGE','CRPIX1','INTEGER',crpix1,rval,cval,ierr)
         call ptdscr('IMAGE','CRPIX2','INTEGER',crpix2,rval,cval,ierr)
         call ptdscr('IMAGE','INSTRUME','CHARACTER',ival,rval,'SURVEY',
     :                ierr)
*
* IF GIVEN FILE DOESN'T HAVE A DESCRIPTOR DEFINING DATA UNITS
* (BUNIT) GET ITS VALUE FROM THE USER AND PUT IT IN THE FILE
*
         call gtdscr('IMAGE','BUNIT','CHARACTER',ival,rval,bunit,ierr)
         if(ierr.ne.0) then
            bunit='UNKNOWN'
            call rdkeyc('UNITS',.true.,1,bunit,ival,ierr)
            call ptdscr('IMAGE','BUNIT','CHARACTER',ival,rval,bunit,
     :                   ierr)
         endif
      endif
*
* FINISH
*
  999 call frdata(' ',ierr)

      end
