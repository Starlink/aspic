      subroutine simcrd
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Produces a file containing simulated CRDD. The user provides
*       an image containing an artificial 'sky', this program generates
*       a file containing the CRDD which IRAS would have produced if it
*       had scanned across the artificial sky. The extent, centre and
*       angle of the scan are specified as parameters.
*
*SOURCE
*       SIMCRD.FOR in SIMCRDD.TLB
*
*METHOD
*       The user is prompted for a 3D stack containing a set of images
*       defining the point spread function of each detector in a single
*       IRAS band (as produced by program PSFSTACK).All other parameters
*       defining the sky and area to be scanned are obtained from the
*       user. Subroutine GENCRD is called to generate the CRDD values
*       which are then writen to the output file. CRDD-like descriptors
*       are then added to the output.
*       Notes:
*       1) Throughout the program 'flat' geometry is assumed.
*       2) The convolution can be done either with linear or nearest
*          neighbour interpolation.
*       3) If the output file is to be processed by CRDIMAGE then
*          boresight descriptors are required. These are not created by
*          SIMCRDD but can be copied from a real CRDD file in which case
*          the RA and DEC information in the image formed by CRDIMAGE
*          are meaningless.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gt2dir,gt3dir,gt2diw,ptderr,wrerr,gtstrn,gtwork,rltoi4
*       THIS PACKAGE (SIMCRDD.TLB):
*               gencrd
*       EDRS:
*               gtdscr,getpar,gtdscn,ptdscr
*       INTERIM:
*               cydscr,rdkeyc,frdata
*
*STARLINK PARAMETERS
*       SKY/read/       2d image containing artificial sky to be scanned
*       CDELT1/read/    Size of sky pixels along x axis in arc degrees
*                       (Only accessed if descriptor CDELT1 not in sky).
*       CDELT2/read/    Size of sky pixels along y axis in arc degrees
*                       (Only accessed if descriptor CDELT2 not in sky).
*       PSFSTACK/read/  Stack of PSF images produced by program PSFSTACK
*       BAND/read/      IRAS band no. (1-4). Only accessed if descriptor
*                       BAND not present in sky image.
*       ANGLE/read/     Angle in degreees from sky image y axis to
*                       required scan direction. Rotation from Y to X
*                       is considered +ve. Also rotation from East to
*                       North must be in same sense as Y to X.
*       UNITS/read/     Units in which scan length is to be expressed
*                       Samples,arcmins,seconds or sky pixels.
*       SCANLEN/read/   Length of scan required.
*       SPECIFY/read/   Specifies which point in the scan is going to
*                       be given by the user, CENTRE or START.
*       START_X/read/   X co-ord in sky pixels of scan start.
*       START_Y/read/   Y co-ord in sky pixels of scan start.
*       CENTRE_X/read/  X co-ord in sky pixels of scan centre.
*       CENTRE_Y/read/  Y co-ord in sky pixels of scan centre.
*       METHOD/read/    Interpolation method to use: linear or nearest
*                       neighbour
*       OUTPUT/read/    Name of the output file.
*       DESFILE/read/   Name of file containing boresight descriptors
*       OBJECT/read/    A string to store as descriptor OBJECT.
*       FINESKY/error/  Accessed if the sky image pixels are smaller
*                       than PSF image pixels.
*       BADCDELT/error/ Accessed if sky pixels are not square, and
*                       parameter UNITS has value "sky pixels".
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/1/87
*-------------------------------------------------------------------
*
      implicit none
*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
*
* DECLARE LOCAL VARIABLES
*
      integer   band    ! IRAS band no. of simulated CRDD
      integer   blank   ! Invalid pixel flag in output image
      character contnt*30 ! Title of 3d psf stack
      character cval(3)*1 ! Dummy character argument
      integer   i       ! Loop count
      integer   ierr    ! Error status
      integer   lmeth   ! No. of characters in string 'method'
      integer   invals  ! Flag for invalid pixels in sky image
      integer   ip      ! Pointer to image holding boresight descriptors
      integer   ipsky   ! Pointer to start of sky image
      integer   ival(3) ! Dummy integer argument
      character method*7! Interpolation merthod LINEAR or NEAREST
      integer   nlin    ! Size of image holding boresight descriptors:lines
      integer   nlinps  ! Size of PSF images in 3d stack: lines in image
      integer   nlins   ! No. of lines in sky image
      integer   npix    ! Size of image holding boresight descriptors:pixels
      integer   npixps  ! Size of PSF images in 3d stack: pixels per line
      integer   npixs   ! No. of pixels in each line of sky image
      logical   ok      ! True if all psf pixels smaller than sky pixels
      real      rinval  ! Flag for invalid pixels in real images
      real      rval(3) ! Dummy real argument
      real      scales  ! Scale factor for sky image integers
      real      zeros   ! Zero level for sky image integers
      character title*30! Title from sky image
      real      trdtfp(6,IR_dts)! Co-effs of transformation from
                        ! detector (X,Y) in pixels to focal plane (Z,Y)
                        ! in arcmins
      integer   invalp(IR_dts)  ! Flag for invalid pixels in 2d PSFs
      integer   ipcrdd          ! Pointer to CRDD real data
      integer   ipout   ! Pointer to output image
      integer   ippsf(IR_dts)   ! Pointers to PSF images within 3d stack
      integer   lspec   ! No. of characters in string 'spec'
      integer   lunits  ! No. of characters in string 'units'
      integer   ndets   ! No. of detectors in chosen band
      integer   nlinp(IR_dts)   ! Size of 2d PSF images: lines in image
      integer   npixp(IR_dts)   ! Size of 2d PSF images: pixels per line
      integer   nsamp   ! No. of samples in output CRDD file
      real      outmax  ! Max value of output CRDD
      real      scale   ! Scale factor for output integers
      real      scalep(IR_dts)  ! Scale factors for 2d PSF images
      real      scanln  ! Scan length in users units
      character spec*6  ! Position in scan to be specified: START or CENTRE
      real      temp(IR_dts,6)  ! Temporary real descriptor storage
      character units*7 ! Units in which scan length is specified by user
      integer   xcen    ! X co-ord of centre of scan in sky pixels
      integer   xstart  ! X co-ord of start of scan in sky pixels
      integer   ycen    ! Y co-ord of centre of scan in sky pixels
      integer   ystart  ! Y co-ord of start of scan in sky pixels
      real      zero    ! Zero level for output integers
      real      zerop(IR_dts)   ! Zero levels for 2d PSF images
      real      crvl1s,crvl2s,cdlt1s,cdlt2s,crot1s
      integer   crpx1s,crpx2s,crpx1p,crpx2p
      integer   istrt,iend,ballno,det
      real      ra0,dec0,xy0(2),alpha
      logical   found
      character instrm*30
      real      answer(IR_dts)
      parameter (rinval=-1.0e32)
*
* GET SKY FRAME AND DESCRIPTORS (EDRS AND IRAS)
*
      call gt2dir('SKY',102,.false.,npixs,nlins,ipsky,ierr)
      if(ierr.ne.0) goto 999
      title=' '
      invals=-100000
      scales=1.0
      zeros=0.0
      call gtdscr('SKY','TITLE','CHARACTER',ival,rval,title,ierr)
      call gtdscr('SKY','INVAL','INTEGER',invals,rval,cval,ierr)
      call gtdscr('SKY','BSCALE','REAL',ival,scales,cval,ierr)
      call gtdscr('SKY','BZERO','REAL',ival,zeros,cval,ierr)
      call gtdscr('SKY','CDELT1','REAL',ival,cdlt1s,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CDELT1')
         call getpar('CDELT1','REAL',1,0.0,360.0,.false.,ival,cdlt1s,
     :               ierr)
         if(ierr.ne.0) goto 999
      endif
      call gtdscr('SKY','CDELT2','REAL',ival,cdlt2s,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CDELT2')
         call getpar('CDELT2','REAL',1,0.0,360.0,.false.,ival,cdlt2s,
     :               ierr)
         if(ierr.ne.0) goto 999
      endif
*
* GET 3D PSF IMAGE AND DESCRIPTORS (EDRS AND IRAS) FOR ALL DETECTORS IN
* CHOSEN BAND
*
      call gt3dir('PSFSTACK',102,.false.,npixps,nlinps,ndets,ippsf,ierr)
      if(ierr.ne.0) goto 999
      call gtdscr('PSFSTACK','CONTENT','CHARACTER',ival,rval,contnt,
     :             ierr)
      call gtdscr('PSFSTACK','BAND','INTEGER',band,rval,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('BAND')
         call getpar('BAND','INTEGER',1,1.0,4.0,.false.,band,rval,ierr)
         if(ierr.ne.0) goto 999
      endif
      call gtdscn('PSFSTACK','NPIX','INTEGER',1,ndets,npixp,rval,cval,
     :             ierr)
      call gtdscn('PSFSTACK','NLIN','INTEGER',1,ndets,nlinp,rval,cval,
     :             ierr)
      call gtdscn('PSFSTACK','INVAL','INTEGER',1,ndets,invalp,rval,cval,
     :             ierr)
      call gtdscn('PSFSTACK','BSCALE','REAL',1,ndets,ival,scalep,cval,
     :             ierr)
      call gtdscn('PSFSTACK','BZERO','REAL',1,ndets,ival,zerop,cval,
     :             ierr)
      call gtdscn('PSFSTACK','TRDTFP1','REAL',1,ndets,ival,temp(1,1),
     :             cval,ierr)
      call gtdscn('PSFSTACK','TRDTFP2','REAL',1,ndets,ival,temp(1,2),
     :             cval,ierr)
      call gtdscn('PSFSTACK','TRDTFP3','REAL',1,ndets,ival,temp(1,3),
     :             cval,ierr)
      call gtdscn('PSFSTACK','TRDTFP4','REAL',1,ndets,ival,temp(1,4),
     :             cval,ierr)
      call gtdscn('PSFSTACK','TRDTFP5','REAL',1,ndets,ival,temp(1,5),
     :             cval,ierr)
      call gtdscn('PSFSTACK','TRDTFP6','REAL',1,ndets,ival,temp(1,6),
     :             cval,ierr)
*
* CHECK THAT PIXEL SIZES OF ALL PSF IMAGES ARE SMALLER THAN SKY PIXELS
*
      ok=.true.
      do det=1,ndets
         if(temp(det,2).gt.60*cdlt1s.or.temp(det,6).gt.60*cdlt2s)
     :      ok=.false.
      enddo
      if(.not.ok) then
         call wrerr('FINESKY')
         goto 999
      endif
*
* GET SCAN ORIENTATION AS ANGLE FROM +VE Y THRU +VE X TO SCAN DIRECTION
* (IN DEGREES)
*
      alpha=0
      call getpar('ANGLE','REAL',1,0.0,360.0,.true.,ival,alpha,ierr)
*
* GET UNITS THE USERS WANTS TO USE TO SPECIFY THE SCAN LENGTH
*
      ival(1)=1
      call gtstrn('UNITS',.true.,'SAMPLES,ARCMINS,SECONDS,PIXELS.',1,
     :             ival,units,lunits,ierr)
      if(ierr.ne.0) goto 999
*
* GET SCAN LENGTH AND CONVERT TO SAMPLES
*
      call getpar('SCANLEN','REAL',1,0.0,1.0e32,.false.,ival,scanln,
     :             ierr)
      if(ierr.ne.0) goto 999

      if(units.eq.'SAMPLES') then
         nsamp=int(scanln)

      else if(units.eq.'ARCMINS') then
         nsamp=int(scanln*DT_srt(band)/IR_scr)

      else if(units.eq.'SECONDS') then
         nsamp=int(scanln*DT_srt(band))

      else if(units.eq.'PIXELS') then
         if(cdlt1s.eq.cdlt2s) then
            nsamp=int(scanln*60*cdlt1s*DT_srt(band)/IR_scr)
         else
            call wrerr('BADCDELT')
            goto 999
         endif

      else
         goto 999
      endif
*
* SEE IF USER WISHES TO SPECIFY SCAN START OR CENTRE
*
      ival(1)=1
      call gtstrn('SPECIFY',.true.,'START,CENTRE.',1,ival,spec,lspec,
     :             ierr)
*
* GET THE CHOSEN POSITION IN PIXELS AND CONVERT TO START POSITION
*
      if(spec.eq.'START') then
         xstart=npixs*0.5
         call getpar('START_X','INTEGER',1,-1.0e6,1.0e6,.true.,xstart,
     :                rval,ierr)
         ystart=1
         call getpar('START_Y','INTEGER',1,-1.0e6,1.0e6,.true.,ystart,
     :                rval,ierr)
      else
         xcen=npixs*0.5
         call getpar('CENTRE_X','INTEGER',1,-1.0e6,1.0e6,.true.,xcen,
     :                rval,ierr)
         xstart=nint(xcen-((nsamp-1)*sind(alpha)*IR_scr/
     :                                       (120*cdlt1s*DT_srt(band))))
         ycen=nlins*0.5
         call getpar('CENTRE_Y','INTEGER',1,-1.0e6,1.0e6,.true.,ycen,
     :                rval,ierr)
         ystart=nint(ycen-((nsamp-1)*cosd(alpha)*IR_scr/
     :                                       (120*cdlt2s*DT_srt(band))))
      endif
*
* GET INTERPOLATION METHOD
*
      ival(1)=1
      call gtstrn('METHOD',.true.,'NEAREST,LINEAR.',1,ival,method,lmeth,
     :             ierr)
*
* ORDER THE COEFFICENTS OF THE DETECTOR TO FOCAL PLANE TRANSFORMATION
* TO AVOID UNNECESSARY PAGING
*
      do det=1,ndets
         do i=1,6
            trdtfp(i,det)=temp(det,i)
         enddo
      enddo
*
* GET REAL WORKSPACE FOR CRDD
*
      call gtwork('CRDD','REAL',nsamp*ndets,ipcrdd,ierr)
      if(ierr.ne.0) goto 999
*
* CALL GENCRD TO GENERATE THE CRDD FILE
*
      call gencrd(xstart,ystart,alpha,method,
     :                  cdlt1s,cdlt2s,trdtfp,%val(ippsf(1)),
     :                  npixp,nlinp,invalp,scalep,zerop,%val(ipsky),
     :                  npixs,nlins,scales,zeros,invals,answer,rinval,
     :                  ndets,npixps,nlinps,IR_dts,IR_scr,nsamp,
     :                  %val(ipcrdd),outmax,DT_srt(band),ierr)
      if(ierr.ne.0) goto 999
*
* GET OUTPUT I*4 IMAGE
*
      call gt2diw('OUTPUT',104,.false.,ndets,nsamp,ipout,ierr)
      if(ierr.ne.0) goto 999
*
* COPY REAL CRDD TO OUTPUT IMAGE
*
      blank=-999999
      scale=outmax/32767.0
      zero=0.0
      call rltoi4(%val(ipout),%val(ipcrdd),nsamp,ndets,blank,rinval,
     :             scale,zero)
*
* GET AN IMAGE FROM WHICH TO COPY THE DESCRIPTORS TO THE SIMULATED CRDD
* (TO GET REALISTIC BORESIGHT VALUES SO THAT SIMULATED CRDD WILL GO
* THROUGH CRDIMAGE)
*
      call gt2dir('DESFILE',104,.true.,npix,nlin,ip,ierr)
      if(ierr.eq.0) call cydscr('DESFILE','OUTPUT',ierr)
*
* UPDATE ESSENTIAL CRDD DESCRIPTORS IN OUTPUT FILE
*
      call ptdscr('OUTPUT','HISTORY','CHARACTER',ival,rval,
     :            '*** SIMULATED CRDD GENERATED BY PROGRAM SIMCRDD ***',
     :            ierr)
      call ptdscr('OUTPUT','NAXIS','INTEGER',2,rval,cval,ierr)
      call ptdscr('OUTPUT','NAXIS1','INTEGER',nsamp,rval,cval,ierr)
      call ptdscr('OUTPUT','NAXIS2','INTEGER',ndets,rval,cval,ierr)
      call ptdscr('OUTPUT','BSCALE','REAL',ival,scale,cval,ierr)
      call ptdscr('OUTPUT','BZERO','REAL',ival,zero,cval,ierr)
      call ptdscr('OUTPUT','BLANK','INTEGER',blank,rval,cval,ierr)
      call ptdscr('OUTPUT','BITPIX','INTEGER',32,rval,cval,ierr)
      call ptdscr('OUTPUT','BUNIT','CHARACTER',ival,rval,
     :            '.Arbitary units',ierr)
      call ptdscr('OUTPUT','CONTENT','CHARACTER',ival,rval,contnt,ierr)
      call rdkeyc('OBJECT',.true.,1,title,ival,ierr)
      call ptdscr('OUTPUT','OBJECT','CHARACTER',ival,rval,title,ierr)
      call ptdscr('OUTPUT','RA-REQ','CHARACTER',ival,rval,'Unknown',
     :             ierr)
      call ptdscr('OUTPUT','DEC-REQ','CHARACTER',ival,rval,'Unknown',
     :             ierr)
      call ptdscr('OUTPUT','SOP','INTEGER',-1,rval,cval,ierr)
*
* FINISH
*
  999 call frdata(' ',ierr)

      end
