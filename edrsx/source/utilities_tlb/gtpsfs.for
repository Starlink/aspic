      subroutine gtpsfs(parnam,ndets,ippsf,npixp,nlinp,invalp,scalep,
     :                  zerop,crval1,crval2,cdelt1,cdelt2,size,npixps,
     :                  nlinps,title,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To aquire a set of 2d images with certain discriptors.
*
*SOURCE
*       GTPSFS.FOR in UTILITIES.TLB
*
*METHOD
*       The images are aquired using the parameters in the array PARNAM
*       and an array of pointers returned. Also arrays containing the
*       following descriptors are returned: NAXIS1,NAXIS2,INVAL,BSCALE,
*       BZERO,CRVAL1,CRVAL2,CDELT1,CDELT2,TITLE. The maximum image size
*       is also returned.
*
*ARGUMENTS
*   INPUTS:
*       parnam(size)    character       Array of INTERIM parameter names
*       ndets           integer         Number of images to be aquired
*       size            integer         Size of arrays
*   OUTPUTS:
*       ippsf(size)     integer         Array of pointers to images
*       npixp(size)     integer         Array of image sizes in pixels
*                                       (0 implies a null frame)
*       nlinp(size)     integer         Array of image sizes in lines
*                                       (0 implies a null frame)
*       invalp(size)    integer         Array of INVAL values
*       scalep(size)    real            Array of BSCALE values
*       zerop(size)     real            Array of BZERO values
*       crval1(size)    real            Array of CRVAL1 values
*       crval2(size)    real            Array of CRVAL2 values
*       cdelt1(size)    real            Array of CDELT1 values
*       cdelt2(size)    real            Array of CDELT2 values
*       title(size)     character       Array of TITLE values
*       npixps          integer         Max size of images in pixels
*       nlinps          integer         Max size of images in lines
*       ierr            integer         Error status: 0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gt2dir,ptderr
*       EDRS:
*               gtdscr
*       INTERIM:
*               cnpar
*
*STARLINK PARAMETERS
*       'parnam'/read/  Argument parnam contains an array of parameter
*                       names for accessing the images
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/11/87
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer   ndets,size,ierr,npixps,nlinps
      integer   npixp(size),nlinp(size),invalp(size),ippsf(size)
      real      scalep(size),zerop(size),cdelt1(size)
      real      cdelt2(size),crval1(size),crval2(size)
      character title(size)*(*),parnam(size)*(*)

*
* DECLARE LOCAL VARIABLES
*
      character cval*1  ! Dummy character argument
      integer   det     ! Image counter
      logical   edrs    ! True if all images are in EDRS format
      integer   inval   ! Default value for INVAL descriptor
      integer   ival    ! Dummy integer argument
      integer   maxbad  ! Maximum no. of bad entries before routine exit
      integer   nbad    ! No. of bad entries so far
      logical   null    ! True if null value given by user for image
      real      rval    ! Dummy real argument

*
* SET PARAMETER VALUES
*
      parameter (maxbad=3,inval=-32767)

*
* FLAG THAT ALL IMAGES ARE EDRS IMAGES
*
      edrs=.true.

*
* LOOP THROUGH ALL DETECTORS IN CROSS SCAN ORDER
*
      nlinps=0
      npixps=0
      do det=1,ndets
         nbad=0

*
* GET A POINTER TO THE DATA AND THE SIZE OF THE DATA
*
  10     call gt2dir(parnam(det),-102,.true.,npixp(det),nlinp(det),
     :               ippsf(det),ierr)
         if(ierr.eq.2) then
            edrs=.false.
         else if(ierr.eq.1) then
            npixp(det)=0
            nlinp(det)=0
            invalp(det)=inval
            null=.true.
         else
            if(ierr.ne.0) goto 999
            null=.false.
         endif

*
* UPDATE SIZE LARGEST INPUT IMAGE SIZE
*
         npixps=max(npixps,npixp(det))
         nlinps=max(nlinps,nlinp(det))

*
* IF AN IMAGE WAS SPECIFIED GET DESCRIPTORS BSCALE,BZERO,INVAL
*
         if(.not.null) then
            invalp(det)=-32768
            scalep(det)=1.0
            zerop(det)=0.0
            title(det)=' '
            call gtdscr(parnam(det),'INVAL','INTEGER',invalp(det),rval,
     :                  cval,ierr)
            if(ierr.ne.0) edrs=.false.
            call gtdscr(parnam(det),'BSCALE','REAL',ival,scalep(det),
     :                  cval,ierr)
            call gtdscr(parnam(det),'BZERO','REAL',ival,zerop(det),cval,
     :                  ierr)
            call gtdscr(parnam(det),'TITLE','CHARACTER',ival,rval,
     :                  title(det),ierr)
            if(ierr.ne.0) edrs=.false.

*
* GET IRAS DESCRIPTORS: CRVAL1,CRVAL2,CDELT1,CDELT2
*
            call gtdscr(parnam(det),'CRVAL1','REAL',ival,crval1(det),
     :                  cval,ierr)
            if(ierr.ne.0) then
               call ptderr('CRVAL1')
               crval1(det)=0.0
               call getpar('CRVAL1','REAL',1,-1.0e32,1.0e32,.true.,ival,
     :                      crval1(det),ierr)
               call cnpar('CRVAL1',ierr)
            endif

            call gtdscr(parnam(det),'CRVAL2','REAL',ival,crval2(det),
     :                  cval,ierr)
            if(ierr.ne.0) then
               call ptderr('CRVAL2')
               crval2(det)=0.0
               call getpar('CRVAL2','REAL',1,-1.0e32,1.0e32,.true.,ival,
     :                      crval2(det),ierr)
               call cnpar('CRVAL2',ierr)
            endif

            call gtdscr(parnam(det),'CDELT1','REAL',ival,cdelt1(det),
     :                  cval,ierr)
            if(ierr.ne.0) then
               call ptderr('CDELT1')
               cdelt1(det)=0.0
               call getpar('CDELT1','REAL',1,-1.0e32,1.0e32,.true.,ival,
     :                      cdelt1(det),ierr)
               call cnpar('CDELT1',ierr)
            endif

            call gtdscr(parnam(det),'CDELT2','REAL',ival,cdelt2(det),
     :                  cval,ierr)
            if(ierr.ne.0) then
               call ptderr('CDELT2')
               cdelt2(det)=0.0
               call getpar('CDELT2','REAL',1,-1.0e32,1.0e32,.true.,ival,
     :                      cdelt2(det),ierr)
               call cnpar('CDELT2',ierr)
            endif

         endif

*
* GO ROUND FOR NEXT DETECTOR
*
      enddo

*
* IF NOT ALL INPUTS WERE EDRS IMAGES SET ERROR STATUS NEGATIVE
*
      if(.not.edrs) ierr=-1

*
* FINISH
*
  999 continue

      end
