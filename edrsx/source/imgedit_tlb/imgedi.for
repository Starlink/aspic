      subroutine imgedi
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To edit individual pixel values in a bdf file. If the file
*       doesn't already exist then it is created.
*
*SOURCE
*       IMGEDI.FOR in IMGEDIT.TLB
*
*METHOD
*       The user is asked for an input image to edit. If non is given
*       a new image is created. In this case the user must specify
*       the size of the image.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtwork,i2torl,rimset,i2prod
*       THIS PACKAGE (IMGEDIT.TLB):
*              edibdf
*       EDRS:
*              gt2dir,gt2diw,gtdscr,ptdscr,getpar
*       INTERIM:
*              cydscr,rdkeyc,frdata
*
*STARLINK PARAMETERS
*       INPUT/read/     Input image
*       NLINEOUT/read/  Size of output image if no input image given
*       NPIXOUT/read/   Size of output image if no input image given
*       OUTPUT/read/    Output image
*       TITLE/read/     Title for output image
*       ILEVEL/read/    User information level
*       LOOP/read/      If true, then more than one pixel value may be
*                       altered.
*       BADBACK/error/  Accessed if a non-numeric background value given
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*       tabs
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 14/3/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      logical   auto    ! True if pixel locations are to be generated
                        ! automatically, false if user gives them.
      character back*30 ! Buffer for string holding background value
      real      backvl  ! Background value for pixels in created output
      character cval*1  ! Dummy character argument
      integer   ierr    ! Error status
      integer   ilevel  ! User information level
      logical   inimag  ! True if a valid input image was given
      integer   invali  ! Flag for invalid pixels in input image
      integer   invalo  ! Flag for invalid pixels in output image
      integer   ipin    ! Pointer to input image
      integer   ipout   ! Pointer to output image
      integer   ipwork  ! Pointer to real workspace
      integer   ival    ! Dummy integer argument
      logical   loop    ! True if more than 1 pixel to be altered
      integer   ncomm   ! Position of chosen option within option list
      integer   nlin    ! No. of lines in output image
      integer   npix    ! No. of pixels in a line in output image
      real      rinval  ! Flag for invalid pixels in real image
      real      rval    ! Dummy real argument
      real      scale   ! Scale factor for output image
      integer   stposn  ! Position of selected option within option list
      integer   strlen  ! Function giving used length of a string
      character title*30! Title for output image
      real      zero    ! Zero offset for output image
*
      parameter (rinval=-1.0e32,invalo=-32767)
*
* GET INPUT IMAGE
*
      call gt2dir('INPUT',102,.true.,npix,nlin,ipin,ierr)
      if(ierr.eq..0) then
*
* IF AN INPUT WAS SPECIFIED, GET IMAGE DESCRIPTORS
*
         inimag=.true.
         invali=-100000
         scale=1.0
         zero=0.0
         title=' '
         call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
         call gtdscr('INPUT','INVAL','INTEGER',invali,rval,cval,ierr)
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title,ierr)
*
* IF NO INPUT GIVEN, GET SIZE OF OUTPUT IMAGE
*
      else
         title=' '
         inimag=.false.
         nlin=1
         call getpar('NLINEOUT','INTEGER',1,1.0,2000.0,.true.,nlin,rval,
     :                ierr)
         npix=1
         call getpar('NPIXOUT','INTEGER',1,1.0,2000.0,.true.,npix,rval,
     :                ierr)
      endif
*
* GET USER INFORMATION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
*
* SEE IF USER WISHES TO CHANGE MORE THAN ONE PIXEL
*
      stposn=2
      call gtstrn('LOOP',.true.,'YES,NO,TRUE,FALSE.',1,stposn,cval,
     :             ival,ierr)
      if(ierr.ne.0) then
         goto 999
      else
         if(mod(stposn,2).eq.0) then
            loop=.false.
         else
            loop=.true.
         endif
      endif
*
* GET WORKSPACE TO HOLD REAL IMAGE
*
      call gtwork('WORK','REAL',nlin*npix,ipwork,ierr)
      if(ierr.ne.0) goto 999
*
* IF AN INPUT IMAGE WAS GIVEN CONVERT IT TO A REAL IMAGE IN WORKSPACE
*
      if(inimag) then
         call i2torl(%val(ipin),npix,nlin,scale,zero,invali,rinval,
     :               %val(ipwork))
*
* IF NO INPUT WAS GIVEN FILL THE WORKSPACE WITH VALUES GIVEN BY THE
* PARAMETER BACKGRND
*
      else
         back='INVALID'
         call rdkeyc('BACKGRND',.true.,1,back,ival,ierr)
         call upperc(back)
         call lbgone(back)
         if(index('INVALID',back(:strlen(back))).eq.1) then
            backvl=rinval
         else
            call ctor(back,backvl,ierr)
            if(ierr.ne.0) then
               call wrerr('BADBACK')
               goto 999
            endif
         endif
         call rimset(%val(ipwork),npix,nlin,backvl)
      endif
*
* SEE IF PIXEL LOCATIONS ARE TO BE DETERMINED AUTOMATICALLY OR IF USER
* IS TO BE PROMPTED FOR PIXEL LOCATIONS
*
      if(inimag) then
         ncomm=2
      else
         ncomm=1
      endif
      call gtstrn('AUTO',.true.,'YES,NO,TRUE,FALSE.',1,ncomm,cval,
     :             ival,ierr)
      if(mod(ncomm,2).eq.0) then
         auto=.false.
      else
         auto=.true.
      endif
*
* CALL EDIBDF TO CREATE THE EDITTED BDF FILE
*
      call edibdf(%val(ipwork),npix,nlin,rinval,auto,loop,ilevel,ierr)
*
* IF SUCCESSFUL OBTAIN OUTPUT IMAGE
*
      if(ierr.eq.0) then
         call gt2diw('OUTPUT',102,.false.,npix,nlin,ipout,ierr)
         if (ierr.eq.0)then
*
* COPY IMAGE FROM REAL WORKSPACE TO OUTPUT IMAGE USING 0.75 OF
* AVAILABLE OUTPUT DYNAMIC RANGE
*
            call i2prod(%val(ipwork),npix,nlin,rinval,scale,zero,
     :                  %val(ipout),invalo)
*
* COPY DESCRIPTORS FROM INPUT IF INPUT GIVEN
*
            if(inimag) call cydscr('INPUT','OUTPUT',ierr)
*
* ENSURE ALL MANDATORY DESCRIPTORS HAVE CORRECT VALUES
*
            call ptdscr('OUTPUT','NAXIS','INTEGER',2,rval,cval,ierr)
            call ptdscr('OUTPUT','NAXIS1','INTEGER',npix,rval,cval,ierr)
            call ptdscr('OUTPUT','NAXIS2','INTEGER',nlin,rval,cval,ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,scale,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,zero,cval,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',invalo,rval,cval,
     :                   ierr)
            call rdkeyc('TITLE',.true.,1,title,ival,ierr)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,
     :                   ierr)
         endif
      endif

*
* FREE DATA AREAS
*
 999  call frdata(' ',ierr)

      end



