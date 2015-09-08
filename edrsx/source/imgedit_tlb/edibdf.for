      subroutine edibdf(image,npix,nlin,rinval,auto,loop,ilevel,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To change individual pixel values within an image
*
*SOURCE
*       EDIBDF.FOR in IMGEDIT.TLB
*
*METHOD
*       The image is stored as a real array. The user gives a value
*       for each pixel, but can also give several text strings instead
*       of a data value:
*
*               CHANGE   changes the method of determining pixel
*                        locations (AUTO or MANUAL).
*               AUTO     selects AUTO mode
*               MANUAL   selects MANUAL mode
*               QUIT     terminates data input
*               END
*               EXIT
*               INVALID  specifies that the pixel is to be set invalid
*
*       Any abreviation of these can be given.
*
*ARGUMENTS
*   INPUTS:
*       npix,nlin        integers  The size of the image
*       image(npix,nlin) real      The image to be edited
*       rinval           real      The pixel value for invalid pixels
*       auto             logical   If true then starts in AUTO mode
*       loop             logical   If false then routine exits after
*                                  one pixel has been altered.
*       ilevel           integer   User information level
*   OUTPUTS:
*       image(npix,nlin) real      The edited image
*       ierr             integer   Error status 0 - Success
*                                               1 - QUIT specified
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               strlen,upperc,wrerr
*       EDRS:
*               lbgone,getpar
*       INTERIM:
*               wruser,cnpar,rdkeyc,ctor
*
*STARLINK PARAMETERS
*       X/read/         Pixel X co-ordinate (pixel no.)
*       Y/read/         Pixel Y co-ordinate (line no.)
*       VALUE/read/     Buffer for user input of pixel value/commands
*       WHAT/error/     Accessed if user input didn't have any of the
*                       allowed commands and couldn't be translated
*                       into a number.
*       END/error/      Accessed if the end of the image is reached
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 15/3/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,ierr,ilevel
      real      image(npix,nlin),rinval
      logical   auto,loop
*
* DECLARE LOCAL VARIABLES
*
      logical   change  ! True if a change of mode requested
      logical   inc     ! True if default pixel location to be incremented
      integer   ival    ! Dummy integer argument
      integer   line    ! Current line being edited
      logical   more    ! True if user does  not wish to finish
      integer   pixel   ! Current pixel being edited
      character prbuf*20! Buffer for text output
      real      rval    ! Dummy real argument
      integer   strlen  ! Function giving used length of a string
      character text*13 ! Buffer for pixel value/command
      integer   tlen    ! Used length of variable "text
      real      value   ! Pixel value to be stored in image
*
* INITIALISE ERROR STATUS, CURRENT PIXEL AND CHANGE STATE
*
      ierr=0
      line=1
      pixel=1
      change=.true.
*
* LOOP UNTIL USER WISHES TO EXIT (OR UNTIL ONE PIXEL IS CHANGED IF
* ARGUMENT LOOP IS FALSE)
*
      more=.true.
      do while(more)
         if(ilevel.gt.1) call wruser(' ',ierr)
*
* IF IN AUTO MODE DISPLAY CURRENT PIXEL CO-ORDINATES
*
         if(auto) then
            if(change) then
               if(ilevel.gt.1) then
                  call wruser(' ',ierr)
                  call wruser('  AUTO mode.  Current pixel is :',ierr)
               endif
               change=.false.
            endif
            if(ilevel.gt.1) then
               write(prbuf,10) pixel,line
   10          format('  X: ',I5,'  Y: ',I5)
               call lbgone(prbuf(16:))
               call lbgone(prbuf(6:10))
               call wruser(prbuf,ierr)
            endif
*
* IF IN MANUAL MODE, GET PIXEL CO-ORDINATES FROM USER
*
         else
            if(change) then
               if(ilevel.gt.1) then
                  call wruser(' ',ierr)
                  call wruser('  MANUAL mode. Give pixel co-ordinates.',
     :                           ierr)
               endif
               change=.false.
            endif
            call getpar('X','INTEGER',1,1.0,real(npix),.true.,pixel,
     :                   rval,ierr)
            call cnpar('X',ierr)
            call getpar('Y','INTEGER',1,1.0,real(nlin),.true.,line,
     :                   rval,ierr)
            call cnpar('Y',ierr)
         endif
*
* CONSTRUCT THE DEFAULT TEXT STRING TO OFFER TO THE USER
*
         if(image(pixel,line).ne.rinval) then
            write(text,'(G13.6)') image(pixel,line)
         else
            text='INVALID'
         endif
*
* GET A TEXT STRING FROM THE USER
*
         call rdkeyc('VALUE',.true.,1,text,ival,ierr)
         call cnpar('VALUE',ierr)
         call upperc(text)
         call lbgone(text)
         tlen=strlen(text)
*
* PARSE THE TEXT STRING AND PERFORM REQUESTED ACTION
*
         inc=.false.
*
* FIRST SEE IF A CHANGE OF MODE WAS REQUESTED
*
         if(index('CHANGE',text(:tlen)).eq.1) then
            change=.true.
            auto=.not.auto
*
* NEXT SEE IF USER WISHES TO FINISH THE EDITING WITHOUT SAVING THE
* OUTPUT
*
         else if(index('QUIT',text(:tlen)).eq.1) then
            more=.false.
            ierr=1
*
* NEXT SEE IF USER WISHES TO FINISH THE EDITING, SAVING THE OUTPUT
*
         else if(index('EXIT',text(:tlen)).eq.1.or.
     :           index('END',text(:tlen)).eq.1) then
            more=.false.
*
* NEXT SEE IF AUTO MODE REQUESTED EXPLICITLY
*
         else if(index('AUTO',text(:tlen)).eq.1) then
            change=.true.
            auto=.true.
*
* NEXT SEE IF MANUAL MODE REQUESTED EXPLICITLY
*
         else if(index('MANUAL',text(:tlen)).eq.1) then
            change=.true.
            auto=.false.
*
* SEE IF PIXEL IS TO BE SET INVALID
*
         else if(index('INVALID',text(:tlen)).eq.1) then
            image(pixel,line)=rinval
            inc=.true.
*
* IF NONE OF THESE TRY TO TRANSLATE THE TEXT INTO A PIXEL VALUE
*
         else
            call ctor(text(:tlen),value,ierr)
            if(ierr.eq.0) then
               image(pixel,line)=value
               inc=.true.
            else
               call wrerr('WHAT')
            endif
         endif
*
* INCREMENT CURRENT PIXEL IF REQUIRED
*
         if(inc) then
            pixel=pixel+1
            if(pixel.gt.npix) then
               pixel=1
               line=line+1
               if(line.gt.nlin) then
                  call wrerr('END')
                  line=nlin
                  pixel=npix
               endif
            endif
         endif
*
* GO ROUND FOR NEXT PIXEL UNLESS LOOPING IS SUPRESSED
*
         if(.not.loop) more=.false.
      enddo
*
* FINISH
*
      end
