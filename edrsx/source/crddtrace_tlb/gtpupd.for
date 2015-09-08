      subroutine gtpupd(xlim,ymax,detout,ndtout,ndets,device,
     :                  ballno,bzone,usecur,title,spaces,offset,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Allows the user to update the parameter values being used by
*       program CRDDTRACE using either the cursor or the keyboard
*
*SOURCE
*       GTPUPD.FOR in CRDDTRACE.TLB
*
*METHOD
*       The effect of the following parameters may be changed:
*               DETS
*               DEVICE
*               XLIMS
*               YMAX
*               CURSOR
*               TITLE
*               SPACING
*               OFFSETS
*       In addition, HELP may be selected. The user specifies which
*       parameter he wishes to update by either giving the parameter
*       name when prompted for 'PARAM', or by placing the cursor on
*       a labelled square on the graphics device.
*
*ARGUMENTS
*   INPUTS:
*       usecur  logical         True if cursor is to be used for getting
*                               parameters to be updated
*       All output arguments contain default values on entry (except
*        IERR)
*   OUTPUTS:
*       ballno(ndets)   integer An array of ballnos for detectors in
*                               this band
*       detout(ndets)   integer An array of detector ids for display
*       device        character SGS workstation id of graphics device
*       ndtout          integer The no. of detectors to display
*       ndets           integer The size of arrays ballno and detout
*       xlim(2)         real    The limits of x axis in arcmins from
*                               centre
*       ymax            real    The upper limit of y axis in flux units
*       bzone           integer Current SGS base zone identifier
*       title         character Title for display
*       spaces        character String describing method of calculating
*                               the offset for each data trace. One of
*                               'CONSTANT','AVERAGE','FREE'.
*       offset(ndets)   real    An array holding the offsets for each
*                               data trace. The user gives values for
*                               these if he specifies 'FREE' as for the
*                               parameter 'SPACING', otherwise they are
*                               calculated by the program.
*       ierr            integer Status return
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               crtcmd,getdev,gtlims,wrerr,gtstrn
*       THIS PACKAGE (CRDDTRACE.TLB):
*               gtdets
*       EDRS:
*               getpar
*       INTERIM:
*               cnpar,wruser
*       SGS:
*               sgs_clswk,sgs_opnwk
*
*STARLINK PARAMETERS
*       PARAM/read/     Name of parameter to update
*       DETS/read/      List of detectors to display
*       DEVICE/read/    Device for graphics output
*       OFFSET/read/    List of offsets for data traces
*       SPACING/read/   Method of calculating offsets for data traces
*       XLIMS/read/     Limits of x axis in arcmins
*       YMAX/read/      Max limit of y axis in flux units
*       NONEUPD/error/  Accessed if the specified parameter couldn't be
*                       updated
*       TOOBAD/error/   Accessed if too many parameters couldn't be
*                       updated.
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer           ierr,ndets,detout(ndets),ballno(ndets),
     :                  ndtout,bzone
      character*(*)     device,title,spaces
      real              xlim(2),ymax,offset(ndets)
      logical           usecur
*
* DECLARE LOCAL VARIABLES
*
      integer   ival    ! Dummy integer argument
      integer   ltext   ! Length of string returned by GTSTRN
      logical   more    ! More parameters may be altered
      integer   nbad    ! The no. of failed attempts at updating parameters
      integer   npar    ! The position of the parameter within list in
                        ! CRTCMD
      character ttl(1)*80 ! Buffer for display title
*
* SEE WHICH PARAMETER THE USER WISHES TO UPDATE NEXT
*
      more=.true.
      do while(more)
         npar=0
         call cnpar('PARAM',ierr)
         npar=1
         call crtcmd('PARAM','Redraw display,Detectors,Device,X '//
     :               'limits,Y max,Title,Command source,Offset '//
     :               'spacing,Help.',0.0,1.0,
     :               0.0,0.15,npar,'Select a parameter by '//
     :               'positioning cursor and pressing any key',usecur,
     :               ierr)
         if(ierr.eq.0) then
*
* IF REQUIRED UPDATE 'DETS' PARAMETER
*
            if(npar.eq.2) then
               call cnpar('DETS',ierr)
               call gtdets('DETS',detout,ballno,ndets,ndtout,
     :                     ierr)
               if(ierr.eq.0) then
                  if(spaces.ne.'CONSTANT'.and.spaces.ne.'AVERAGE') then
                     call wruser(' ',ierr)
                     call wruser(' Reverting to AVERAGE offset spacing',
     :                                ierr)
                     call wruser(' ',ierr)
                     spaces='AVERAGE'
                  endif
               endif
*
* IF REQUIRED, UPDATE GRAPHICS DEVICE
*
            else if(npar.eq.3) then
               call cnpar('DEVICE',ierr)
               call getdev('DEVICE',device,.false.,ierr)
               if(ierr.eq.0) call sgs_clswk(bzone,ierr)
               if(ierr.eq.0) call sgs_opnwk(device,bzone,ierr)
               if(ierr.eq.0) more=.false.
*
* IF REQUIRED, GET NEW LIMITS FOR X
*
            else if(npar.eq.4) then
               call cnpar('XLIMS',ierr)
               call gtlims('XLIMS',xlim,ierr)
*
* IF REQUIRED, GET NEW LIMITS FOR Y
*
            else if(npar.eq.5) then
               call cnpar('YMAX',ierr)
               call getpar('YMAX','REAL',1,0.0,1e30,.true.,
     :                     ival,ymax,ierr)
*
* IF REQUIRED, GET A NEW TITLE FOR THE DISPLAY
*
            else if(npar.eq.6) then
               call cnpar('TITLE',ierr)
               ttl(1)=title
               call rdkeyc('TITLE',.true.,1,ttl,ival,ierr)
               if(ierr.eq.1) ierr=0
               if(ierr.eq.0) title=ttl(1)
*
* IF REQUIRED, SELECT THE ALTERNATIVE SOURCE FOR COMMANDS AND PARAMETERS
*
            else if(npar.eq.7) then
               usecur=.not.usecur
               call wruser(' ',ierr)
               if(usecur) then
                  call wruser('Using cursor for command source',ierr)
               else
                  call wruser('Using keyboard for command source',ierr)
               endif
               call wruser(' ',ierr)
*
* IF REQUIRED, GET TRACE OFFSET SPACING METHOD
*
            else if(npar.eq.8) then
               if(spaces.eq.'CONSTANT') then
                  ival=1
               else if(spaces.eq.'AVERAGE') then
                  ival=2
               else
                  ival=3
               endif
               call cnpar('SPACING',ierr)
               call gtstrn('SPACING',.true.,'CONSTANT,AVERAGE,FREE.',
     :                     1,ival,spaces,ltext,ierr)
*
* IF 'FREE' WAS SPECIFIED, THEN GET THE CORRECT NUMBER OF OFFSETS
*
              if(spaces.eq.'FREE') then
                 call cnpar('OFFSET',ierr)
                 call gtoffs('OFFSET',offset,detout,ballno,ndets,ndtout,
     :                       ierr)
              endif
*
* IF REQUIRED, DISPLAY HELP TEXT ABOUT THE PARAMETER 'PARAM'
*
            else if(npar.eq.9) then
               call gthelp('PARAM','HELP',ierr)
               ierr=0
*
* IF USER SELECTED 'Redraw Display' THEN DONT UPDATE ANY MORE PARAMETERS
*
            else
               more=.false.
            endif
*
* IF AN ERROR OCCURED GETTING THE PARAMETER, TELL USER AND INCREMENT
* ERROR COUNT
*
            if(ierr.ne.0) then
               call wrerr('NONEUPD')
               nbad=nbad+1
            else
               nbad=0
            endif
*
* IF ERROR COUNT BECOMES TOO HIGH EXIT
*
            if(nbad.gt.3) then
               more=.false.
               call wrerr('TOOBAD')
               ierr=16
            endif
*
* IF NULL VALUE WAS GIVEN FOR PARAMETER NAME, THEN NO MORE PARAMETERS
* ARE TO BE UPDATED
*
            if(npar.eq.0) then
               more=.false.
            endif
*
* IF AN ERROR OCCURED GETTING THE PARAMETER NAME, QUIT
*
         else
            more=.false.
         endif
      enddo
*
* FINISH
*

      end
