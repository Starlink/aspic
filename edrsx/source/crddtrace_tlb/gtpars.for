      subroutine gtpars(ballno,ndets,nomdet,detout,ndtout,device,
     :                  intrac,title,cursor,spaces,offset,lgfile,lgunit,
     :                  ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To aquire the parameter values required by program
*       CRDDTRACE
*
*SOURCE
*       GTPARS.FOR in CRDDTRACE.TLB
*
*ARGUMENTS
*   INPUTS:
*       ballno(ndets)   integer  An array containing the Ball
*                                no. of each detector in the
*                                currently selected band, in order
*                                of increasing z
*       ndets   integer          The no. of detectors in the current band.
*       nomdet  integer          The identifier of the detector which is
*                                closest to the centre of the object
*       lgunit  integer          Fortran logical unit no. on which log
*                                file is to be opened.
*   OUTPUTS:
*       detout(ndets)   integer  A list of detectors selected for
*                                display. Each detector is identified
*                                by its position ordered by increasing z.
*       ndtout  integer          The no. of detectors selected for display.
*                                The identifiers of the selected detectors
*                                are stored in the first ndtout elements
*                                of array detout.
*       device  character*(*)    The SGS workstation identifier of the
*                                device on which the output is to appear.
*       intrac  logical          A flag. If .true. then the user is given
*                                the option of re-displaying the data with
*                                different parameter values.
*       title   character*(*)    A title for the display
*       cursor  logical          True if user wishes to select commands
*                                and parameters by using the cursor
*       spaces  character*(*)    The method to use for calculating the
*                                trace offsets on the final display.
*                                Can be 'CONSTANT','AVERAGE' or 'FREE'.
*       offset(ndets)  real      An offset for each trace, only set if
*                                'spaces' is 'FREE'.
*       filnam  character*(*)    Name of text file opened for log.
*                                Returned blank if no file opened.
*       ierr    integer          Status return
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gtstrn,getdev,wrerr,gtfile
*       THIS PACKAGE (CRDDTRACE.TLB):
*               gtdets,gtoffs
*       EDRS:
*               getpar
*       INTERIM:
*               rdkeyc
*
*STARLINK PARAMETERS
*       LOGFILE/read/   Name of text file for log of data values, etc.
*       DEFDETS/read/   Default no. of detector traces to display
*       REDRAW/read/    If true then interactive session is requested
*       DETS/read/      Actual list of detectors to display
*       DEVICE/read/    Device for graphics output
*       CURSOR/read/    True if cursor is to be used for command and
*                       parameter selection
*       TITLE/read/     Title for display
*       SPACING/read/   The type of offset spacing to use
*       OFFSET/read/    The offset values for each trace if SPACES=FREE
*       NODEFDETS/error/Accessed if parameter DEFDETS could not be got
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer           ndets,ballno(ndets),detout(ndets),ndtout,ierr
      integer           nomdet,lgunit
      character*(*)     device,title,spaces,lgfile
      logical           intrac,cursor
      real              offset(ndets)
*
* DECLARE LOCAL VARIABLES
*
      integer           defdet  ! Default no. of detectors to display
      integer           hidet   ! Highest default detector no.
      integer           i       ! Implied loop count
      integer           idet    ! Detector loop count
      integer           ival    ! Dummy integer argument
      integer           lodet   ! Lowest default detector no.
      integer           ltext   ! No. of characters in text
      character*80      prbuf   ! Buffer for text sent to user terminal
      real              rval    ! Dummy real argument
      character*80      text    ! Temporary text buffer
      character*80      ttl(1)  ! Buffer for title
*
* OPEN A FILE FOR LOG OF DATA VALUES AND POINT SOURCE PROFILE PARAMETERS
* (USER HAS OPTION OF LOGGING NOT BEING PERFORMED)
*
      lgfile='NONE'
      call gtfile('LOGFILE',lgunit,.true.,'NEW',lgfile,ierr)
*
* DETERMINE A DEFAULT SET OF DETECTORS TO DISPLAY CONSISTING OF THE
* NOMINALLY CENTRAL DETECTOR AND UPTO A NUMBER ON EITHER SIDE GIVEN BY
* PARAMETER 'DEFDETS'
*
      defdet=5
      call getpar('DEFDETS','INTEGER',1,1.0,16.0,.true.,defdet,rval,
     :            ierr)
      if(ierr.ne.0) then
         call wrerr('NODEFDETS')
         goto 999
      endif
      lodet=max(nomdet-defdet/2,1)
      hidet=min(nomdet+defdet/2,ndets)
      ndtout=hidet-lodet+1
      do idet=lodet,hidet
         detout(idet-lodet+1)=idet
      enddo
*
* GET A SET OF DETECTORS TO DISPLAY FROM THE USER USING ABOVE DEFAULTS
*
      call gtdets('DETS',detout,ballno,ndets,ndtout,ierr)
      if(ierr.ne.0) goto 999
*
* DETERMINE IF USER WANTS THE OPTION OF RE-DRAWING THE PLOTS WITH
* DIFFERENT PARAMETER VALUES
*
      ival=2
      call gtstrn('REDRAW',.true.,'YES,NO,TRUE,FALSE.',1,ival,
     :             text,ltext,ierr)
      if(ierr.ne.0) goto 999
      if(mod(ival,2).eq.0) then
         intrac=.false.
      else
         intrac=.true.
      endif
*
* GET SGS WORKSTATION IDENTIFIER OF GRAPHICS DEVICE FOR OUTPUT
*
      device=' '
      call getdev('DEVICE',device,.false.,ierr)
      if(ierr.ne.0) goto 999
*
* SEE IF USER WISHES TO USE A CURSOR FOR SELECTING COMMANDS AND
* PARAMETERS, RATHER THAN THE KEYBOARD
*
      ival=2
      call gtstrn('CURSOR',.true.,'YES,NO,TRUE,FALSE.',1,ival,
     :             text,ltext,ierr)
      if(ierr.ne.0) goto 999
      if(mod(ival,2).eq.0) then
         cursor=.false.
      else
         cursor=.true.
      endif
*
* GET TRACE OFFSET SPACING METHOD
*
      ival=1
      call gtstrn('SPACING',.true.,'CONSTANT,AVERAGE,FREE.',1,ival,
     :            spaces,ltext,ierr)
      if(ierr.ne.0) goto 999
*
* IF 'FREE' WAS SPECIFIED, THEN GET THE CORRECT NUMBER OF OFFSETS
*
      if(spaces.eq.'FREE') call gtoffs('OFFSET',offset,detout,ballno,
     :                                  ndets,ndtout,ierr)
      if(ierr.ne.0) goto 999
*
* GET A TITLE FOR THE DISPLAY
*
      ttl(1)=title
      call rdkeyc('TITLE',.true.,1,ttl,ival,ierr)
      if(ierr.eq.1) ierr=0
      title=ttl(1)
*
* FINISH
*

 999  continue
      end
