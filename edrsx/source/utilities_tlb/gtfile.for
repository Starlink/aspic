      subroutine gtfile(pname,unit,null,flstat,filnam,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To open a text file for subsequent processing by Fortran
*       read and write operations. The file name is obtained from the
*       user. NONE can also be specified (see method).
*
*SOURCE
*       GTFILE.FOR in UTILITIES.TLB
*
*METHOD
*       The user is prompted repeatedly for a text file name until
*       either one is obtained and opened succesfully, or the maximum
*       number of bad values allowed is exceeded. If the user replies
*       NONE, then if allowed by argument 'null', no file is opened but
*       the error status set appropriately.
*
*ARGUMENTS
*   INPUTS:
*       pname   character       Parameter name with which file is
*                               associated
*       unit    integer         The fortran unit number to open the
*                               file on
*       null    logical         If true then the user can select to open
*                               no file by specifying NONE
*       flstat  character       The file status to use in the OPEN
*                               statemnet eg 'NEW', 'OLD' or 'UNKNOWN'
*       filnam  character       Default file name.
*   OUTPUTS:
*       filnam  character       The name of the file opened. If no file
*                               is opened filnam is returned blank.
*       ierr    integer         Status return 0 - Success
*                                             1 - Success, but no file
*                                                 opened.
*                                             2 - Too many bad values
*                                                 given (reported).
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               ustrln,wrerr
*       INTERIM:
*               rdkeyc,cnpar
*
*STARLINK PARAMETERS
*       pname/read/     Parameter with which to aquire file name
*       NONULL/error/   Accessed if a null value is given and NULL is
*                       .false.
*       NOTEXT/error/   Accessed if a non-text file is given
*       NOACCESS/error/ Accessed if the file cannot be accessed
*
*VAX SPECIFICS
*       implicit none
*       Carriagecontrol keyword in OPEN
*       end of line comments
*       do while
*       enddo
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/1/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer           unit,ierr
      logical           null
      character*(*)     pname,flstat,filnam
*
* DECLARE LOCAL VARIABLES
*
      character access*10 ! Access mode for file, sequential or append
      character deflt*80! Default file name
      integer   fillen  ! Length of string filnam minus trailing blanks
      integer   ival    ! Dummy integer argument
      logical   more    ! True if a good file has not yet been obtained
      integer   nbad    ! The current number of bad files given by user
      logical   nofile  ! True if user wishes no file to be opened
      logical   shodef  ! True if default filename is to be displayed
      integer   ustrln  ! A function giving length of a string minus any
                        ! trailing spaces or control characters
      character txfile*3! YES if given file is a text file

*
* DETERMINE ACCESS MODE, SEQUENTIAL IF FILE STATUS IS 'OLD' OR 'NEW',
* 'APPEND' IF STATUS IS 'UNKNOWN'
*
      if(index('UNKNOWN',flstat).eq.1) then
         access='APPEND'
      else
         access='SEQUENTIAL'
      endif

*
* INITIALIZE BAD FILE COUNTER, SAVE DEFAULT FILE AND DETERMINE WHETHER
* OR NOT TO DISPLAY THE DEFAULT VALUE TO THE USER
*
      nbad=0
      if(filnam.eq.' '.or.filnam.eq.'NONE') then
         if(null) then
            deflt='NONE'
            shodef=.true.
         else
            deflt=' '
            shodef=.false.
         endif
      else
         deflt=filnam
         shodef=.true.
      endif
*
* LOOP UNTIL A GOOD FILE IS OBTAINED OR THE MAX NO. OF BAD FILES HAS
* BEEN GIVEN
*
      more=.true.
      do while(more)
*
* GET A FILE NAME FROM THE USER
*
         nofile=.false.
         filnam=deflt
         call rdkeyc(pname,shodef,1,filnam,ival,ierr)
         fillen=ustrln(filnam)
*
* THE VALUE "NONE" MEANS THE USER IS ASKING FOR NO FILE TO BE OPENED
*
         if(index('NONE',filnam(:fillen)).eq.1.or.
     :      index('none',filnam(:fillen)).eq.1) then
            nofile=.true.
*
* ALSO, A NULL RETURN WHEN THE DEFAULT IS NULL MEANS THE USER IS ASKING
* FOR NO FILE TO BE OPENED
*
         else
            if(ierr.eq.1.and.deflt.eq.' ') nofile=.true.
         endif
*
* IF THE USER WISHES NOT TO OPEN A FILE AND THIS IS ALLOWED BY THE
* CALLING ROUTINE, QUIT
*
         if(nofile) then
            if(null) then
               more=.false.
*
* OTHERWISE GIVE AN ERROR MESSAGE AND FLAG THAT NO FILE NAME WAS GIVEN
*
            else
               call wrerr('NONULL')
            endif
            filnam=' '
            ierr=1
*
* IF A FILE NAME HAS BEEN OBTAINED ATTEMPT TO OPEN IT
*
         else if(filnam.ne.' ') then
            open(unit,file=filnam(:fillen),status=flstat,
     :           carriagecontrol='LIST',access=access,iostat=ierr)
*
* IF SUCCESSFUL THEN SEE IF FILE IS A TEXT FILE
*
            if(ierr.eq.0) then
               inquire(unit,formatted=txfile)
*
* IF IT ISN'T THEN SEND A MESSAGE TO USER, AND FLAG AN ERROR
*
               if(txfile.ne.'YES') then
                  call wrerr('NOTEXT')
*
* IF IT IS A TEXT FILE THEN FILE HAS BEEN SUCCESFULLY GOT, SO QUIT
*
               else
                  more=.false.
               endif
*
* IF FILE COULD NOT BE OPENED SEND MESSAGE
*
            else
               call wrerr('NOACCESS')
            endif
         endif
*
* IF A BAD VALUE WAS GIVEN FOR THE TEXT FILE, THEN GET ANOTHER FILE SO
* LONG AS MAXIMUM NUMBER OF BAD FILE NAMES HAS NOT BEEN EXCEEDED
*
         if(more) then
            nbad=nbad+1
            if(nbad.gt.3) then
               call wrerr('TOOBAD')
               ierr=2
               more=.false.
               filnam=' '
            else
               call cnpar(pname,ierr)
            endif
         endif
*
* LOOP FOR NEXT ATTEMPT
*
      enddo
*
* FINISH
*
 999  continue

      end
