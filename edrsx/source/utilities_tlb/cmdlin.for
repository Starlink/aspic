      subroutine cmdlin(echo,option,params,npar,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To see if the calling program was invoked with either of the
*       qualifiers (NO)ECHO or (NO)OPTIONS, or with parameters.
*
*       PARAMETERS:
*               input file   output file
*
*       QUALIFIERS:
*       ECHO    causes the calling program to display output on terminal
*               screen
*       OPTIONS causes the user to be prompted for options controlling
*               the operation of the calling program.
*
*SOURCE
*       CMDLIN.FOR in UTILITIES.FOR
*
*METHOD
*       The program should be defined as a foreign command in DCL, for
*       instance for program ZAPTAB:
*
*               $ZAPTAB:==$DIRECTORY:ZAPTAB
*
*       where DIRECTORY is a logical name for the directory in which
*       ZAPTAB.EXE is stored. The Run-Time-Library routine
*       LIB$GET_FOREIGN is used to retrieve the command line used to
*       invoke ZAPTAB without the verb, eg if user typed
*
*               $ZAPTAB/EC/NOOPT
*
*       then the RTL routine would return  "/EC/NOOPT
*
*ARGUMENTS
*   INPUTS:
*       npar    integer         The size of array params
*   OUTPUTS:
*       echo    logical         True if echo is requested by user
*       option  logical         True if options are requested by user
*       params  character       Parameter values. Blank if non given on
*                               command line.
*       ierr    integer         Error status  0 - Success
*                                             1 - Unrecognized qualifier
*                                             2 - Ambiguous qualifier
*                                       (all errors are reported)
*
*USED BY
*       ZAPTAB
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               ustrln
*       EDRS:
*               LBGONE
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 2/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      logical   echo,option
      integer   ierr,npar
      character params(npar)*(*)
*
* DECLARE LOCAL VARIABLES
*
      character cmd*80  ! String to hold command line
      integer   ipar    ! Parameter counter
      integer   isp     ! Position of end of current qualifier
      integer   isp1    ! Position of the 1st space seperator in cmd
      integer   isp2    ! Position of the 2nd slash seperator in cmd
      integer   lib$get_foreign ! RTL routine to get command line
      integer   len     ! Length of command line string cmd
      integer   nv      ! The number of valid qualifiers which match
                        ! current qualifier field
      integer   ustrln  ! Function giving used length of a string
*
* SET UP DEFAULT VALUES FOR QUALIFIERS
*
      echo=.true.
      option=.true.
*
* GET THE FOREIGN COMMAND LINE THAT INVOKED MEMO
*
      ierr=lib$get_foreign(cmd,,len,)
      if(.not.ierr) call lib$stop(%val(ierr))
      call lbgone(cmd)
*
* PARSE THE COMMAND LINE TO OBTAIN ANY QUALIFIERS OR COMMANDS
* LIB$GET_FOREIGN RETURNS THE COMMAND LINE THAT INVOKED THE CURRENT
* IMAGE WITH THE CURRENT IMAGE NAME REMOVED. THE FIRST CHARACTER IN
* CMD IS THE FIRST CHARACTER OF ANY POST VERB TEXT ON THE COMMAND LINE.
* THEREFORE IF THE FIRST CHARACTER IS A SLASH ("/") THEN THE FIRST
* FIELD IN CMD IS A QUALIFIER.
*
      ierr=0
  10  if(cmd(1:1).eq.'/') then
*
* REMOVE THE SLASH AND ANY FOLLOWING BLANKS
*
         cmd(1:1)=' '
         call lbgone(cmd)
*
* THE FIRST FIELD IS TERMINATED BY EITHER A SPACE OR ANOTHER SLASH
*
         isp1=index(cmd,' ')-1
         isp2=index(cmd,'/')-1
*
* IF THERE ARE NO MORE SLASHES ON THE COMMAND LINE, THE QUALIFIER
* FIELD ENDS AT CHARACTER ISP1
*
         if(isp2.eq.-1) then
            isp=isp1
*
* OTHERWISE THE FIELD ENDS AT EITHER THE FIRST SLASH OR THE FIRST SPACE
*
         else
            isp=min(isp1,isp2)
         endif
*
* ISP IS THE LENGTH OF THE FIRST FIELD. IF IT IS ZERO THEN THERE
* IS NO QUALIFIER OR COMMAND.
*
         if(isp.ne.0) then
*
* COMPARE THE FIELD WITH EACH VALID QUALIFIER TO IDENTIFY IT
*
            nv=0
            if(index('ECHO',cmd(:isp)).eq.1) then
               nv=nv+1
               echo=.true.
            endif
            if(index('NOECHO',cmd(:isp)).eq.1) then
               nv=nv+1
               echo=.false.
            endif
            if(index('OPTIONS',cmd(:isp)).eq.1) then
               nv=nv+1
               option=.true.
            endif
            if(index('NOOPTIONS',cmd(:isp)).eq.1) then
               nv=nv+1
               option=.false.
            endif
*
* CHECK FOR UNRECOGNIZED OR AMBIGUOUS QUALIFIERS
*
            if(nv.eq.0) then
               write(*,*) ' *** UNRECOGNIZED QUALIFIER "/',cmd(:isp),
     :                    '"'
               ierr=1
            else if(nv.gt.1) then
               write(*,*) ' *** AMBIGUOUS QUALIFIER "/',cmd(:isp),'"'
               ierr=2
            else
               ierr=0
            endif
         else
*
* SEND MESSAGE IF A SLASH WIHOUT A QUALIFIER WAS FOUND
*
            write(*,*) ' *** BLANK QUALIFIER FIELD BEING IGNORED'
         endif
*
* REMOVE THE CURRENT QUALIFIER FIELD FROM CMD
*
         cmd(:isp)=' '
         call lbgone(cmd)
*
* GO ROUND TO SEE IF THERE ARE ANY MORE QUALIFIERS
*
         goto 10
      endif
*
* IF THERE IS ANYTHING LEFT ON THE COMMAND LINE THEN IT IS A PARAMETER
* EXTRACT ALL PARAMETERS
*
      do ipar=1,npar
         if(cmd.ne.' ') then
            isp1=index(cmd,' ')-1
            params(ipar)=cmd(:isp1)
            cmd(:isp1)=' '
            call lbgone(cmd)
         else
            params(ipar)=' '
         endif
      enddo
*
* FINISH
*
      end
