      subroutine zapuse
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Removes lines containing "USED BY" from file headers and
*       all succesive lines until a line is found which has just a
*       * on it or a line which has no gap between the * and the next
*       character.
*
*SOURCE
*       ZAPUSE.FOR in ZAPUSED.TLB
*
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               STRLEN,GTFILE,CMDLIN,ZAPHED
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      character dfltfl*80       ! The default file name for the output
      integer   dfltln          ! Length of string dfltfl actually used
      logical   echo            ! True if user wants output file echoed
                                ! on terminal screen
      integer   ierr            ! Error status
      character infile*80       ! Name of input file
      logical   option          ! true if user wants to be presented
                                ! with options (otherwise assume default
                                ! options)
      character outfil*80       ! Name of output file
      character params(10)*80   ! List of parameters used in invokation
      integer   semico          ! The position within the input file
                                ! name of a semicolon
      integer   strlen          ! Function giving length of string minus
                                ! trailing spaces and control characters
*
* CALL CMDLIN TO PROCESS COMMAND LINE AND SEE WHAT OPTIONS USER REQUIRES
*
      call cmdlin(echo,option,params,10,ierr)
      if(ierr.ne.0) goto 999
*
* USE THE FIRST INVOKATION PARAMETER AS THE FIRST CHOICE FOR THE
* INPUT FILE NAME. IF NO PARAMETERS WERE GIVEN PROMPT USER FOR
* A FILE NAME
*
      if(params(1).ne.' ') then
         infile=params(1)
      else
         infile=' '
      endif
*
* OPEN INPUT FILE ON UNIT 10
*
      call gtfile(10,' Input text file ?: ','OLD',infile,ierr)
      if(ierr.ne.0) goto 999
*
* USE THE INPUT FILE NAME AS A DEFAULT FOR THE OUTPUT FILE BUT ENSURE
* THAT NO VERSION NUMBER IS ATTACHED
*
      semico=index(infile,';')
      if(semico.eq.0) then
         dfltfl=infile
      else
         dfltfl=infile(:semico-1)
      endif
      dfltln=strlen(dfltfl)
*
* SET OUTPUT FILE TO NAME GIVEN AS 2ND INVOKATION PARAMETER
* IF USER HAS NO OPTION USE INPUT FILE NAME, OTHERWISE
* PROMPT HIM FOR A FILE NAME
*
      if(params(2).ne.' ') then
         outfil=params(2)
      else
         if(option) then
            outfil=' '
         else
            outfil=dfltfl
         endif
      endif
*
* OPEN OUTPUT FILE ON UNIT 11
*
      call gtfile(11,' Output text file (default= '//
     :           dfltfl(:dfltln)//') ?: ','NEW',outfil,ierr)
*
* IF NULL VALUE WAS GIVEN OPEN THE DEFAULT FILE
*
      if(ierr.eq.-1) then
         outfil=dfltfl
         call gtfile(11,' Output text file ?: ','NEW',outfil,ierr)
      endif
      if(ierr.ne.0) goto 999
*
* CALL ZAPHED TO READ THE INPUT FILE AND REMOVE "USED BY" LINES
*
      call zaphed('USED BY',echo)
*
* CLOSE FILES AND FINISH
*
 999  close(10)
      close(11)

      end
