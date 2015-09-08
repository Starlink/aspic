      subroutine getfor(string,prompt)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Simulates LIB$GET_FOREIGN, but provides command line recall
*       and editing facilities similar to DCL.
*
*SOURCE
*       GETFOR.FOR in UTILITIES.TLB
*
*METHOD
*       On first call, get the foreign command line using CLI$GET_VALUE.
*       If this contains nothing but the command verb, then the user is
*       prompted, using routine GETINP. On subsequent calls, the user is
*       prompted immediately.
*
*ARGUMENTS
*   INPUTS:
*       prompt  character       The user prompt
*   OUTPUTS:
*       string  character       The returned string
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              getinp,ustrln
*       EDRS:
*              lbgone
*       Run Time Library:
*              lib$signal,cli$get_value,str$upcase
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 30/9/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      character string*(*),prompt*(*)

*
* DECLARE LOCAL VARIABLES
*
      integer   cli$get_value ! Status from CLI routine
      integer   end           ! Position of end of command line
      logical   first         ! True if this is the first time through
                              ! this routine since image start up
      integer   istat         ! Error status
      integer   slash         ! Position of 1st slash in command line
      integer   slen          ! String length
      integer   space         ! Position of 1st space in command line
      integer   ustrln        ! Function giving useful len. of a string

      data first /.true./
      save first

*
* IF THIS IS THE FIRST TIME THROUGH THIS ROUTINE GET THE COMMAND LINE
* WHICH STARTED UP THE CURRENT IMAGE
*
      if(first) then
         istat=cli$get_value('$LINE',string,slen)
         if(.not.istat) then
            call lib$signal(%val(istat))
            goto 999
         endif

*
* REMOVE THE COMMAND VERB FROM THE STRING
*
         call lbgone(string)
         end=ustrln(string)
         space=index(string,' ')
         if(space.eq.0) space=100000
         slash=index(string,'/')
         if(slash.eq.0) slash=100000
         string(:min(end,slash-1,space))=' '
         call lbgone(string)

*
* IF THIS LEAVES NOTHING IN THE STRING, PROMPT USER FOR A VALUE
*
         if(string.eq.' ') call getinp(string,prompt)

*
* FLAG THAT THE ROUTINE HAS BEEN ENTERED AT LEAST ONCE
*
         first=.false.

*
* IF ROUTINE HAS BEEN ENTERED BEFORE, PROMPT USER FOR A VALUE
*
      else
         call getinp(string,prompt)
      endif

*
* CONVERT STRING TO UPPERCASE
*
      call str$upcase(string,string)

*
* FINISH
*
  999 continue
      end





      subroutine getinp(string,prompt)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Gets a string of text from the users keyboard. The users is
*       given the specified prompt. DCL-like multiple command line
*       recall is available using cursor keys or control-B.
*
*SOURCE
*       GETINP.FOR in UTILITIES.TLB
*
*METHOD
*       The previous command lines are stored as logical names in the
*       process logical name table. They have the form RECALL_1,
*       RECALL_2 etc, upto RECALL_max (max is specified in a parameter
*       statement in the following code). These form a cyclic buffer in
*       which the latest command line is stored at the next higher
*       'slot' (i.e RECALL_1 follows RECALL_max etc). One slot is always
*       kept vacant and will recieve the next command line specified by
*       the user. This slot is pointed to by the logical name RECALL_PW.
*       This routine performs the followingf steps:
*       1) Get the location of the next vacant slot from the logical
*          name RECALL_PW. If it doesn't exist, assume slot is slot 1.
*       2) Get a text string from the user using routine rdkybd to
*          provide the command line recall facility.
*       3) Store the returned string as a logical name in the next slot
*          pointed to by RECALL_PW, but only if:
*               a) It is not identical to the previous command line
*               b) It is not a null string
*       4) Increment the value of RECALL_PW, cycling from max back to
*          1 when max is reached.
*
*ARGUMENTS
*   INPUTS:
*       prompt  character       The prompt string
*   OUTPUTS:
*       string  character       The returned text string from the user
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              resets,gtslot,rdkybd
*       EDRS:
*              lbgone
*       INTERIM:
*              ctoi,wruser
*       Run Time Library:
*              lib$sys_trnlog,lib$set_trnlog
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       RTL routines
*       VMS definitions include file
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/88
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE DEFINITIONS OF RUN TIME LIBRARY RETURN VALUES
*
      include '($SSDEF)'

*
* DECLARE ARGUMENTS
*
      character string*(*),prompt*(*)

*
* DECLARE LOCAL VARIABLES
*
      integer   istat           ! Error status
      integer   lenout          ! No. of characters entered by user
      integer   lib$set_logical ! Error status from lib$set_logical
      integer   lib$sys_trnlog  ! Error status from lib$sys_trnlog
      character lognam*11       ! Logical name in which the string
                                ! obtained from user will be stored
      character lstbuf*255      ! Previous command line from user
      integer   maxrec          ! Max no. of recalls allowed
      integer   minc            ! Statement function MOVE dummy argument
      integer   move            ! Statement function giving cyclic move-
                                ! ment round the buffer
      integer   mval            ! Statement function MOVE dummy argument
      integer   pw              ! No. of next slot for to be written
      character pwbuf*3         ! Character version of pw

*
* SET THE MAXIMUM NUMBER OF RECALLS ALLOWED
*
      parameter (maxrec=21)

*
* DEFINE FUNCTION WHICH RETURNS THE SLOT NUMBER WHICH IS A GIVEN
* INCREMENT AWAY FROM A STARTING SLOT. INCREMENTS CAN BE +VE OR -VE
*
      move(mval,minc)=mod(mval+minc+100*maxrec-1,maxrec)+1

*
* GET THE LOCATION OF THE NEXT VACANT SLOT FROM THE LOGICAL
* NAME RECALL_PW. IF IT DOESN'T EXIST, OR HAS BEEN CORRUPTED,
* INITIALISE THE RECALL LOGICAL NAME STRUCTURE
*
      istat=lib$sys_trnlog('RECALL_PW',,pwbuf,,,)
      if(istat.eq.SS$_NORMAL) then
         call ctoi(pwbuf,pw,istat)
         if(istat.ne.0) call resets(pw,maxrec)
      else
         call resets(pw,maxrec)
      endif

*
* GET A TEXT STRING FROM THE USER USING ROUTINE RDKYBD TO
* PROVIDE THE COMMAND LINE RECALL FACILITY.
*
      call rdkybd(string,prompt,lenout,pw,maxrec)

*
* STORE THE RETURNED STRING AS A LOGICAL NAME IN THE NEXT SLOT
* (POINTED TO BY RECALL_PW), BUT ONLY IF:
*               A) IT IS NOT IDENTICAL TO THE PREVIOUS COMMAND LINE
*               B) IT IS NOT A NULL STRING
*
      if(string.ne.' ') then
         call gtslot(move(pw,-1),lstbuf,maxrec,istat)
         if(lstbuf.ne.string) then
            write(lognam,'(''RECALL_'',I3)') pw
            call lbgone(lognam(8:))
            istat=lib$set_logical(lognam,string(:lenout))
            if(.not.istat) then
               call wruser('*** Unable to write to RECALL buffer',istat)
            else

*
* IF ALL IS OK, INCREMENT THE VALUE OF RECALL_PW, CYCLING FROM MAX
* BACK TO 1 WHEN MAX IS REACHED.
*
               pw=move(pw,1)
               write(pwbuf,'(I3)') pw
               istat=lib$set_logical('RECALL_PW',pwbuf)
               if(.not.istat) then
                  call wruser('*** Unable to write to RECALL buffer',
     :                        istat)
               endif
            endif
         endif
      endif

*
* FINISH
*
      end



      subroutine resets(pw,maxrec)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Initialises the logical name structure used by the "read
*       with command line recall" routine getinp.
*
*SOURCE
*       RESETS.FOR in UTILITIES.TLB
*
*METHOD
*       Set logical name RECALL_PW to "1".
*       Delete any logical names RECALL_i, where i is an integer.
*       If any such names were found and deleted, give "corruption
*       message.
*
*ARGUMENTS
*   INPUTS:
*       maxrec  integer         Max. number of RECALL slots
*   OUTPUTS:
*       pw      integer         The slot number in which the next
*                               user command line will be stored.
*
*SUBROUTINES CALLED
*       INTERIM:
*              wruser
*       EDRS:
*              lbgone
*       Run Time Library:
*              lib$set_logical,lib$delete_logical
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer pw,maxrec

*
* DECLARE LOCAL VARIABLES
*
      logical   flag            ! True if some log names were deleted
      integer   i               ! Loop count
      integer   istat           ! Error status
      integer   lib$set_logical ! Error status from RTL routine
      integer   lib$delete_logical ! Error status from RTL routine
      character lognam*11       ! Logical name in which the string
                                ! obtained from user will be stored

*
* RESET RECALL_PW TO 1
*
      pw=1
      istat=lib$set_logical('RECALL_PW','1')
      if(.not.istat) then
         call wruser('*** Unable to write to RECALL buffer',istat)
      endif
*
* DELETE ANY LOGICAL NAMES FOR RECALL SLOTS
*
      flag=.false.
      do i=1,maxrec
         write(lognam,'(''RECALL_'',I3)') i
         call lbgone(lognam(8:))
         istat=lib$delete_logical(lognam)
         if(istat) flag=.true.
      enddo

*
* IF ANY LOGICAL NAMES WERE DELETED GIVE MESSAGE
*
      if(flag) call wruser('*** RECALL buffer corrupted',istat)

*
* FINISH
*
      end



      subroutine gtslot(slot,string,maxrec,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the string stored in a specified RECALL slot.
*
*SOURCE
*       GTSLOT.FOR in UTILITIES.TLB
*
*METHOD
*       The value of slot actually used is wrapped round into the
*       range 1 to maxrec.
*       An attempt is made to translate the logical name RECALL_'slot'.
*       If this is successful, the equivalence string is returned.
*       Otherwise, the ierr is set to a non zero value and a null
*       string returned.
*
*ARGUMENTS
*   INPUTS:
*       slot    integer         Slot number required
*       maxrec  integer         Max number of slots defined
*   OUTPUTS:
*       string  character       Contents of required slot
*       ierr    integer         Error status 0 - Sucess
*                                            1 - Slot not defined
*
*SUBROUTINES CALLED
*       EDRS:
*              lbgone
*       Run Time Library:
*              lib$sys_trnlog
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/7/88
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE DEFINITIONS OF RUN TIME LIBRARY RETURN VALUES
*
      include '($SSDEF)'

*
* DECLARE ARGUMENTS
*
      integer   slot,maxrec,ierr
      character string*(*)

*
* DECLARE LOCAL VARIABLES
*
      integer   istat           ! Error status
      integer   lib$sys_trnlog  ! Error status from lib$sys_trnlog
      character lognam*11       ! Logical name in which the string
                                ! obtained from user will be stored
      integer   minc            ! Statement function MOVE dummy argument
      integer   move            ! Statement function giving cyclic move-
                                ! ment round the buffer
      integer   mval            ! Statement function MOVE dummy argument

*
* DEFINE FUNCTION WHICH RETURNS THE SLOT NUMBER WHICH IS A GIVEN
* INCREMENT AWAY FROM A STARTING SLOT. INCREMENTS CAN BE +VE OR -VE
*
      move(mval,minc)=mod(mval+minc+100*maxrec-1,maxrec)+1

*
* ATTEMPT TO TRANSLATE THE SLOT LOGICAL NAME
*
      write(lognam,'(''RECALL_'',I3)') move(slot,0)
      call lbgone(lognam(8:))
      istat=lib$sys_trnlog(lognam,,string,,,)

*
* IF UNSUCCESSFUL, RETURN A NULL STRING AND SET IERR=1
*
      if(istat.ne.SS$_NORMAL) then
         ierr=1
         string=' '
      else
         ierr=0
      endif

*
* FINISH
*
      end


      subroutine rdkybd(string,prompt,lenout,pw,maxrec)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Reads text from the keyboard. DCL-like command line recall
*       can be performed by using cursor or control keys. The recall
*       buffer is not updated (that is done by routine GETINP).
*
*SOURCE
*       RDKYBD.FOR in UTILITIES.TLB
*
*METHOD
*       This routine handles the selection of strings to display
*       when up or down arrow keys are pressed. The actual keyboard
*       read is done by routine rdkbd2.
*
*ARGUMENTS
*   INPUTS:
*       prompt  character       The user prompt
*       pw      integer         The slot no. at which the next string
*                               will be stored
*       maxrec  integer         Max no. of strings which can be stored
*   OUTPUTS:
*       string  character       Final string entered by user
*       lenout  integer         No. of characters entered by user
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtslot,rdkbd2
*       INTERIM:
*              wruser
*
*VAX SPECIFICS
*       do while
*       enddo
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/2/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer   lenout,pw,maxrec
      character string*(*),prompt*(*)

*
* DECLARE LOCAL VARIABLES
*
      character clear*7  ! String holding control codes to clear a line
      character dncurs*3 ! Character version of a cursor down terminator
      integer   istat    ! Error status
      integer   lprm     ! Length of prompt in characters
      integer   minc     ! Statement function MOVE dummy argument
      logical   more     ! True if user has not finished giving input
      integer   move     ! Statement function giving cyclic movement
                         ! round the buffer
      integer   mval     ! Statement function MOVE dummy argument
      character pbuf*255 ! Temporary buffer for prompt string
      integer   pr       ! Pointer to slot from which current recall
                         ! string was read
      character return*1 ! Character version of a RETURN terminator
      integer   slstat   ! 0 if specified slot defined, 1 otherwise
      character term*10  ! String which caused termination of user input
      character upcurs*1 ! Character version of a cursor up terminator

*
* SET UP CONTROL CHARACTER STRINGS AS PARAMETERS
*
      parameter (

     :    return = char(13),
     :    upcurs = char(2),
     :    dncurs = char(27)//'[B',
     :    clear  = char(13)//char(27)//'[K'//char(0)//char(0)//char(0)

     :          )

*
* DEFINE FUNCTION WHICH RETURNS THE SLOT NUMBER WHICH IS A GIVEN
* INCREMENT AWAY FROM A STARTING SLOT. INCREMENTS CAN BE +VE OR -VE
*
      move(mval,minc)=mod(mval+minc+100*maxrec-1,maxrec)+1

*
* COPY PROMPT TO TEMPORARY BUFFER
*
      pbuf=prompt
      lprm=len(prompt)

*
* INITIALISE POINTER TO CURRENTLY RECALLED COMMAND LINE SLOT
*
      pr=pw

*
* LOOP UNTIL THE USER TERMINATES A STRING WITH A RETURN (AS OPPOSED TO
* CURSOR UP OR DOWN KEYS)
*
      more=.true.
      do while(more)

*
* GET THE TEXT OF THE COMMAND BEING RECALLED (THE SLOT BEING WRITTEN
* TO IS CONSIDERED TO BE BLANK). IF SLOT IS NOT YET DEFINED, SLSTAT
* IS SET TO 1
*
         if(pr.ne.pw) then
            call gtslot(pr,string,maxrec,slstat)
         else
            string=' '
         endif

*
* GET A STRING FROM THE USER TERMINATED WITH EITHER RETURN, UP ARROW OR
* DOWN ARROW
*
         call rdkbd2(string,pbuf,lprm,term,lenout)

*
* UPDATE THE COMMAND TO RECALL IF STRING WAS TERMINATED BY A CURSOR KEY
*
         if(term.eq.upcurs) then
            if(pr.ne.move(pw,1).and.slstat.eq.0) pr=move(pr,-1)

         else if(term.eq.dncurs) then
            if(pr.ne.pw) pr=move(pr,+1)

*
* IF BUFFER WAS FILLED GIVE MESSAGE AND TRY AGAIN
*
         else if(term.eq.' ') then
            call wruser('*** Input buffer full',istat)

*
* IF STRING WAS TERMINATED WITH ANYTHING OTHER THAN AN ARROW KEY,
* ASSUME THAT USER HAS FINISHED GIVING INPUT
*
         else

*
* CHECK WHETHER STRING ENTERED IS A RECALL COMMAND. IF SO PROCESS IT.
*
            call recall(string,pr,pw,maxrec,istat)
*
* IF IT WASN'T A VALID RECALL COMMAND OR IF IT WAS A "RECALL/ALL
* COMMAND THEN EXIT
*
            if(istat.ne.0) more=.false.
         endif

*
* MODIFY THE PROMPT STRING TO INCLUDE A CARRAGE RETURN AND LINE CLEAR
* AT THE START. THIS IS SO THAT ANY FURTHER PROMPTS WILL OVERLAY THE
* ONE AND NOT GET ADDED ON THE END
*
         pbuf=clear//prompt
         lprm=len(prompt)+7

*
* LOOP ROUND TO DISPLAY NEXT RECALL STRING AND GET A NEW STRING FROM
* USER
*
      enddo

*
* FINISH
*
      end



      subroutine rdkbd2(string,prompt,lprom,term,lenout)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Prompts the user and reads a string from the keyboard. The
*       end of the string is marked by the user pressing either
*       RETURN (or control-Z), or either the up or down cursor keys.
*       The ascii form of the terminator is returned together with
*       the string and the number of characters read.
*           The contents of the argument "string" on entry are pre-
*       loaded into the read buffer before prompting the user.
*
*SOURCE
*       RDKBD2.FOR in UTILITIES.TLB
*
*METHOD
*       The system service QIOW is used to read the text from the user.
*       The extended facilities available with an "itemlist read" are
*       used. These anable (for instance) the default line recall to be
*       switched off, a prompt to be specified, and allows escape
*       sequences to act as terminators of the text (such as generated
*       by the cursor down key for instance). For more info. see
*       VAX/VMS vol.10A "IO USERS PART 1" chapter 8.
*
*ARGUMENTS
*
*   INPUTS:
*       string  character       Initial contents of read buffer
*       prompt  character       Prompt string
*       lprom   integer         No. of useful characters in prompt
*
*   OUTPUTS:
*       term    character       Termination sequence used by user
*       lenout  integer         No. of chars entered by user excluding
*                               terminator
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               ustrln
*       VMS SYSTEM SERVICES:
*               sys$assign,sys$dassgn,sys$qiow
*       Run Time Library:
*               lib$signal
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*       2 byte integer values
*       RTL routines
*       System services
*       names longer than 6 characters
*       VAX/VMS system include files
*       STRUCTURE data types
*       byte data
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/88
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE SYSTEM FILES HOLDING VALUES USED BY QIOW
*
      include '($IODEF)'
      include '($TRMDEF)'

*
* DECLARE ARGUMENTS
*
      character string*(*),prompt*(*),term*(*)
      integer   lenout,lprom

*
* DECLARE LOCAL VARIABLES
*
      integer   buflen     ! Length of buffer for text read in
      integer   istat      ! Error status
      integer   litem      ! Length of itemlist in bytes
      integer   nitem      ! No. of items in extended read itemlist
      integer   ustrln     ! Function giving used length of a string
      integer   sys$assign ! Status from system service
      integer   sys$dassgn ! Status from system service
      integer   sys$qiow   ! Status from system service
      integer   tt_chan    ! Channel assigned to users terminal

*
* DECLARE STRUCTURE HOLDING ITEM DESCRIPTORS REQUIRED FOR AN EXTENDED
* (ITEMLIST) READ OPERATION BY QIOW
*
      structure /ITEM_LIST/
         integer*2 buflen,      ! Length of data buffer
     :             code         ! Item code
         integer*4 data,        ! Start address of data buffer (or
                                ! immediate data if required).
     :             retadd       ! Return address (always zero)
      end structure

      parameter (nitem=4)
      record /ITEM_LIST/ item(nitem)

*
* DECLARE STRUCTURE HOLDING IO ROUTINE STATUS BLOCK (INCLUDING INFO
* ABOUT TERMINATORS)
*
      structure /IOSTAT_BLOCK/
         integer*2  iostat,     ! Status from io routine
     :              term_offset ! Offset to start of terminator sequence
                                ! from start of read buffer (in chars)
         byte       terminator, ! First char of terminator sequence
     :              reserved,   ! Not used
     :              term_len,   ! No. of chars in terminator sequence
     :              cur_pos     ! Position of cursor at termination
      end structure

      record /IOSTAT_BLOCK/ iosb

*
* SAVE THE LENGTH OF THE TEXT BUFFER STRING
*
      buflen=len(string)

*
* SET UP THE QIOW EXTENDED READ ITEMLIST.......
* 1ST: INHIBIT THE TERMINAL DRIVERS RECALL FACILITY AND ALLOW ESCAPE
*      SEQUENCES (SUCH AS PRODUCED BY THE DOWN ARROW KEY) TO TERMINATE
*      INPUT
*
      item(1).buflen=0
      item(1).code=TRM$_MODIFIERS
      item(1).data=TRM$M_TM_NORECALL.OR.TRM$M_TM_ESCAPE
      item(1).retadd=0

*
* 2ND: INDICATE THAT A PROMPT IS REQUIRED AND SET POINTER TO PROMPT
*
      item(2).buflen=lprom
      item(2).code=TRM$_PROMPT
      item(2).data=%loc(prompt)
      item(2).retadd=0

*
* 4TH: RESERVE 10 BYTES IN BUFFER TO HOLD (POTENTIALLY) LONG ESCAPE
* SEQUENCE TERMINATORS
*
      item(3).buflen=0
      item(3).code=TRM$_ESCTRMOVR
      item(3).data=10
      item(3).retadd=0

*
* 4TH: INITIALISE READ BUFFER TO HOLD CONTENTS OF ARGUEMENT "STRING
*
      item(4).buflen=ustrln(string)
      item(4).code=TRM$_INISTRNG
      item(4).data=%loc(string)
      item(4).retadd=0

*
* LAST: STORE LENGTH OF ITEM LIST (IN BYTES)
*
      litem=12*nitem

*
* ASSIGN AN IO CHANNEL TO THE USERS TERMINAL
*
      istat=sys$assign('TT',tt_chan,,)
      if(.not.istat) then
         call lib$signal(%val(istat))
         goto 999
      endif

*
* READ IN THE DATA USING AN EXTENDED READ QIO FUNCTION
*
      istat=sys$qiow(,%val(tt_chan),
     +                   %val(IO$_READVBLK.or.IO$M_EXTEND),iosb,,,
     +                   %ref(string),%val(buflen),,,item,%val(litem))

*
* CHECK THE ERROR STATUS (INCLUDING IO STATUS IN IOSB)
*
      if(.not.istat) then
         call lib$signal(%val(istat))
         goto 999
      endif
      if(.not.iosb.iostat) then
         call lib$signal(%val(iosb.iostat))
         goto 999
      endif

*
* DEASSIGN THE IO CHANNEL
*
      istat=sys$dassgn(%val(tt_chan))
      if(.not.istat) call lib$signal(%val(istat))
*
* RETURN THE NO. OF CHARACTERS TYPED BY THE USER (EXCLUDING TERMINATOR)
*
      lenout=iosb.term_offset

*
* IF INPUT TERMINATED BECAUSE BUFFER WAS FILLED SET TERMINATOR BLANK
*
      if(iosb.term_offset.eq.buflen) then
         term=' '

*
* OTHERWISE, SAVE THE TERMINATOR SEQUENCE
*
      else
         term=string(iosb.term_offset+1:iosb.term_offset+iosb.term_len)

*
* REMOVE THE TERMINATOR FROM THE RETURNED STRING
*
         string(iosb.term_offset+1:)=' '
      endif

*
* FINISH
*
  999 continue
      end




      subroutine recall(string,pr,pw,maxrec,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Checks a string entered by the user to see if it is a RECALL
*       command. If it is, this routine processes the command and sets
*       the input string blank (RECALL commands are not stored).
*
*SOURCE
*       RECALL.FOR in UTILITIES.TLB
*
*METHOD
*       Parse the string into command, qualifier and parameter.
*       Implement REC/ALL   - gives a list of all saved commands
*                 REC fred  - returns slot starting with "fred
*                 REC 10    - Returns 10th previous string
*                 REC       - Returns last command
*       If a recall command is specified, the input string is set
*       blank on exit to avoid the string being stored in a recall
*       slot.
*
*ARGUMENTS
*   INPUTS:
*       string  character       String supplied by user
*       pr      integer         Currently displayed slot
*       pw      integer         Destination slot for storing next string
*       maxrec  integer         Max no. of slots available
*   OUTPUTS:
*       string  character       Set to blank if a RECALL command found
*       ierr    integer         Error status 0 - User to be re-prompted
*                                            1 - User input finished
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              ustrln,gtslot
*       EDRS:
*              lbgone
*       INTERIM:
*              wruser,ctoi
*       Run Time Library:
*              str$upcase,lib$get_symbol
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 30/9/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      character string*(*)
      integer   pw,pr,ierr,maxrec

*
* DECLARE LOCAL VARIABLES
*
      character buffer*255 ! Tidied up copy of the input string
      character cbuf*255   ! Buffer for translation of DCL symbols
      character com*6      ! Part of string which may contain RECALL
      integer   end        ! Useful length of tidied up input string
      integer   istat      ! Error status
      integer   lcom       ! Useful length of string in variable "com
      integer   lib$get_symbol ! Status return from RTL routine
      integer   lparm      ! Useful length of string in variable "parm
      integer   lqual      ! Useful length of string in variable "qual
      integer   minc       ! Statement function MOVE dummy argument
      integer   move       ! Statement function giving cyclic movement
                           ! round the buffer
      integer   mval       ! Statement function MOVE dummy argument
      integer   n          ! Offset from current command backwards
      character parm*40    ! Parameter from input string
      character qual*5     ! Qualifier from input string
      integer   slash      ! Position of 1st slash in the input string
      integer   slot       ! Current slot number
      integer   space      ! Position of 1st space in the input string
      integer   ustrln     ! Function giving useful length of a string
      character text*255   ! Text contained in current slot

*
* DEFINE FUNCTION WHICH RETURNS THE SLOT NUMBER WHICH IS A GIVEN
* INCREMENT AWAY FROM A STARTING SLOT. INCREMENTS CAN BE +VE OR -VE
*
      move(mval,minc)=mod(mval+minc+100*maxrec-1,maxrec)+1

*
* INITIALISE IERR TO INDICATE THAT THE USER HAS FINISHED GIVING INPUT
*
      ierr=1

*
* SEE IF THE STRING IS A DCL SYMBOL
*
      istat=lib$get_symbol(string,buffer)

*
* IF IT IS, USE THE TRANSLATION OF THE SYMBOL INSTEAD OF THE SYMBOL NAME
* OTHERWISE USE THE ORIGINAL STRING
*
      if(.not.istat) buffer=string

*
* CONVERT COPY OF INPUT STRING TO UPPER CASE, STRIP LEADING BLANKS AND
* FIND ITS USEFUL LENGTH
*
      call str$upcase(buffer,buffer)
      call lbgone(buffer)
      end=ustrln(buffer)

*
* FIND POSITION OF FIRST SPACE AND FIRST SLASH
*
      space=index(buffer,' ')
      if(space.eq.0) space=100000
      slash=index(buffer,'/')
      if(slash.eq.0) slash=100000

*
* SLASH ONLY INDICATES A QUALIFIER IF IT OCCURS BEFORE THE FIRST SPACE
*
      if(slash.gt.space) slash=100000

*
* EXTRACT THE COMMAND FROM THE STRING
*
      lcom=min(end,space-1,slash-1)
      com=buffer(:lcom)

*
* SEE IF THE COMMAND WAS AN ABBREVIATION OF RECALL, AND IF SO
* SET INPUT STRING BLANK TO INHIBIT STORAGE OF RECALL COMMAND
*
      if(lcom.gt.0.and.index('RECALL',com(:lcom)).eq.1) then
         string=' '

*
* IF IT WAS THEN EXTRACT ANY COMMAND QUALIFIER
*
         if(slash.lt.end) then
            qual=buffer(slash+1:min(end,space-1))
            call lbgone(qual)
            lqual=ustrln(qual)

*
* CHECK VALIDITY OF QUALIFIER. ONLY /ALL ALLOWED
*
            if(index('ALL',qual(:lqual)).ne.1) then
               call wruser('*** Invalid RECALL qualifier "/'
     :                     //qual(:lqual)//'"',istat)
               goto 999
            endif
         else
            qual=' '
         endif

*
* NOW EXTRACT ANY COMMAND PARAMETER
*
         if(space.lt.end) then
            parm=buffer(space+1:end)
            call lbgone(parm)
            lparm=ustrln(parm)
         else
            parm=' '
         endif

*
* IF BOTH A QUALIFIER AND A PARAMETER WERE GIVEN GIVE MESSAGE AND QUIT
*
         if(parm.ne.' '.and.qual.ne.' ') then
            call wruser('*** Too many RECALL parameters - "'
     :                  //parm(:lparm)//'"',istat)
            goto 999
         endif

*
* IF A QUALIFIER (/ALL) WAS SPECIFIED, GIVE A LIST OF ALL DEFINED
* RECALL SLOTS
*
         if(qual.ne.' ') then
            text=' '
            do n=1,maxrec-1
               write(text(:4),'(2X,I2)') n
               call gtslot(move(pw,-n),text(6:),maxrec,istat)
               if(istat.ne.0) goto 999
               call wruser(text(:ustrln(text)),istat)
            enddo

*
* IF NO QUALIFIER WAS GIVEN, PROCESS SEPERATELY THE THREE CASES WHERE
*   1) NO PARAMETER IS GIVEN
*   2) A NUMERIC PARAMETER IS GIVEN
*   3) A STRING PARAMETER IS GIVEN
*
         else

*
* IF NO PARAMETER WAS GIVEN, SET PR TO RECALL THE PREVIOUS COMMAND
*
            if(parm.eq.' ') then
               if(pr.ne.pw+1) pr=move(pr,-1)
               ierr=0

*
* IF A NUMERIC PARAMETER (=n) WAS GIVEN, RECALL THE nTH
* PREVIOUS COMMAND
*
            else
               call ctoi(parm,n,istat)
               if(istat.eq.0) then
                  if(n.gt.0.and.n.lt.maxrec) then
                     pr=move(pw,-n)
                     ierr=0
                  else
                     call wruser('*** Invalid value for numeric RECALL'
     :                           //' parameter',istat)
                  endif

*
* OTHERWISE IF A STRING PARAMETER WAS GIVEN, RECALL THE LAST SLOT WHICH
* STARTS WITH THE GIVEN STRING
*
               else
                  text=' '
                  do n=1,maxrec-1
                     call gtslot(move(pw,-n),text,maxrec,istat)
                     call str$upcase(text,text)
                     call lbgone(text)
                     if(index(text,parm(:lparm)).eq.1) then
                        pr=move(pw,-n)
                        ierr=0
                        goto 999
                     endif
                  enddo

*
* IF CONTROL GETS TO THIS POINT THEN NO MATCHING SLOT WAS FOUND
*
                  call wruser('*** RECALL string not found "'//
     :                        parm(:lparm)//'"',istat)
               endif
            endif
         endif
      endif

*
* FINISH
*
  999 continue
      end

