      subroutine zaphed(field,echo)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Reads text off unit 10, deletes header fields starting with
*       "*<field>" and writes the resulting text to unit 11.
*
*SOURCE
*       ZAPHED.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       field         character A string containing the header field
*                               title eg "USED BY" deletes the field
*                               starting "*USED BY".
*       echo            logical If true then user requires lines
*                               deleted to be echoed to the terminal.
*
*USED BY
*       ZAPUSED
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               ustrln
*       EDRS:
*               LBGONE
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       do while
*       end of line comments
*       variable format expressions
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      logical   echo
      character field*(*)
*
* DECLARE LOCAL VARIABLES
*
      logical   fldstr  ! True if current line is start of header field
      integer   ierr    ! Error status
      character intext*160      ! Buffer for input text
      logical   notfnd  ! True if field was not found in input
      integer   ustrln  ! Function giving length of string minus
                        ! trailing spaces and control characters
      logical   inhdr   ! True if the current comment block started
                        ! with selected field
      integer   stlen   ! Used length of string
      character string*30 ! String containing comment defining start
                        ! of required comment block

*
*INITIALIZE THINGS
*
      string='*'//field
      call lbgone(string(2:))
      stlen=ustrln(string)
      notfnd=.true.
      inhdr=.false.
*
* READ IN A LINE FROM THE INPUT FILE
*
 10   read(10,'(A)',iostat=ierr) intext
*
* IF A LINE WAS SUCCESSFULLY AQUIRED, THEN .....
*
      if(ierr.eq.0) then
*
* IF IT IS THE START OF THE REQUIRED HEADER FIELD THEN SET
* USED BY HEADER FLAG
*
         if(index(intext,string(:stlen)).eq.1) then
            fldstr=.true.
            inhdr=.true.
         else
            fldstr=.false.
         endif
*
* IF CURRENTLY IN THE REQUIRED HEADER FIELD, DELETE ANY LINES THAT
* CONTAIN MORE THAN "*" AND WHICH HAVE A SPACE AFTER THE *
*
         if(inhdr) then
            notfnd=.false.
            if(.not.fldstr) then
               if(ustrln(intext).gt.1.and.
     :            ichar(intext(2:2)).gt.32) then
                  inhdr=.false.
                  if(echo) then
                     write(*,*) '  ******** LINES DELETED ********'
                  endif
                endif
            endif
         endif
*
* WRITE COMPLETE OUTPUT LINE TO OUTPUT FILE
*
         if(inhdr) then
            if(echo) write(*,*) intext(:ustrln(intext))
         else
            write(11,20) intext
  20        format(A<ustrln(intext)>)
         endif
*
* GO ROUND FOR NEXT INPUT LINE
*
         goto 10
      endif
*
* IF FIELD WAS NOT FOUND GIVE MESSAGE TO USER
*
      if(notfnd) then
         write(*,*)
         write(*,*)
         write(*,*) ' *** NO "',string(:stlen),'" FIELD FOUND IN THE ',
     :               'INPUT FILE'
         write(*,*)
      endif
*
* FINISH
*
 999  continue

      end
