      subroutine subtab(tab,ntab,echo)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Reads text of unit 10, replaces tabs with spaces, and writes
*       the resulting text to unit 11. It also removes "tabs" from
*       the VAX SPECIFICS field in the header (if it exists).
*
*SOURCE
*       SUBTAB.FOR in ZAPTAB.TLB
*
*ARGUMENTS
*   INPUTS:
*       ntab            integer The number of tab positions defined
*       tab(ntab)       integer Array holding tab positions
*       echo            logical If true then user requires output
*                               file to be echoed to terminal screen
*                               Only lines which had a tab in are
*                               displayed.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               STRLEN
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       do while
*       end of line comments
*       variable format expressions
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 2/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   ntab,tab(ntab)
      logical   echo
*
* DECLARE LOCAL VARIABLES
*
      integer   i       ! Loop count
      integer   ierr    ! Error status
      integer   inchar  ! Pointer to current input character
      character intext*160      ! Buffer for input text
      logical   notabs  ! True if no tabs have been found in input
      integer   nspace  ! No. of spaces from next available output
                        ! character to next tab position
      integer   nxttab  ! Pointer to current tab
      integer   outpos  ! Pointer to next available output character
      character outtxt*160      ! Buffer for output text
      integer   strlen  ! Function giving length of string minus
                        ! trailing spaces and control characters
      logical   vaxhdr  ! True if the current comment block started
                        ! with "*VAX SPECIFICS
*
* READ IN A LINE FROM THE INPUT FILE
*
      notabs=.true.
 10   read(10,'(A)',iostat=ierr) intext
*
* IF A LINE WAS SUCCESSFULLY AQUIRED, THEN .....
*
      if(ierr.eq.0) then
*
* IF IT IS THE START OF THE "*VAX SPECIFICS" HEADER FIELD THEN SET
* VAX SPECIFIC HEADER FLAG
*
         if(index(intext,'*VAX SPECIFICS').eq.1) vaxhdr=.true.
*
* IF CURRENTLY IN THE *VAX SPECIFICS HEADER, DELETE ANY LINES THAT
* SAY "*        tabs
*
         if(vaxhdr) then
            if(index(intext,'*  tabs').eq.1) then
               if(echo) then
                  write(*,*) intext(:strlen(intext))
                  write(*,*) '  ******** LINE DELETED ********'
               endif
               goto 10
*
* IF THE END OF THE HEADER HAS BEEN REACHED, CLEAR FLAG
*
            else if(index(intext,'*').eq.0) then
               vaxhdr=.false.
            endif
         endif
*
* INITIALIZE OUTPUT STRING AND POINTERS TO OUTPUT TEXT STRING AND TABS
* ARRAY
*
         outtxt=' '
         outpos=1
         nxttab=1
*
* LOOP THROUGH ALL INPUT CHARACTERS
*
         do inchar=1,strlen(intext)
*
* IF CURRENT INPUT CHARACTER IS NOT A TAB, COPY IT TO THE OUTPUT BUFFER
*
            if(ichar(intext(inchar:inchar)).ne.9) then
               outtxt(outpos:outpos)=intext(inchar:inchar)
               outpos=outpos+1
*
* IF CURRENT INPUT CHARACTER IS A TAB, THEN CALCULATE NO. OF SPACES
* TO NEXT TAB
*
            else
               notabs=.false.
               ierr=0
               do while(tab(nxttab).le.outpos.and.ierr.eq.0)
                  nxttab=nxttab+1
                  if(nxttab.gt.ntab) ierr=1
               enddo
               if(ierr.ne.0) then
                  write(*,*) ' *** INSUFFICIENT TABS DEFINED'
                  goto 999
               endif
               nspace=tab(nxttab)-outpos
*
* INSERT CORRECT NUMBER OF SPACES INTO OUTPUT BUFFER
*
               if(nspace.ne.0) then
                  do i=1,nspace
                     outtxt(outpos:outpos)=' '
                     outpos=outpos+1
                  enddo
               endif

            endif
*
* GO ROUND FOR NEXT INPUT CHARACTER
*
         enddo
*
* WRITE COMPLETE OUTPUT LINE TO OUTPUT FILE
*
         write(11,20) outtxt
  20     format(A<outpos-1>)
*
* IF REQUESTED BY USER, ECHO ANY LINES WHICH HAD TABS REPLACED
*
         if(echo.and.nxttab.ne.1) write(6,30) outtxt
  30     format(' ',A<outpos-1>)
*
* GO ROUND FOR NEXT INPUT LINE
*
         goto 10
*
* IF NO TABS WERE FOUND IN THE INPUT FILE, GIVE A MESSAGE
*
      endif
      if(notabs) then
         write(*,*)
         write(*,*)
         write(*,*) ' *** NO TABS WERE FOUND IN THE INPUT FILE'
         write(*,*)
      endif
*
* FINISH
*
 999  continue

      end
