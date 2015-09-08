      subroutine gettab(tab,ntab,maxtab,option,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To get the positions of the tabs from the user.
*
*SOURCE
*       GETTAB.FOR in ZAPTAB.TLB
*
*       First offer user the option of EDT default tabs which are seperated
*       by 8 spaces. If he doesn't want that, offer him the option of
*       specifying his own spacing and fisrt tab position and assume
*       constant spacing between all tabs. If he doesn't want to do that
*       then prompt him for each individual tab position.
*
*
*ARGUMENTS
*   INPUTS:
*       maxtab          integer         The maximum number of tabs allowed
*       option          logical         If false then user wants default
*                                       without being asked
*   OUTPUTS:
*       tab(maxtab)     integer array   The tabs positions required
*       ntab            integer         The number of tabs defined
*       ierr            integer         Status value. Zero for success.
*                                       A non-zero value is return if
*                                       too many bad values were given
*                                       (reported).
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               STRLEN
*       THIS PACKAGE (ZAPTAB.TLB):
*               YES,VALUE
*       EDRS:
*               LBGONE
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 1/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   maxtab,ntab,tab(maxtab),ierr
      logical   option
*
* DECLARE LOCAL VARIABLES
*
      logical   bad     ! True if tab position was negative or out of
                        ! order
      integer   fsttab  ! Position of first tab (1st character is
                        ! position 1 )
      logical   more    ! True if more tabs remain to be processed
      integer   nbad    ! Negative value counter
      character prompt*40! String holding prompt for individual tab
                        ! positions
      integer   strlen  ! Function returning string length minus
                        ! trailing spaces
      integer   tabspa  ! No. of spaces between tabs
      logical   true    ! Buffer for flag returned by routine YES
      real      value   ! A function returning a value obtained from
                        ! the user
      logical   yes     ! A function which returns .true if user
                        ! replies in the affirmative to the prompt given

*
* ASK USER IF DEFAULT TABBING IS OK UNLESS HE HAS NO OPTION
*
      ierr=0
      tabspa=0
      if(option) then
         true=yes(' Use EDT default tab spacing? (Y/N): ',ierr)
         write(*,*)
      else
         true=.true.
      endif
      if(ierr.ne.0) goto 999
      if(true) then
         tabspa=8
         fsttab=1
*
* IF NOT THEN SEE IF USER WANTS CONSTANT SPACED TABS
*
      else
         true=yes(' Use constant tab spacing? (Y/N): ',ierr)
         write(*,*)
         if(ierr.ne.0) goto 999
         if(true) then
            nbad=0
 10         tabspa=value(' Give number of spaces between tabs: ',
     :                   ierr)
            write(*,*)
            if(ierr.ne.0) goto 999
*
* CHECK VALUE IS NOT NEGATIVE
*
            if(tabspa.le.0) then
               write(*,*) ' *** SPACING MUST BE POSITIVE'
               nbad=nbad+1
               if(nbad.lt.3) then
                  goto 10
               else
                  write(*,*) ' *** TOO MANY BAD VALUES GIVEN'
                  ierr=1
                  goto 999
               endif
            endif
            nbad=0
 20         fsttab=value(' Give position of first tab: ',ierr)
            write(*,*)
            if(ierr.ne.0) goto 999
*
* CHECK VALUE IS NOT NEGATIVE
*
            if(fsttab.le.0) then
               write(*,*) ' *** POSITION MUST BE POSITIVE'
               nbad=nbad+1
               if(nbad.lt.3) then
                  goto 20
               else
                  write(*,*) ' *** TOO MANY BAD VALUES GIVEN'
                  ierr=1
                  goto 999
               endif
            endif
*
* OTHERWISE THE USER MUST SPECIFY EACH TAB INDIVIDUALLY
*
         else
            write(*,*)
            write(*,*) ' Give positions for the tabs.'
            write(*,*) ' Finish the list by pressing RETURN'
            write(*,*)
            more=.true.
            ntab=0
            nbad=0
*
* LOOP TO GET TABS UNTIL RETURN IS PRESSED, AN ERROR OCCURS OR
* MAX ALLOWED NO OF TABS IS REACHED
*
            do while(more)
               ntab=ntab+1
*
* GET A TAB POSITION FROM USER
*
               write(prompt,30) ntab
  30           format('     Give position of tab no. ',I3,' : ')
               call lbgone(prompt(31:))
               tab(ntab)=value(prompt(:strlen(prompt)+1),ierr)
               write(*,*)
               bad=.false.
*
* IF RETURN WAS PRESSED THEN THE LIST IS FINISHED
*
               if(ierr.eq.-1) then
                  ntab=ntab-1
                  more=.false.
                  ierr=0
*
* IF THE USER GAVE TOO MANT BAD VALUES THEN QUIT
*
               else if(ierr.ne.0) then
                  goto 999
*
* OTHERWISE A VALUE WAS GIVEN. CHECK IT'S POSITIVE
*
               else
                  if(tab(ntab).le.0) then
                     write(*,*) ' *** TAB POSITIONS MUST BE POSITIVE'
                     ntab=ntab-1
                     bad=.true.
                  endif
*
* IF NOT FIRST TAB THEN CHECK IT'S OK IN OTHER WAYS
*
                  if(ntab.gt.1) then
*
* TABS MUST BE IN ORDER OF LEFT TO RIGHT
*
                     if(tab(ntab).le.tab(ntab-1)) then
                        write(*,*) ' *** CANNOT HAVE A TAB AT OR ',
     :                             'BEFORE PREVIOUS TAB. TRY AGAIN'
                        ntab=ntab-1
                        bad=.true.
*
* EXIT LOOP IF MAX ALLOWED NO. OF TABS HAS BEEN REACHED
*
                     else
                        if(ntab.eq.maxtab) then
                           write(*,*) ' *** NO MORE TABS ALLOWED'
                           more=.false.
                        endif
                     endif
                  endif
               endif
*
* IF A GOOD VALUE WAS GIVEN, ENSURE BAD VALUE COUNTER IS ZERO
*
            if(.not.bad) then
               nbad=0
            endif
*
* IF TOO MANY BAD VALUES WERE GIVEN QUIT
*
            if(nbad.ge.3) then
               more=.false.
               ierr=1
            endif
            enddo
         endif
      endif
*
* IF CONSTANT SPACING WAS REQUESTED THEN SET UP THE MAXIMUM NUMBER OF TABS
*
      if(tabspa.ne.0) then
         do ntab=1,maxtab
            tab(ntab)=fsttab+(ntab-1)*tabspa
         enddo
         ntab=maxtab
      endif

 999  continue

      end
