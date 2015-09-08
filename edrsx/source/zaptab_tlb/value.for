      function value(prompt,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns a valid real value by promptiny the user. If the
*       user gives a null value then the value 0 is returned and
*       the status set to -1.
*
*SOURCE
*       VALUE.FOR in ZAPTAB.TLB
*
*METHOD
*       Routine RDCHAR is used to read values from the terminal, since
*       it accepts a null value as a valid input. WRITE always reprompts
*       the user if a null value is given.
*
*ARGUMENTS
*   INPUTS:
*       prompt  character       The prompt to issue to the user
*   OUTPUTS:
*       ierr    integer         Status return 0 - Success
*                                             1 - Too many bad values
*                                            -1 - Null given
*       (function)      real    The value given by user
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               STRLEN
*       THIS PACKAGE (ZAPTAB.TLB):
*               RDCHAR
*
*VAX SPECIFICS
*       enddo
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/8/87
*-------------------------------------------------------------------
*
      logical more
      integer strlen
      character buffer*30,prompt*(*)
*
* INITIALIZE SUCCESS FLAG, STATUS VALUE AND BAD VALUES COUNTER
      more=.true.
      ierr=1
      ntry=0
*
* LOOP UNTIL A VALID VALUE IS OBTAINED FROM THE USER
*
      do while(more)
*
* CLEAR THE BUFFER AND GET A NEW STRING FROM USER
*
         buffer=' '
         call rdchar(buffer,prompt)
*
* IF FIRST CHARACTER IS NOT A CARRAGE RETURN THEN A NON-NULL STRING
* WAS GIVEN
*
         if(ichar(buffer(1:1)).ne.13) then
*
* ATTEMPT TO CONVERT IT TO A REAL NUMBER
*
            lenbuf=strlen(buffer)
            read(buffer(:lenbuf),*,iostat=ierr) value
*
* IF CONVERSION WENT OK, THEN FINISH
*
            if(ierr.eq.0) then
               more=.false.
*
* OTHERWISE GIVE MESSAGE AND GET NEW STRING UNLESS MAX NUMBER OF
* ATTEMPTS HAS BEEN EXCEEDED
*
            else
               if(ntry.lt.3) then
                  write(*,*)
                  write(*,*) ' ***BAD VALUE. TRY AGAIN'
                  ntry=ntry+1
               else
                  write(*,*)
                  write(*,*) ' *** TOO MANY BAD VALUES'
                  more=.false.
                  ierr=1
                  value=0
               endif
            endif
*
* IF A NULL STRING WAS SPECIFIED THEN RETURN WITH IERR=-1
*
         else
            ierr=-1
            more=.false.
            value=0
         endif
      enddo

      end
