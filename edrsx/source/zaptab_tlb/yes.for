      function yes(prompt,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Prompts the user and returns a logical value .true. if the
*       users reply is affirmative
*
*SOURCE
*       YES.FOR in ZAPTAB.TLB
*
*
*ARGUMENTS
*   INPUTS:
*       prompt  character       The prompt string
*   OUTPUTS:
*       ierr    integer         Error status 0 - Success
*                                            1 - Too many bad values
*VAX SPECIFICS
*       enddo
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/8/87
*-------------------------------------------------------------------
*
      character*30 textin
      character*(*) prompt
      logical yes,wrong
*
* INITIALIZE ERROR STATUS, NO. OF ATTEMPTS COUNTER AND SUCCESS FLAG
*
      ierr=0
      ntry=0
      wrong=.true.
*
* LOOP UNTIL A VALID RESPONSE IS GIVEN BY USER
*
      do while(wrong)
*
* GET A TEXT STRING FROM THE USER
*
         call lib$get_input(textin,prompt,len)
*
* CONVERT TO UPPER CASE
*
         call str$upcase(textin,textin)
*
* IF REPLY IS VALID, THEN SET THE FUNCTION VALUE AND FLAG SUCCESS
*
         if(index('YES',textin(:len)).ne.0) then
            yes=.true.
            wrong=.false.
         else if(index('NO',textin(:len)).ne.0) then
            yes=.false.
            wrong=.false.
*
* IF AN INVALID REPLY IS GIVEN, BUT MAX NO OF ATTEMPTS HAS NOT YET BEEN
* MADE, TRY AGAIN.
*
         else if(ntry.lt.3) then
            ntry=ntry+1
            write(*,*)
            write(*,*) ' *** ANSWER "YES" OR "NO"'
*
* IF MAX NO OF ATTEMPTS HAS BEEN EXCEEDED, THEN PRETEND A VALID REPLY
* HAS BEEN GIVEN BUT SET ERROR STATUS TO FAILURE VALUE (1)
*
         else
            write(*,*)
            write(*,*) ' *** TOO MANY WRONG VALUES'
            ierr=1
            wrong=.false.
         endif

      enddo

      end
