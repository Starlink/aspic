      subroutine wruser(text,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Displays the given text on the users terminal in lines
*       of 80 characters each. (The INTERIM version doesn't display
*       anything if text is over 130 characters long)
*
*METHOD
*       Call interim version (renamed as iruser).
*
*ARGUMENTS
*   INPUTS:
*       text    character       Text to be displayed
*   OUTPUTS:
*       ierr    integer         Error status: 0 - Success
*
*SUBROUTINES CALLED
*       IRUSER (source at end of this file) - A copy of WRUSER from
*               the INTERIM library.
*
*VAX SPECIFICS
*       implicit none
*       do while
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/5/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character text*(*)
      integer   ierr
*
* DECLARE LOCAL VARIABLES
*
      integer   linlen  ! Length of each line
      logical   more    ! True when all text has been displayed
      integer   pos     ! Current position
      integer   rem     ! Length remaining to be displayed

      parameter (linlen=80)
*
* JUST DO IT
*
      pos=1
      rem=len(text)
      more=.true.
      do while(more)
         if(rem.le.linlen) then
            call iruser(text(pos:),ierr)
            more=.false.
         else
            call iruser(text(pos:pos+linlen-1),ierr)
            if(ierr.ne.0) more=.false.
            pos=pos+linlen
            rem=rem-linlen
         endif
      enddo
*
* FINISH
*
      end





*
*--------------------------------------------------------------------
*
* INTERIM version of WRUSER (as of 26/5/88) renamed IRUSER
*
*
      SUBROUTINE IRUSER(TEXT,STATUS)
C++
C     WRUSER - Write User output (to terminal)
C
C     This routine will write a line of output, in the form of a
C     character string, to the user's terminal. If the first
C     character in the buffer is a '$', ' ' or '0', then the
C     associated carriage control function is performed and the
C     character is not printed. Otherwise the whole line is out-
C     -put as if a ' ' has been specified. Any error encountered
C     will be signalled by an appropiate status value.
C
C     CALL WRUSER(TEXT,STATUS)
C
C     Input argument:
C     ---------------
C     TEXT:    CHARACTER expression:   Text to be written
C
C     Output argument:
C     ----------------
C     STATUS:  INTEGER variable:       Status return value
C
C
C     D.PEARCE  23/JUL/80
C--
C
      IMPLICIT      INTEGER(A-Z)
C
      CHARACTER*(*) TEXT
      INTEGER*4     STATUS
C
      CHARACTER*5   FORMAT
C
      INCLUDE 'INTERIM(ERRPAR)'
C
C
C     .....write text to terminal output stream
      FORMAT='(A)'
      IF (TEXT(1:1).NE.'$'.AND.
     +    TEXT(1:1).NE.' '.AND.
     +    TEXT(1:1).NE.'0') FORMAT='(X,A)'
      WRITE (*,FORMAT,IOSTAT=IOS) TEXT
      STATUS=ERR_NORMAL
      IF (IOS.NE.0) STATUS=ERR_USRNAV
C
      RETURN
      END
