      SUBROUTINE RDCHAR(BUF,PROMPT)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To read characters from the users terminal and return
*       when buffer is full without waiting for a carrage return.
*
*SOURCE
*       RDCHAR.FOR in UTILITIES.TLB
*
*METHOD
*       Cant remember the details. I wrote this routine 3 years ago
*
*ARGUMENTS
*   INPUTS:
*       prompt  character       A prompt to display before getting text
*   OUTPUTS:
*       buf     character       The buffer for text. The length of buf
*                               determines how many characters are read
*                               before returning.
*
*USED BY
*       ZAPTAB
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       system services
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/8/87
*-------------------------------------------------------------------
*
*
      include '($IODEF)'
      integer tt_chan,sys$qiow,sys$assign,sys$dassgn
      character buf*(*),prompt*(*)
*
* GET LENGTH OF BUFFERS
*
      lnbuf=len(buf)
      lenpmt=len(prompt)
*
* ASSIGN AN IO CHANNEL TO THE USERS TERMINAL
*
      istat=sys$assign('tt',tt_chan,,)
      if(.not.istat) then
         call lib$signal(%val(istat))
         goto 999
      endif
*
* READ IN THE DATA
*
      if(prompt.ne.' ') then
         istat=sys$qiow(,%val(tt_chan),%val(io$_readprompt),
     +                  ,,,%ref(buf),%val(lnbuf),,,%ref(prompt),
     +                  %val(lenpmt))
      else
         istat=sys$qiow(,%val(tt_chan),%val(io$_readvblk),,,,
     +                  %ref(buf),%val(lnbuf),,,,)
      endif
      if(.not.istat) then
         call lib$signal(%val(istat))
         goto 999
      endif
*
* DEASSIGN THE IO CHANNEL
*
      istat=sys$dassgn(%val(tt_chan))
      if(.not.istat) call lib$signal(%val(istat))
*
* FINISH
*
  999 continue

      end
