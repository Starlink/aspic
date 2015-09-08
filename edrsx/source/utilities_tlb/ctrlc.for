      subroutine ctrlc
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sets up control-C trapping, so that the flag BREAK in the
*       common block /CONTROLC/ is set true, after control-c is
*       pressed. It is up to other routines to read this flag,
*       take appropriate action, and then clear the flag.
*SOURCE
*       CTRLC.FOR in UTILITIES.TLB
*-----------------------------------------------------------------

      include '($IODEF)'
      include '($JPIDEF)'
      external chndlr
      integer sys$assign,ttchan,sys$qiow,sys$getjpi,bufad,retad,
     :        proc,pcb
      integer*2 code,blen
      data ttchan/0/
      common /ITEMLIST/ blen,code,bufad,retad,lstend

*
* ONLY SET UP CONTROL-C TRAPPING IF PROCESS IS INTERACTIVE
*
      blen=4
      code=JPI$_STS
      bufad=%loc(pcb)
      retad=0
      lstend=0
      istat=sys$getjpi(,,,blen,,,)
      if(.not.btest(pcb,14)) then

*
* ASSIGN CHANNEL TO USERS TERMINAL ON THE FIRST CALL ONLY
*
         if(ttchan.eq.0) then
            istat=sys$assign('SYS$COMMAND',ttchan,,)
            if(.not.istat) call lib$stop(%val(istat))
         endif

*
* CALL QIOW TO SET AN AST TO DELIVER CONTROL TO ROUTINE CHNDLR WHEN
* CONTROL-C IS PRESSED
*
         istat=sys$qiow(,%val(ttchan),
     :                   %val(IO$_SETMODE.or.IO$M_CTRLCAST),
     :                  ,,,chndlr,,%val(3),,,)
         if(.not.istat) call lib$stop(%val(istat))
      endif

*
* FINISH
*
      end


*
* THIS IS THE ROUTINE WHICH IS CALLED WHEN CTRL-C IS PRESSED. IT SETS
* THE BREAK FLAG FOR OTHER ROUTINES TO READ, AND CALLS CTRLC TO SET UP
* ANOTHER AST
*
      subroutine chndlr
      logical break
      common /CONTROLC/ break
      data break/.false./

      break=.true.
      call ctrlc

      end
