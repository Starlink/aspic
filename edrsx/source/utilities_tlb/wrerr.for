      subroutine wrerr(name)
*++
*     WRERR - Write error
*
*     This communicates an error condition to the environment. The
*     argument is a parameter name and the precise action (error
*     messages etc...) depends on the connection file/command
*     process.
*
*SOURCE
*     WRERR.FOR in UTILITIES.TLB
*
*     CALL WRERR(NAME)
*
*     Input argument:
*     --------------
*     NAME:    CHARACTER expression:    Parameter name
*
*
*     D.PEARCE  25/JUL/80  VERSION #2
*     (Modified by D.S. Berry to check Log name DSCL_HELP_PROMPT before
*      issuing help information on errors)
*--
*
      implicit      integer(a-z)

*
      character*(*) name
      character buff*30
      logical nohelp
*
      character*72  errmsg

*
*     INCLUDE 'ERRPAR'
*
*
*     .....search for entry in PCT
      call stl_findpe(name,entry)

*
*     .....if entry does not exist, then create one

      if (entry.eq.0) call stl_crepe(name,entry)

*
*     .....Get Program Parameter Information from environment
      call stl_getppi(entry,.false.,1,errmsg,n,status)

*
*     .....write message to user if OK

      if (status.ne.err_normal) then
         call stl_abend('0*** BAD CONNECTION FILE ENTRY FOR '//name)

      else

         istat=lib$sys_trnlog('DSCL_HELP_PROMPT',,buff,,,)
         call lbgone(buff)
         nohelp=.true.
         if(buff(1:4).eq.'TRUE')then
            nohelp=.false.
            call gthelp(name,'ERROR',ierr)
            if(ierr.ne.0) nohelp=.true.
         endif

         if(nohelp) then
            call wruser('0'//errmsg,status)
         endif

      endif


*
*
      return

      end



