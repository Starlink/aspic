      subroutine wrerr(name)
 
*++
*     WRERR - Write error
*
*     This communicates an error condition to the environment. The
*     argument is a parameter name and the precise action (error
*     messages etc...) depends on the connection file/command
*     process.
*
*     CALL WRERR(NAME)
*
*     Input argument:
*     --------------
*     NAME:    CHARACTER expression:    Parameter name
*
*
*     D.PEARCE  25/JUL/80  VERSION #2
*--
*
      implicit      integer(a-z)
 
*
      character*(*) name
 
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
         call gthelp(name,'ERROR',ierr)
 
         if(ierr.ne.0)then
            call wruser('0'//errmsg,status)
         endif
 
      endif
 
 
*
*
      return
 
      end
 
 
 
