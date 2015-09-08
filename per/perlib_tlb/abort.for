      subroutine per_abort(i)
 
*
*   Universal abort-on-error routine
*
*   Written by C.D.Pike at RGO
*
      character*72 text
      write (3,900)
900   format (1h ,//,1h ,'***PROGRAM ABORTED***',//)
      call wruser('***PROGRAM ABORTED***',istat)
 
      if (i.eq.8) then
         write (3,*) 'Cannot compute a phase for this signal'
         call wruser('Cannot compute a phase for this signal',istat)
 
      else if (i.eq.9) then
         write (3,*) 'Matrix is singular'
         call wruser('Matrix is singular',istat)
 
      else if (i.eq.21) then
         write (3,*) 'Matrix is singular'
         call wruser('Matrix is singular',istat)
 
      else
         write (3,*) 'Aborting with value ',i
         write (text,910) i
910      format ('Aborting with value ',i3)
         call wruser(text,istat)
      end if
 
      call exit
 
      end
 
 
 
