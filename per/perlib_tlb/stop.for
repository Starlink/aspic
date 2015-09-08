      subroutine per_stop(istat,err)
 
*
*   This a basic routine to handle FATAL errors
*
*   If the error value is NOT ERR_NORMAL then
*   the program STOPS
*
      character*(*) err
      include 'INTERIM(ERRPAR)'
 
      if (istat.ne.err_normal) then
         call wruser('*** FATAL ERROR CONDITION ***',is)
         call wrerr(err,is)
         call frdata(' ',is)
         call exit
      end if
 
 
      end
 
 
 
