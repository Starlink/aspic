      subroutine gtfnam(name,fname,ierr)
      implicit      integer(a-z)
      character*(*) fname
 
*
      include 'interim(pctcom)'
      include 'interim(errpar)'
 
*
*
*     .....Search for entry in PCT
      ierr=1
      call stl_findpe(name,entry)
 
      if (entry.eq.0) go to 99
 
*
*     .....check for ACTIVE state
*
 
      if (pct_state(entry).eq.pct_active) then
         call stl_rdpv(pct_pvaddr(1,entry),1,fname,actval)
 
      else
         go to 99
 
      endif
 
      ierr=0
 
   99 end
 
 
 
