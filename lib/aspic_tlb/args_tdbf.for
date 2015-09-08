      subroutine args_tdbf (id,par,info,ninfo,status)

*+  Tell database values for parameter 'par' of record 'id'. Information
*   is held in 1st 'ninfo' elements of 'info' in floating format.
*   Status returns are those from 'args_putp', ie 0 for success, and 1
*   or 2 for failure, plus extra ones of 3 for "no room for extra user
*   parameter" and 4 for "attempt to write inappropriate number of
*   values for parameter".

      include 'ASPIC(dbparms)'

      integer id,ninfo,status,i
      real info(ninfo)
      character par*(*),cinfo(MAXPAR)*(FLTLEN)

      do i = 1,min(ninfo,MAXPAR)
          call rtoc(info(i),cinfo(i),tstat)
      enddo

      call args_tdbc (id,par,cinfo,ninfo,status)

      end
