      subroutine args_qdbf (id,par,infosz,info,ninfo,status)

*+  Query database value of parameter 'par' for record 'id'. Information
*   is returned in 1st 'ninfo' elements of 'info' in floating format.
*   Status returns are those from 'args_getp', ie 0 for success and 1
*   or 2 for failure, plus an extra one of 3 for illegal 'par'.

      include 'ASPIC(dbparms)'

      integer id,infosz,ninfo,status,i,tstat
      real info(infosz)
      character par*(*),cinfo(MAXPAR)*(FLTLEN)

      call args_qdbc (id,par,infosz,cinfo,ninfo,status)

      do i = 1,min(ninfo,MAXPAR)
          call ctor (cinfo(i),info(i),tstat)
      enddo
      do i = min(ninfo,MAXPAR)+1,infosz
          info(i) = 0.0
      enddo

      end
