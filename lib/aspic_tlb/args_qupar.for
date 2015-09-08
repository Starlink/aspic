      subroutine args_qupar (id,uspsz,uspars,nusp,status)

*+  Read user parameter names for record no 'id' into 'uspars' and return
*   their count in 'nusp'. 'id' must have a valid value (invalid value
*   gives 'nusp' = 0 ). Status returns are those from 'args_rddb' ie 0
*   for success and 1 or 2 for failure.

      include 'ASPIC(dbparms)'

      integer id,uspsz,nusp,status,tstat,i,start,end
      character uspars(uspsz)*(*),line*(RECLEN)

      call args_rddb (id,line,status)

      if (status.ne.0) then
          nusp = 0
      else
          call ctoi (line(ONUSP:ONUSP+INTLEN-1),nusp,tstat)
          do i = 1,min(uspsz,nusp)
              start = OUSP1 + (i-1)*(PARLEN + USVLEN)
              end = start + PARLEN - 1
              uspars(i) = line(start:end)
          enddo
      endif

      end
