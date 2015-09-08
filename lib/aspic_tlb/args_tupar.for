      subroutine args_tupar (id,uspars,nusp,status)

*+  Write user parameter names for record no 'id' (in 'uspars').
*   If 'id' is out of range then a new record is written.
*   Status returns are those from 'args_rddb' or 'args_wrdb' ie 0 for
*   success and 1 or 2 for failure.

      include 'ASPIC(dbparms)'

      integer id,nusp,status,i,start,end
      character uspars(NUSPAR)*(*),line*(RECLEN)

      call args_rddb (id,line,status)

      if (status.eq.0) then
          call itoc (nusp,line(ONUSP:ONUSP+INTLEN-1),status)
          do i = 1,min(NUSPAR,nusp)
              start = OUSP1 + (i-1)*(PARLEN + USVLEN)
              end = start + PARLEN - 1
              line(start:end) = uspars(i)
          enddo
          call args_wrdb (id,line,status)
      endif

      end
