      subroutine args_rdhd (recpos , idmax , status)

*+  Read header record from ARGS database. Status returns are 0 for success
*   and 2 for "file not open"

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbopen)'
      include 'ASPIC(dbbuff)'

      integer recpos(MAXREC) , idmax , i , status

*   header may already have been read
      if (stidmx .ne. IUNDEF) then
          idmax = stidmx
          if (stidmx .gt. 0) then
              call gen_copy (strpos , recpos , INTLEN * stidmx)
          endif
          status = 0
      else

          if (.not. dbopen) then
              idmax = 0
              status = 2
          else

*           force update to disk if header record has been altered
              if (newpos) then
                  call args_fwrhd (status)
              endif
              read (unit = DBCHAN , rec = 1 , fmt = 1000) idmax , recpos
 1000         format (<MAXREC+1>i<INTLEN>)
              stidmx = idmax
              if (idmax .gt. 0) then
                  call gen_copy (recpos , strpos , INTLEN * idmax)
              endif
              newpos = .false.
              status = 0
          endif

      endif

      end
