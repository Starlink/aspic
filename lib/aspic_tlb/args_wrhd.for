      subroutine args_wrhd (recpos , idmax , status)

*+  Write header record to ARGS database. Status returns are 0 for success
*   and 2 for "file not open". NB If 'idmax' is IUNDEF , update to disk
*   is forced.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbopen)'
      include 'ASPIC(dbbuff)'

      integer recpos(MAXREC) , idmax , status

*   may need simply to update header in buffer
      if (idmax .eq. stidmx .and. idmax .ne. IUNDEF) then
          if (idmax .gt. 0) then
              call gen_copy (recpos , strpos , INTLEN * idmax)
          endif
          newpos = .true.
          status = 0
      else

          if (.not. dbopen) then
              status = 2
          else
              if (stidmx .ne. IUNDEF .and. newpos) then
                  write (unit = DBCHAN , rec = 1 , fmt = 2000) stidmx ,
     :                strpos
 2000             format (<MAXREC+1>i<INTLEN>)
              endif
              stidmx = idmax
*       'idmax' may be IUNDEF , in which case so now is 'stidmx'
              if (idmax .gt. 0) then
                  call gen_copy (recpos , strpos , INTLEN * idmax)
              endif
              newpos =.true.
              status = 0
          endif

      endif

      end
