      subroutine args_dldb (id , status)

*+  Delete record 'id' from ARGS database. Status returns are 0 for success,
*   1 for illegal 'id' and 2 for "file not open".

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbbuff)'

      integer id , status , recpos(MAXREC)

      call args_rdhd (recpos , idmax , status)

      if (status .ne. 0 ) then
          status = 2
      else

*       'id' must be > 0 and <= 'idmax'
          if (id .le. 0 .or. id .gt. idmax) then
              status = 1
          else
              do i = id , idmax - 1
                  recpos(i) = recpos(i + 1)
              enddo
              call args_wrhd(recpos , idmax - 1 , status )
*           remember that records above the deleted one are
*           now renumbered.
              if (stid .ne. IUNDEF) then
                  if (stid .eq. id) then
                      stid = IUNDEF
                  else if (stid .gt. id) then
                      stid = stid - 1
                  endif
              endif

              status = 0
          endif

      endif

      end
