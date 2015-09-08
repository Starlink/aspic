      subroutine args_rddb (id , line , status)

*+  Read record 'id' from ARGS database. Status returns are 0 for success,
*   1 for illegal 'id' value and 2 for "file not open".

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbbuff)'

      integer id , status , recpos(MAXREC)
      character line * (RECLEN)

*   this record may already have been read
      if (id .eq. stid .and. stid .ne. IUNDEF) then
          line = strec
          status = 0
      else

          call args_rdhd (recpos , idmax , status)

          if ( status .ne. 0 ) then
              status = 2
          else

              if (id .le. 0 .or. id .gt. idmax) then
                  line = ' '
                  status = 1
              else

*               force update to disk if record is new or has been altered
                  if (newrec) then
                      call args_fwrdb (status)
                  endif
                  read (unit = DBCHAN , rec = recpos(id) , fmt = '(a)')
     :                line
                  stid = id
                  strec = line
                  newrec = .false.
                  status = 0
              endif

          endif

      endif

      end
