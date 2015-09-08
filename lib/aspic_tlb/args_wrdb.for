      subroutine args_wrdb (id , line , status)

*+  Write record 'id' to ARGS database ('id' = 'idmax' + 1 means write new
*   record provided there is room). Status returns are 0 for success,
*   1 for illegal 'id' and 2 for "file not open". NB If 'id' is IUNDEF
*   then update to disk is forced.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbbuff)'

      integer id , status , recpos(MAXREC)
      character line * (RECLEN)

*   may need simply to update  record in buffer
      if (id .eq. stid .and. id .ne. IUNDEF) then
          strec = line
          newrec = .true.
          status = 0
      else

          call args_rdhd (recpos , idmax , status)

          if (status .ne. 0) then
              status = 2
          else

*           cater for a possible new record
              if (id .eq. idmax + 1) then
                  if (idmax .gt. 0 .and. recpos(idmax) .gt. MAXREC) then
                      status = 1
                  else
                      if (idmax .eq. 0) then
                          recpos(id) = 2
                      else
                          recpos(id) = recpos(idmax) + 1
                      endif
                      call args_wrhd (recpos,id,status)
                      idmax = id
                  endif
              endif

              if ((id .le. 0 .or. id .gt. idmax) .and.
     :            id .ne. IUNDEF) then
                  status = 1
              else
                  if (stid .ne. IUNDEF .and. newrec) then
                      write (unit = DBCHAN , rec = recpos(stid) ,
     :                    fmt = '(a)') strec
                  endif
                  stid = id
*               'id' may be IUNDEF , in which case so now is 'stid'
                  strec = line
                  newrec = .true.
                  status = 0
              endif

          endif

      endif

      end
