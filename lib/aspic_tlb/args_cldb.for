      subroutine args_cldb (status)

*+  Close ARGS database. Status returns are those for 'args_wrdb' and
*   'args_wrhd' ie 0 for success and 1 or 2 for failure. Note that the file
*   is not physically closed , the effect is merely that of a forced update
*   to disk. Note that if the file is already closed, no error results.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbopen)'

      integer status

      if (.not.dbopen) then
          status = 0
      else

*       NB must do header last!
          call args_fwrdb (status)
          call args_fwrhd (status)

      endif

      end
