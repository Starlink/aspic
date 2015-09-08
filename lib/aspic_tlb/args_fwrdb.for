      subroutine args_fwrdb (status)

*+  Force update of current database record on disk. Status returns
*   are the same as for 'args_wrdb' ie 0 for success and 1 or 2 for
*   failure.

      include 'ASPIC(dbparms)'

      integer status
      character line*(RECLEN)

      call args_wrdb (IUNDEF , line , status)

      end
