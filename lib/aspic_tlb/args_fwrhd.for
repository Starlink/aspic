      subroutine args_fwrhd (status)

*+  Force update of database header record on disk. Status returns are
*   the same as for 'args_wrhd' ie 0 for success and 2 for failure. 

      include 'ASPIC(dbparms)'

      integer status , recpos(MAXREC) , idmax

      call args_wrhd (recpos , IUNDEF , status)

      end
