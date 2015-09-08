      subroutine args_tidmx (idmax)

*+  Reset number of records in database file. Any error in 'args_wrhd'
*   will not be detected.

      include 'ASPIC(dbparms)'

      integer idmax,recpos(MAXREC),status

      call args_wrhd (recpos,idmax,status)

      end
