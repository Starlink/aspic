      subroutine args_qidmx (idmax)

*+  What is number of records in database file? Any error in 'args_rdhd'
*   returns value zero.

      include 'ASPIC(dbparms)'

      integer idmax,recpos(MAXREC),status

      call args_rdhd (recpos,idmax,status)

      end
