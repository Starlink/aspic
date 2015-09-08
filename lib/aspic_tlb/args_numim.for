      subroutine args_numim (idmax)

*+  Determine how many images are displayed. Any error causes zero answer.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dblnam)'

      integer idmax,status

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.eq.0) then

          call args_qidmx (idmax)

      endif

*   don't need to close database as have only read from it

      end
