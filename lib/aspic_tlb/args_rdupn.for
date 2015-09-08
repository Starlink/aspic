      subroutine args_rdupn (uspsz,uspars,nusp,status)

*+  Read names of user parameters for the highest numbered record in the
*   database file into 'uspars'. 'uspsz' is size of 'uspars' and 'nusp'
*   is the number of defined user parameters. (Note that 'nusp' is also
*   a standard parameter and so can be read using 'args_rdpar')
*
*   Status returns are 0 for success, 1 for "no such record", 2 for "error
*   opening database file.

      include 'ASPIC(dblnam)'

      integer uspsz,nusp,status,idmax
      character uspars(uspsz)*(*)

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.eq.0) then

*       determine number of records in file (must be > 0)
          call args_qidmx (idmax)
          if (idmax.eq.0) then
              status = 1
          else
              call args_qupar (idmax,uspsz,uspars,nusp,status)
          endif

      endif

*   don't need to close database as have only read from it

      end
