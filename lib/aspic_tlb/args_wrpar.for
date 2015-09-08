      subroutine args_wrpar (param,value,nval,status)

*+  Set parameter 'param' to value 'value' (character) for the highest
*   numbered record in the database file. Status returns are 0 for success, 1
*   for "no such record", 2 for "error opening database file" and 3 for
*   "illegal 'param' or no room for new user-defined one".

      include 'ASPIC(dblnam)'

      integer nval,status,idmax,tstat
      character param*(*),value(nval)*(*)

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.eq.0) then

*       determine number of records in file (must be > 0)
          call args_qidmx (idmax)
          if (idmax.eq.0) then
              status = 1
          else
              call args_tdbc (idmax,param,value,nval,status)
          endif

        endif

*   close database file (don't report status)
      call args_cldb (tstat)

      end
