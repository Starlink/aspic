      subroutine args_rdpar (param,vsiz,value,nval,status)

*+  Put values of parameter 'param' for the highest numbered record in the
*   database file into 'value' (character). Status returns are 0 for success, 1
*   for "no such record", 2 for "error opening database file", 3 for
*   "illegal 'param'" and 4 for "attempt to read inappropriate number of
*   values for 'param'"

      include 'ASPIC(dblnam)'

      integer vsiz,nval,status,idmax
      character param*(*),value(vsiz)*(*)

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.eq.0) then

*       determine number of records in file (must be > 0)
          call args_qidmx (idmax)
          if (idmax.eq.0) then
              status = 1
          else
              call args_qdbc (idmax,param,vsiz,value,nval,status)
          endif

        endif

*   don't need to close database as have only read from it

      end
