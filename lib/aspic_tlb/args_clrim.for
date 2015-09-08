      subroutine args_clrim (status)

*+  Clear all images out of the database file. What is actually done is that
*   the number of images is simply set to zero. Status returns are 0 for
*   success and 2 for "error opening database file".

      include 'ASPIC(dbparms)'
      include 'ASPIC(dblnam)'

      integer status,tstat

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.eq.0) then

          call args_tidmx (0)

      endif

*   close database file (don't report status)
      call args_cldb (tstat)

      end
