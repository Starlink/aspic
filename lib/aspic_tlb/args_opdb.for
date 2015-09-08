      subroutine args_opdb (device , status)

*+  Open ARGS database file SYS_ARGS:DB<device>.DAT (<device> has
*   any non - alphanumerics removed and is then truncated to 7 chars
*   if necessary). Status returns are 0 for success and 2 for failure.
*   If the file is already open, no error occurs - this is specifically
*   permitted. In this case , the value of 'device' is of no significance
*   and the buffer flags are not set.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbopen)'
      include 'ASPIC(dbbuff)'

      integer status , lclean
      character device * ( * ) , clean * 7

      if (dbopen) then
          status = 0
      else

          call args_alnum (device , clean , lclean)
          open (unit = DBCHAN , file = 'SYS_ARGS:DB'//clean(:lclean)//
     :    '.DAT' , status = 'unknown' , carriagecontrol = 'list' ,
     :    form = 'formatted' , access = 'direct' , recl = RECLEN ,
     :    iostat=status)

*       If successful, note that the file is open, and that data buffers
*       are undefined.
          if (status .eq. 0) then
              dbopen = .true.
              stidmx = IUNDEF
              stid = IUNDEF
              newpos = .false.
              newrec = .false.
          else
              status = 2
          endif

      endif

      end
