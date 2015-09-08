      subroutine args_tidy (id,status)

*+  Tidy all records numbered less than or equal to 'id', ie delete any
*   which are obscured by 'id'. Status returns are those from 'args_dldb'
*   ie 0 for success, 1 for "illegal record no" and 2 for "file not open".

      integer id,status,i,tid
      logical args_obsc

      tid = id
      do i = id-1,1,-1
          if (args_obsc (tid,i)) then
              call args_dldb (i,status)
*           record has been deleted, so make 'tid' point at the same record
              if (status.eq.0) then
                  tid = tid - 1
              endif
          endif
      enddo

      end
