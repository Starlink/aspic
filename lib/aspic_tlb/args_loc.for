      integer function args_loc (array,n,elt)

*+  Return position of 'elt' within first 'n' elements of 'array', zero
*   if absent

      integer n
      character array(*)*(*),elt*(*)

      do args_loc = 1,n
          if (array(args_loc).eq.elt) then
              return
          endif
      enddo

      args_loc = 0

      end
