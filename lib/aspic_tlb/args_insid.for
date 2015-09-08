      logical function args_insid (id , x , y)

*+  Is '(x , y)' in image number 'id' ? If it is, 'args_insid' is true.
*   If not or 'id' is an illegal image number, 'args_insid' is false.

      integer id , x , y , status , cx , cy , sx , sy , i , x0 ,
     :    x1 , y0 , y1
      character c*1

      args_insid = .false.

      call args_qdb7p (id , c , cx , cy , sx , sy , i , i , status)
      if (status .eq. 0) then

          x0 = cx - sx / 2
          x1 = x0 + sx - 1
          y0 = cy - sy / 2
          y1 = y0 + sy - 1

          args_insid = x0 .le. x .and.  x .le. x1 .and. y0 .le. y
     :        .and.  y .le. y1

      endif

      end
