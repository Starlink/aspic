      logical function args_obsc (id1 , id2)

*+  Does image number 'id1' obscure image number 'id2' ? (ignoring any
*   bits off the ARGS). If so, 'args_obsc' is true. If not or if either
*   of 'id1' and 'id2' are illegal image numbers, 'args_obsc' is false.

      integer id1 , id2 , status , cx , cy , sx , sy , i , x0 , x1 ,
     :    y0 , y1
      logical args_insid
      character c*1

      args_obsc = .false.

      call args_qdb7p (id2 , c , cx , cy , sx , sy , i , i , status)
      if (status .eq. 0) then

*       corners of image 'id2'
          x0 = max ( 0 , cx - sx / 2 )
          x1 = min ( 511 , x0 + sx - 1 )
          y0 = max ( 0 , cy - sy / 2 )
          y1 = min ( 511 , y0 + sy - 1 )

*       obscured if all corners of 'id2' are inside 'id1'
          args_obsc = args_insid (id1 , x0 , y0) 
     :          .and. args_insid (id1 , x1 , y0) 
     :          .and. args_insid (id1 , x1 , y1) 
     :          .and. args_insid (id1 , x1 , y0)

      endif

      end
