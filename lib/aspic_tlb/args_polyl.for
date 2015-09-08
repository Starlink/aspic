      subroutine args_polyl (id,n,x,y,status)

*+  Draw polyline on ARGS, 'n' points '(x,y)' given in the user units
*   of image 'id'. If 'n' is 1, no harm is done (current position is
*   merely redefined.)
*
*   Status returns are those from 'args_utoa'.

      include 'ASPIC(dblbuf)'

      integer id,n,status,ax,ay,i,nb,j
      real x(n),y(n)

*   deal in batches of MLBUF points. First move to starting position.
      call args_utoa (id,x(1),y(1),ax,ay,status)
      if (status.eq.0) then

          call args_s1 ('XMA',ax)
          call args_s1 ('YMA',ay)

          do i= 2,n,MLBUF
              nb = min (n - i + 1,MLBUF)
              call args_utoan (id,nb,x(i),y(i),lbufx(1),lbufy(1),status)
*           Status should be OK, because checked above
              do j = 1,nb
                  call args_s1 ('XMA',lbufx(j))
                  call args_s1 ('YDA',lbufy(j))
              enddo
          enddo
          call srsend

      endif

      end
