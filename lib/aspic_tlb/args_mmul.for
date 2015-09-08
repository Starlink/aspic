      subroutine args_mmul (m,n,p,a,b,c)

*+  Multiply matrix 'a(m,n)' by 'b(n,p)' to give 'c(m,p)'

      integer m,n,p,i,j,k
      real a(m,n),b(n,p),c(m,p),t

      do i=1,m
          do j=1,p
              t = 0.0
              do k=1,n
                  t = t + a(i,k) * b(k,j)
              enddo
              c(i,j) = t
          enddo
      enddo

      end
