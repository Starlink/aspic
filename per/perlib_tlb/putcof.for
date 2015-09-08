      subroutine per_putcof(coffs,m)
 
*
*   Stores the coefficients of the autoregressive model
*   in the form needed for computations
*
*   Written by C.D.Pike at RGO
*
      double precision coffs(m+1)
 
      do j=1,m
         coffs(j) = -coffs(j+1)
      end do
 
 
      end
 
 
 
