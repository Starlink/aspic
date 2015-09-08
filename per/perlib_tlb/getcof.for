      subroutine per_getcof(coffs,npt,g,m)
 
*
*   Stores the coefficients of the autoregressive model
*   in the form needed for computations
*
*   Written by C.D.Pike at RGO
*
      double precision coffs(npt),g(m)
 
      do j=1,m
         g(j) = -coffs(j+1)
      end do
 
 
      end
 
 
 
