      subroutine per_getlim(data,axes,st,end)
 
*
*   Extracts the first and last epoch from a data sample
*
*   Written by C.D.Pike at RGO
*
      integer axes(2)
      double precision data(axes(1),axes(2))
      double precision st,end
      st = (data(1,1))
      end = (data(1,axes(2)))

      end
