      subroutine per_getnf(x,n,f,nf)
 
*
*   This subroutine generates an estimate of the minimum number
*   of frequencies required to cover a specific range of frequencies
*   for a given set of epochs.
*
*   The criterion adopted is that of Dworetsky in MNRAS 203 (1983),
*   page 921
*
*   Written by K.F.Hartley at RGO on 26-4-84
*
      double precision x(n)
      real f(2)
      nf = (x(n)-x(1))*(f(2)-f(1))*4.0d0
 
      end
 
 
 
