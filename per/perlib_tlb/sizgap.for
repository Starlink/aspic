      subroutine per_sizgap(a,na,ma,b,nb,mb,ng)
 
*
*   This works out how many samples there are between the
*   last sample in A and the first in B
*
*   Written by K.F.Hartley at RGO on 16-3-84
*
      double precision a(na,ma),b(nb,mb)
      double precision dt
      dt = 0.5d0*( (a(1,2)-a(1,1)) + (b(1,2)-b(1,1)) )
 
*
*   Note the INT(X+0.5) to ensure round off to the nearest integer
*
      ng = int((b(1,1) - a(1,ma))/dt+0.5) -1
 
      end
 
 
 
