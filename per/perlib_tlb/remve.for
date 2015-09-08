      subroutine per_remve(in,out,n,m)
 
*
*   This forms OUT from IN, subtracting a calculated fit from the
*   second column and copying the first and last
*
*   It is used to remove a fitted function from the data values
*   leaving the epochs and weights unchanged.
*
*   Written by K.F.Hartley at RGO on 25-1-84
*
      double precision in(n,m),out(n,m)
      double precision afit(12),xcap,ycap,xmin,xmax,ave
 
*
*   This COMMON block allows the parameters needed to evaluate
*   the polynomial to be passed from PER_POLFT to this
*   subroutine.
*
      common /paras/afit,xmin,xmax,nplus1,ave
 
*
*   Loop through the input samples
*
 
      do i=1,m
         out(1,i)=in(1,i)
 
*
*      The case of simple zero-meaning is special
*
 
         if (nplus1.eq.1) then
            out(2,i)=in(2,i)-ave
 
         else
 
*
*      Scale the input values in the same way as was done when
*      the fit was first performed.
*
*      Note that XMIN and XMAX refer to the original dataset,
*      not the current one.
*
            xcap=( (in(1,i)-xmin) - (xmax-in(1,i)) ) / (xmax-xmin)
            ifail=0
 
*
*      This NAG routine evaluates the polynomial at the epochs
*      corresponding to the current dataset.
*
            call e02aef(nplus1,afit,xcap,ycap,ifail)
            out(2,i)=in(2,i)-ycap
         end if
 
 
*
*      If weights were present, they are copied.
*
 
         if (n.gt.2) out(3,i)=in(3,i)
      end do
 
 
      end
 
 
 
