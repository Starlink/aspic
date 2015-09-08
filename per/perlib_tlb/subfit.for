      subroutine per_subfit(in,out,ax,f,c,p,nf,epoch)
 
*
*   Creates new values at the epochs of the input data and subtracts
*   the results from the input values.
*
*   Written by K.F.Hartley at RGO on 1-Mar-1984
*
      integer ax(2)
      double precision in(ax(1),ax(2)),out(ax(1),ax(2))
      double precision f(20),c(21),p(20)
      double precision epoch,delt,t
      nval = ax(2)
 
*
*   Loop through the samples
*
 
      do i=1,nval
 
*
*      Copy the epoch
*
         out(1,i) = in(1,i)
 
*
*      and the weights, if present
*
 
         if (ax(1).gt.2) out(3,i)=in(3,i)
 
*
*      Work out the time for this obsertvation, relative to the
*      zero epoch of the phases.
*
         t = (in(1,i)-epoch)*6.283185d0
 
*
*      Build up the computed value in TEMP - first the mean value
*
         temp = c(1)
 
*
*      Then the contribution from each sine wave
*
 
         do j = 1,nf
            temp = temp + c(j+1)*sin(t*f(j)+p(j))
         end do
 
 
*
*      Finally form the output value
*
         out(2,i) = in(2,i)-temp
      end do
 
 
      end
 
 
 
