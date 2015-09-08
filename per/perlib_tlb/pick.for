      subroutine per_pick(out,axes,in,oaxes,lims)
 
*
*   Copies the samples with epochs between the LIMS
*   and which have non-zero weight.
*
*   Written by K.F.Hartley at RGO on 1-2-84
*
      integer axes(2),oaxes(2)
      double precision out(axes(1),axes(2)),lims(2)
      double precision in(oaxes(1),oaxes(2))
      ic = 0
 
      do i=1,oaxes(2)
 
         if(in(1,i).ge.lims(1).and.in(1,i).le.lims(2))  then
 
*
*         Check to see if the input had weights.
*
 
            if (axes(1).eq.3) then
 
*
*         If it has then store only thsoe with non-zero weight
*
 
               if (in(3,i).ne.0.0) then
                  ic=ic+1
                  out(1,ic)=in(1,i)
                  out(2,ic)=in(2,i)
                  out(3,ic)=in(3,i)
               end if
 
 
            else
 
*
*         Otherwise store it, and give it a weight of 1.0
*
               ic=ic+1
               out(1,ic)=in(1,i)
               out(2,ic)=in(2,i)
               out(3,ic)=1.0
            end if
 
         end if
 
      end do
 
 
      end
 
 
 
