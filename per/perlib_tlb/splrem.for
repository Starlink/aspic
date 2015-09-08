      subroutine per_splrem(p,y,n,pa,ya,nb)
 
*
*   Removes a mean curve, specified by arrays PA and YA from
*   the data represented by P and Y
*
*   Written by K.F.Hartley at RGO on 22-2-84
*
      double precision p(n),y(n)
      double precision pa(nb),ya(nb)
 
      do i=1,n
 
*
*      Loop through the input samples
*
 
         do k=1,nb-1
 
*
*         Now look through the mean curve for the phases which bracket
*         this observation
*
 
            if (p(i).ge.pa(k).and.p(i).lt.pa(k+1)) then
               kuse = k
            end if
 
         end do
 
 
*
*      Now use linear interpolation between the YA values
*
         fac = (p(i)-pa(kuse))/(pa(kuse+1)-pa(kuse))
         y(i) = y(i) -  ( ya(kuse)*(1.0-fac) + ya(kuse+1)*fac )
      end do
 
 
      end
 
 
 
