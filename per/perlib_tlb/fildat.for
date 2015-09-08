      subroutine per_fildat(a1,n1,a2,n2,np,y,npt)
 
*
*   This subroutine takes two input datasets and puts them
*   into two longer (1-D) arrays with zeros where the gap
*   between the two sets is found.
*
      integer n1(2),n2(2)
      double precision a1(n1(1),n1(2)),a2(n2(1),n2(2))
      double precision y(npt)
      integer np(4)
 
*
*   First the first dataset
*
 
      do i=np(1),np(2)
         y(i) = a1(2,i)
      end do
 
 
*
*   Then the gap
*
 
      do i=np(2)+1,np(3)-1
         y(i)=0.0
      end do
 
 
*
*   Finally the second dataset
*
 
      do i=np(3),np(4)
         y(i)=a2(2,i+1-np(3))
      end do
 
 
      end
 
 
 
