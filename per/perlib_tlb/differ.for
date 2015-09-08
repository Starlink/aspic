      subroutine per_differ(a,da,b,db,x,y)
 
*
*   This seeks to store the differences between the corresponding
*   elements in A and B
*
*   Written by K.F.Hartley at RGO on 13-1-84
*
      integer da(2),db(2)
      double precision a(da(1),da(2)),b(db(1),db(2))
      real x(db(2)),y(db(2))
 
*
*   First try to decide whether the two arrays are at the same epochs.
*
 
      if (db(2).ne.da(2)) then
         call wruser('Arrays are of different length',istat)
         db(2)=0
         return
      end if
 
 
*
*   The first two epochs must agree to 10% of interval between samples
*
      deltat = abs(a(1,1)-b(1,1))/abs(a(2,2)-a(1,2))
 
      if (deltat.gt.0.1) then
         call wruser('The arrays start at different  epochs',istat)
         db(2)=0
         return
      end if
 
 
*
*   Same for end epoch
*
      deltat = abs(a(1,da(2))-b(1,db(2))) / abs(a(1,db(2))-a(1,db(2)
     : -1))
 
      if (deltat.gt.0.1) then
         call wruser('The arrays end at different epochs',istat)
         db(2)=0
         return
      end if
 
 
*
*   With these checks there is a fair chance that the two arrays
*   correspond to the same data.
*
 
      do i=1,db(2)
         x(i)=a(1,i)
         y(i)=a(2,i)-b(2,i)
      end do
 
 
      end
 
 
 
