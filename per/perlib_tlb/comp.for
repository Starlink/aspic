      subroutine per_comp(yfinal,ytemp,npt,point)
 
*
*   Compares the two arrays and displays their RMS difference
*
*   Written by K.F.Hartley at RGO on 3-2-84
*
      double precision yfinal(npt),ytemp(npt)
      integer point(4)
      character*72 text
      rmsdif=0.0
 
      do i=point(2)+1,point(3)-1
         diff = abs(ytemp(i)-yfinal(i))
         rmsdif=rmsdif + diff*diff
      end do
 
      rmsdif=sqrt(rmsdif/(real(point(3)-point(2))))
      write (text,900) rmsdif
900   format (1h ,'RMS difference for this iteration is',f9.6)
      call wruser(text,status)
 
      end
 
 
 
