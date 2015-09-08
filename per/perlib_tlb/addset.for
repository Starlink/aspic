      subroutine per_addset(in,nin,out,no,nfrst,dt)
 
*
*   Adds the values from a 2-D array onto the end of a large
*   1-D array.
*
*   Written by K.F.Hartley
*
      integer nin(2)
      double precision in(nin(1),nin(2)),out(no)
      double precision dt,td
      character*72 text
 
*
*   Check that data are equally spaced
*
      td=(in(1,2)-in(1,1))
      dt=(in(1,nin(2))-in(1,1))/dble(nin(2)-1)
 
      if (abs(td-dt).gt.1.0d-4) then
         call wruser('Data not equally spaced',status)
         write (text,900) dt
900      format (1h ,'Average gap is ',f10.6)
         call wruser(text,istat)
         write (text,910) td
910      format (1h ,'Gap between first two samples is ',f10.6)
         call wruser(text,istat)
         call exit
      end if
 
 
      do i=1,nin(2)
         out(nfrst+i)=in(2,i)
      end do
 
 
      end
 
 
 
