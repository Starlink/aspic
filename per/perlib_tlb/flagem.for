      subroutine per_flagem(work,n)
 
*
*   This subroutine assumes that the data in WORK have been plotted
*   on a SIMPLEPLOT graph.
*
*   It allows the cursor to be used to define some of these points,
*   finds them in the array and, when the user is happy, sets
*   their weight to zero and marks them on the plot.
*
*   Written by K.F.Hartley at RGO on 25-1-84
*
      double precision work(3,n)
      double precision x,y,dx,dy,dxm
      character*70 text
      logical yes
100   continue
      call cursor(xp,yp)
 
*
*      Convert the position to double precision
*
      x=xp
      y=yp
 
*
*      and search the array for the nearest point
*      (Firstly nearest in X, then if two poins equally near
*       the nearest in Y)
*
      dxm=abs(xp-work(1,1))
      dym=abs(yp-work(2,1))
      ibest=1
 
      do i=1,n
 
         if (work(3,i).gt.0.0) then
            dx=abs(x-work(1,i))
            dy=abs(y-work(2,i))
 
            if (dx.lt.dxm) then
               dxm=dx
               dym=dy
               ibest=i
 
            else if (dx.eq.dxm) then
 
               if (dy.lt.dym) then
                  dym=dy
                  ibest=i
               end if
 
            end if
 
         end if
 
      end do
 
 
*
*      Inform the user what has been found and ask
*      if that is satisfactory
*
      write (text,900) x,y
900   format(1h ,'Cursor values are ',2f12.4)
      call wruser(text,istat)
      xt=work(1,ibest)
      yt=work(2,ibest)
      write (text,910) xt,yt
910   format(1h ,'Stored values are ',2f12.4)
      call wruser(text,istat)
      yes=.true.
      call rdkeyl('OK',.true.,1,yes,i,istat)
      call cnpar('OK',istat)
 
      if (yes) then
 
*
*      If it was then set the corresponding weight to zero
*      and blank out the point on the screen
*
         call mark pt(xt,yt,11)
         work(3,ibest)=0.0
 
      else
         call wruser('That point will not be used',istat)
      end if
 
 
*
*      Then ask if another point is wanted
*
      call wruser('Another point?',istat)
      yes=.true.
      call rdkeyl('OK',.true.,1,yes,i,istat)
      call cnpar('OK',istat)
 
      if (yes) then
 
*
*         If it is then return to the start of this subroutine
*
         go to 100
 
      end if
 
 
*
*   Thats all so exit
*
 
      end
 
 
 
