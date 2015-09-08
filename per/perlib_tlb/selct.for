      subroutine per_selct(in,n,x,y,m,xlim,ylim,inside,nout)
 
*
*   This selects samples whose values lie INSIDE or outside
*   a region defined by X and Y limits, by setting
*   weights of all the other points to zero
*
*   It also counts all the samples with non-zero weight
*
*   Note that for this purpose the edges of the box are
*   included as part of the interior.
*
*   Written by K.F.Hartley at RGO on 31-1-84
*
      double precision in(3,n),xlim(2),ylim(2)
      real x(m),y(m),xr(2),yr(2)
      logical inside
 
*
*   First set the REAL limits to the DOUBLE PRECISION ones
*
      xr(1)=xlim(1)
      xr(2)=xlim(2)
      yr(1)=ylim(1)
      yr(2)=ylim(2)
 
      if (inside) then
 
         do i=1,m
 
*
*         If samples inside the box are required, then set
*         the weights of all those outside to zero
*
 
            if (x(i).lt.xr(1).or.x(i).gt.xr(2).or.y(i).lt.yr(1).or.y(i)
     :       .gt.yr(2)) then
               in(3,i)=0.0
            end if
 
         end do
 
 
      else
 
         do i=1,m
 
*
*         The converse is the case if only points outside the selected
*         region are to be retained.
*
 
            if (x(i).ge.xr(1).and.x(i).le.xr(2).and.y(i).ge.yr(1).and
     :       .y(i).le.yr(2)) then
               in(3,i)=0.0
            end if
 
         end do
 
      end if
 
 
*
*   Now count the samples with non-zero weights (some may have been
*   zero for some other reason eg from some previous run of FLAG)
*
      k=0
 
      do i=1,m
 
         if (in(3,i).ne.0.0) then
            k=k+1
         end if
 
      end do
 
      nout=k
 
      end
 
 
 
