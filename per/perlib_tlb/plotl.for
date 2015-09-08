      subroutine per_plotl(x,y,n,igf,igs,mark,off,rg)
 
*
*   Subroutine which plots array Y against array X, using
*   SIMPLELOT
*
*   Details of the plot are defined by IF,IS which define a change
*   in data quality (eg a filled gap), MARK which defines whether
*   the data are plotted as markers or line segments and OFF
*   which is an offset to the actual data.
*   RG defines the range covered by this plot, for marking the gap.
*
*   Written by K.F.Hartley at  RGOP on 13-1-84
*
      real x(n),y(n),rg(2)
      logical mark
 
      if (mark) then
 
         do i=1,n
            xp=x(i)
            yp=y(i)-off
            call mark pt(xp,yp,3)
         end do
 
 
      else
 
*
*      If we are plotting lines it is desirable NOT to join points
*      across gaps. We take the view that if the gap is more than
*      twice the average gap then it should not be plotted.
*
*      So first find the mean gap.
*
         agap=0.0
 
         do i=1,n-1
            agap = agap + abs(x(i+1)-x(i))
         end do
 
         agap = agap/real(n-1)
         xp=x(1)
         yp=y(1)-off
         call join pt(xp,yp)
 
         if (igf.eq.igs) then
 
            do i=2,n
 
               if (abs(x(i)-x(i-1)).le.2.0*agap) then
                  xp=x(i)
                  yp=y(i)-off
                  call join pt(xp,yp)
 
               else
                  call break
               end if
 
            end do
 
 
         else
 
*
*      Note that when a line plot is being drawn and there is a gap
*      then a different pen is used for inside and outside the gap.
*
 
            do i=2,igf-1
 
               if (abs(x(i)-x(i-1)).le.2.0*agap) then
                  xp=x(i)
                  yp=y(i)-off
                  call join pt(xp,yp)
 
               else
                  call break
               end if
 
            end do
 
            call pen(4)
 
            do i=igf,igs
 
               if (abs(x(i)-x(i-1)).le.2.0*agap) then
                  xp=x(i)
                  yp=y(i)-off
                  call join pt (xp,yp)
 
               else
                  call break
               end if
 
            end do
 
            call pen(1)
 
            do i=igs+1,n
 
               if (abs(x(i)-x(i-1)).le.2.0*agap) then
                  xp=x(i)
                  yp=y(i)-off
                  call join pt(xp,yp)
 
               else
                  call break
               end if
 
            end do
 
         end if
 
      end if
 
      call break
 
*
*   Now mark the gap
*   (if any)
*
 
      if (igf.le.n) then
         call pen(2)
         call join pt(x(igf),rg(1))
         call join pt(x(igf),rg(2))
         call break
         call pen(1)
      end if
 
 
      if (igs.le.n) then
         call pen(3)
         call join pt(x(igs),rg(1))
         call join pt(x(igs),rg(2))
         call break
         call pen(1)
      end if
 
 
      call gks_iacwk(1,l,iwk)
      call gks_updte(iwk)
      end
 
 
 
