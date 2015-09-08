      subroutine per_fitpar(xarg,yarg,np,limx,ncen)
 
*
*      This routine fits a parabola to a selected region
*      of a (SIMPLEPLOT) plot of "XARG" as a function of "YARG"
*      the user is asked to select the range using the cursor
*
*      It was written by C.D.Pike at RGO
*      and modified slightly by K.F.Hartley
*
      real xarg(np),yarg(np)
      real ax(2),ay(2),xlim(2)
      real limx(2)
      real*8 fk,p,q,r,s,t,u,aa,bb,cc,scale,sum(8)
      integer np
      integer npos(2)
      character*72 text
 
*
*   In case no valid peak is defined, set up a default value
*   for NCEN
*
      ncen=np/2
 
*
*   It allows as many fits as required, always returning to here
*
1     continue
 
*
*      INITIALIZE THE SUMS
*
 
      do 100 i=1,8
         sum(i)=0.0
100   continue
 
 
*
*      GET THE WINDOW
*
 
      do 110 i=1,2
         call cursor(xlim(i),y)
 
*
*   Test that the cursor position is on the plotted part
*   of the plot, which may not cover the whole of the dataset
*
 
         if(xlim(i).le.limx(1).or.xlim(i).ge.limx(2)) then
            go to 9999
 
         end if
 
110   continue
 
 
*
*      NB NOTE THE USE OF THE "SIMPLEPLOT CURSOR"
*         NOT THE NORMAL FINGS ONE.
*
*
*      NOW SWOP THE TWO VALUES IF NEEDED
*
 
      if (xlim(1).gt.xlim(2)) then
         temp=xlim(1)
         xlim(1)=xlim(2)
         xlim(2)=temp
      end if
 
 
*
*      FIND RANGE OF SAMPLES IN THE WINDOW
*
 
      do i=1,2
         diff=abs(xlim(i)-xarg(1))
         npos(i)=1
 
         do j=1,np
            difft=abs(xlim(i)-xarg(j))
 
            if (difft.lt.diff) then
               diff=difft
               npos(i)=j
            end if
 
         end do
 
      end do
 
      n1=npos(1)
      n2=npos(2)
 
*
*   If the two samples were different then fit a parabola
*
 
      if (xlim(1).ne.xlim(2)) then
 
*
*         NOW RUN THROUGH THE WINDOW AND ACCUMULATE THE SUMS
*      FIRST SCALING THE DATA TO AVOID OVERFLOW
*
         scale=dble(xlim(1))
 
         do 200 k=n1,n2
            fk=dble(xarg(k))/scale
            sum(1)=sum(1)+fk*fk*dble(yarg(k))
            sum(2)=sum(2)+fk*dble(yarg(k))
            sum(3)=sum(3)+dble(yarg(k))
            sum(4)=sum(4)+fk**4
            sum(5)=sum(5)+fk**3
            sum(6)=sum(6)+fk**2
            sum(7)=sum(7)+fk
200      continue
 
 
*
*      CALCULATE THE COEFFICIENTS
*
         p=sum(6)*sum(4)-sum(5)**2
         q=sum(7)*sum(5)-sum(6)**2
         r=sum(7)*sum(4)-sum(6)*sum(5)
         s=dble(n2+1-n1)*sum(5)-sum(7)*sum(6)
         t=sum(2)*sum(4)-sum(1)*sum(5)
         u=sum(5)*sum(3)-sum(2)*sum(6)
 
*
*         THE PARABOLA IS OF THE FORM :-
*         Y = CC + X * BB + X**2 *AA
*
         cc=(p*u-q*t)/(p*s-q*r)
         bb=(t-r*cc)/p
         aa=(sum(1)-sum(6)*cc-sum(5)*bb)/sum(4)
 
*
*      CALCULATE THE STATIONARY POINT OF THE PARABOLA
*      OR THE "CENTRE" OF THE DIP
*
         xcen=(-bb/(2.0*aa))*scale
 
*
*      AND FINALLY PLOT OUT THE RESULTING FIT
*
         call break
         call pen(3)
 
         do 300 i=n1,n2
            x=xarg(i)
            xf=x/scale
            y=aa*xf*xf + bb*xf + cc
            call join pt (x,y)
300      continue
 
         call break
 
      else
         xcen=xlim(1)
      end if
 
      p=1.0/xcen
      write (text,900) p,xcen
900   format ('Period is ',f25.8,' Frequency ',f15.8)
      call wruser(text,istat)
 
*
*   Store approximate centre for use if plotting a window function
*
      ncen=(n1+n2)/2
      go to 1
 
9999  continue
      call pen(1)
 
      end
 
 
 
