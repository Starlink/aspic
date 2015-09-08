      subroutine per_polft(in,x,y,w,work1,out,n,m,maxd)
 
*
*      Fits a polynomial to pairs IN(1,I),IN(2,I)
*      Subtracts the best fit,
*      stores the result in OUT(1,I),OUT(2,I)
*      and passes the fitted parameters through the common block.
*
*      The maximum degree is 11
*
*      The hard work is done by several NAG routines.
*
*      Written by K.F.Hartley et al at RGO on 25-1-84
*
      double precision in(n,m),out(n,m)
      double precision x(m),y(m),w(m),work1(3,m)
      double precision work2(2,12),a(12,12),s(12)
      double precision se,temp,afit(12),xcap,ycap
      double precision xmin,xmax
      double precision sum, ave
      character*72 list
 
*
*   This COMMON block transfers the values needed to evaluate
*   the fitted polynomial to other subroutines.
*
      common /paras/afit,xmin,xmax,nplus1,ave
 
*
*   The rest of this routine is based on the assumption that
*   that a full least squares fit is to be done. However,
*   the most common case is simply xero-meaning the data.
*   This can be done much more quickly as a special case.
*
 
      if (maxd.eq.0) then
         sum=0.0d0
 
         do i=1,m
            sum = sum + in(2,i)
         end do
 
         ave = sum/dble(m)
 
         do i=1,m
            out(1,i)=in(1,i)
            out(2,i)=in(2,i)-ave
 
            if (n.gt.2) out(3,i)=in(3,i)
         end do
 
         return
      end if
 
 
*
*   First store the 2-D data in 1-D arrays.
*
 
      do i=1,m
         x(i)=in(1,i)
         y(i)=in(2,i)
 
         if (n.gt.2) then
            w(i)=in(3,i)
 
         else
            w(i)=1.0
         end if
 
      end do
 
 
*
*      E02ADF requires it's data in non-descending order
*      so sort it. (The chances are that it is in the correct
*      order so this bubble sort ought to be fast.)
*
*      Note also that the actual calculation is in terms of
*      Chebyshev polynomials ( see NAG documentation )
*
 
      do j=1,m-1
 
         do i=j,m
 
            if (x(j).gt.x(i)) then
               temp=x(j)
               x(j)=x(i)
               x(i)=temp
               temp=y(j)
               y(j)=y(i)
               y(i)=temp
               if (n.gt.2) then
                  temp=w(j)
                  w(j)=w(i)
                  w(i)=temp
               end if
            end if
 
         end do
 
      end do
 
 
*
*      Now do the fit
*
      ifail=0
      kplus1=maxd+1
      call e02adf(m,kplus1,12,x,y,w,work1,work2,a,s,ifail)
 
*
*   Check IFAIL for errors
*
 
      if (ifail.ne.0) then
         call wruser('Error in NAG routine',istat)
         return
      end if
 
 
*
*   Now evaluate the polynomial, which reqiuires normalized data.
*
 
      do j=1,12
         afit(j)=a(maxd+1,j)
      end do
 
      xmax=x(1)
      xmin=x(1)
 
      do i=1,m
 
         if (x(i).gt.xmax) xmax=x(i)
 
         if (x(i).lt.xmin) xmin=x(i)
      end do
 
 
*
*   Now evaluate the fitted polynomial at each epoch
*   subtract the result and store the differenece in OUT
*
      nplus1=maxd+1
 
      do i=1,m
         xcap=((x(i)-xmin)-(xmax-x(i)))/(xmax-xmin)
         call e02aef(nplus1,afit,xcap,ycap,ifail)
 
         if (ifail.ne.0) then
            call wruser('Error in polynomial evaluation',istat)
 
         else
            out(1,i)=in(1,i)
            out(2,i)=in(2,i)-ycap
 
            if (n.gt.2) out(3,i)=w(i)
         end if
 
      end do
 
 
      end
 
 
 
