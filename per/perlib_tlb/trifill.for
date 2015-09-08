      subroutine per_trifill(y,yta,ytb,n,nb,nq,mq,m,g)
 
*
*   This subroutine fills gaps in array Y by using the AR coefficients
*   stored in G. There are NB blocks of data (and so NB-1 gaps),
*   pointed to by NQ and MQ.
*   The other arrays are for work space.
*
*   Written by K.F.Hartley at RGO on 2-3-84
*
      implicit double precision (a-h,o-z)
      double precision y(n),yta(n),ytb(n),g(m)
      integer nq(nb),mq(nb)
      character*72 text
 
*
*   First copy the good data into the two work arrays.
*
 
      do i=1,n
         yta(i)=y(i)
         ytb(i)=y(i)
      end do
 
 
*
*   We are going to compute the RMS difference between this solution
*   and the previous one, so set up counters.
*
      rms=0.0d0
      vals=0.0d0
 
*
*   The outer loop goes over the gaps
*
 
      do i=1,nb-1
 
*
*      Set up pointers to the first and last data points in this gap
*
         ifrst=nq(i)+1
         ilast=mq(i+1)
 
*
*      Now produce forwards and backwards estimates of the values
*      to fill this gap.
*
 
         do j=ifrst,ilast
 
*
*         First forwards
*
            yta(j)=0.0d0
 
            do k=1,m-1
               yta(j) = yta(j) - yta(j-k)*g(k+1)
            end do
 
 
*
*         and then backwards
*
            jrev=ilast+ifrst-j
            ytb(jrev)=0.0d0
 
            do k=1,m-1
               ytb(jrev) = ytb(jrev) - ytb(jrev+k)*g(k+1)
            end do
 
         end do
 
 
*
*      Now combine them, count the changes and store the result.
*      (Using triangular weights - decreasing from 1 as we
*      move away from "good" data)
*
 
         do j=ifrst,ilast
            fac=dble(ilast-j)/dble(ilast-ifrst)
            facm1=1.0d0-fac
            ytemp=yta(j)*fac+ytb(j)*facm1
            diff=y(j)-ytemp
            rms=rms + diff*diff
            vals=vals+1
            y(j)=ytemp
         end do
 
      end do
 
 
*
*   Now compute the rms difference
*
      rms = sqrt(rms/vals)
 
      if (rms.gt.9.99999d0) then
         write (text,900) rms
 
      else
         write (text,910) rms
      end if
 
900   format ('The RMS difference for this iteration is',e12.6)
910   format ('The RMS difference for this iteration is',f9.6)
      call wruser(text,istat)
 
      end
 
 
 
