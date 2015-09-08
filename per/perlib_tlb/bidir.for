      subroutine per_bidir(y,ny,nb,nq,eps,nitr,g,lag,a,b)
 
*
*       PURPOSE: Computes the bi-directional LS solution for G.
*
*       INPUT:  LG - LAG
*               Y  - DATA
*               NQ(NB) - DATA BLOCK MARKERS
*
*       OUTPUT: G - THE LAG COEFFICIENTS
*
*   Written by L A Balona at SAAO
*   Modified slightly for this version by K.F.Hartley
*   at RGO on 23-3-84
*
      implicit double precision (a-h,o-z)
      integer ny,lg,nb,nq(nb)
      double precision g(lag),y(ny),a(lag,lag),b(lag)
      lg=lag-1
 
*
 
      do i=1,lg
 
         do j=i,lg
            n0=0
            t1=0.0
            t2=0.0
 
            do kb=1,nb
               k0=n0+1
               k1=n0+lg+1
               n0=nq(kb)
               k2=n0-lg
               k3=n0
 
               do k=k1,k3
                  t1=t1+y(k-i)*y(k-j)
               end do
 
 
               do k=k0,k2
                  t2=t2+y(k+i)*y(k+j)
               end do
 
            end do
 
            a(i,j)=t1+t2
         end do
 
         b1=0.0
         b2=0.0
         n0=0
 
         do kb=1,nb
            k0=n0+1
            k1=n0+lg+1
            n0=nq(kb)
            k2=n0-lg
            k3=n0
 
            do k=k1,k3
               b1=b1+y(k-i)*y(k)
            end do
 
 
            do k=k0,k2
               b2=b2+y(k+i)*y(k)
            end do
 
         end do
 
         b(i)=-b1-b2
      end do
 
 
*
*
 
      do i=2,lg
         j1=i-1
 
         do j=1,j1
            a(i,j)=a(j,i)
         end do
 
      end do
 
 
*
      call per_labsim2(lg,a,b,g,eps,nitr)
 
*
*
 
      do i=1,lg
         b(i+1)=g(i)
      end do
 
      b(1)=1.0
 
      end
 
 
 
