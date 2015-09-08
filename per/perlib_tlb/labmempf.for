      subroutine per_labmempf(lg,g,y,nv,nb,mq,nq,h,pf,pr,pm)
 
*
*
*      PURPOSE: Computes the Burg estimates of the prediction
*               error filter coeffs and the Akaike FPE.
*
*      INPUT:  M - NO OF DATA POINTS
*              Y - DATA ARRAY
*              LG - NOF OF PREDICTION ERROR COEFFS
*
*      OUTPUT: G - THE PREDICTION ERROR COEFFS
*              FPE - THE LOG OF THE NORMALISED FPE'S
*              PM - THE POWER
*
      implicit double precision (a-h,o-z)
      integer nv,nb,nq(nb),mq(nb)
      dimension y(nv),g(lg),h(lg),pf(nv),pr(nv)
 
*
*      Calculate sum of squares to form power
*
      num=0
      ss=0.0
 
      do j=1,nb
         i1=mq(j)+1
         i2=nq(j)
 
         do i=i1,i2
            num=num+1
            ss=ss+y(i)*y(i)
         end do
 
      end do
 
 
*
*   PM is first estimate of the power
*
      dm=ss/float(num)
      pm=dm
 
*
*      Sum over LAG
*
 
      do m=2,lg
         sn=0.0
         sd=0.0
         m2=m-2
 
*
*      Sum over data blocks
*
         k0=0
 
         do kb=1,nb
            j0=mq(kb)
            j1=nq(kb)
            k1=j1-j0+k0
 
*
 
            if(m2.eq.0) then
               k00=k0+1
 
               do k=k00,k1
                  pf(k)=0.0
                  pr(k)=0.0
               end do
 
            end if
 
 
*
            mn=j1-j0-m2-1
 
            do jj=1,mn
               j=j0+jj
               k=k0+jj
               sn=sn-2.0*(y(j+m2+1)+pf(k))*(y(j)+pr(k))
               sd=sd+(y(j+m2+1)+pf(k))**2+(y(j)+pr(k))**2
            end do
 
            k0=k1
         end do
 
 
*
         g(m)=sn/sd
 
         if(m2.ne.0) then
 
            do j=1,m2
               k=m2-j+2
               h(j+1)=g(j+1)+g(m)*g(k)
            end do
 
 
            do j=1,m2
               g(j+1)=h(j+1)
            end do
 
         end if
 
 
*
         k0=0
 
         do kb=1,nb
            j0=mq(kb)
            j1=nq(kb)
            k1=j1-j0+k0
            mn=j1-j0-m2-1
 
            if(m2.ne.0) mn=mn-1
 
            do jj=1,mn
               j=j0+jj
               k=k0+jj
               pr(k)=pr(k)+g(m)*(pf(k)+y(j+m-1))
               pf(k)=pf(k+1)+g(m)*(pr(k+1)+y(j+1))
            end do
 
            k0=k1
         end do
 
         dm=(1.0-g(m)*g(m))*dm
         pm=dm
      end do
 
      g(1)=1.0
 
      end
 
 
 
