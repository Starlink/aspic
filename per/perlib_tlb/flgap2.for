      subroutine per_flgap2(cof1,cof2,lg,yt,yf,npt,np)
 
*
*   This fills a gap in the array YF (defined by the array NP)
*   with values calculated using the two sets of coefficients
*   COF1 and COF2.
*
*
      double precision yt(npt),yf(npt)
      double precision cof1(lg),cof2(lg),ytemp
      integer np(4)
 
*
*   First do the forward extrapolation, storing the results
*   in array YF.
*
 
      do k = np(2)+1,np(3)-1
         ytemp = 0.0d0
 
         do i=1,lg-1
            ytemp = ytemp + yf(k-i)*(-cof1(i+1))
         end do
 
         yf(k) = ytemp
      end do
 
 
*
*   Then the backwards interpolation, using array YT
*
 
      do k=np(3)-1,np(2)+1,-1
         ytemp = 0.0d0
 
         do i=1,lg-1
            ytemp = ytemp + yt(k+i)*(-cof2(i+1))
         end do
 
         yt(k) = ytemp
      end do
 
 
*
*   Then combine the two, using triangualar weighting functions
*
 
      do i = np(2)+1,np(3)-1
         factor = (real(i-np(2))/real(np(3)-np(2)-1))
         yf(i) = (1.0-factor)*yf(i) + factor*yt(i)
         yt(i) = yf(i)
      end do
 
 
      end
 
 
 
