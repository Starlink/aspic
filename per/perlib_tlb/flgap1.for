      subroutine per_flgap1(cof,lg,yt,yf,ytem,npt,np)
 
*
*   This fills gaps in an array, using the coefficients in
*   COF.
*
*   However in this case more messing around is needed, because
*   on exit YF contains the new values, and YT the previous
*   values in YF.
*
      double precision yt(npt),yf(npt),ytem(npt)
      double precision cof(lg)
      integer np(4)
 
*
*   First store the current values from YF
*
 
      do i=np(2)+1,np(3)-1
         ytem(i) = yf(i)
      end do
 
 
*
*
*   First do the forward extrapolation, storing the results
*   in array YF.
*
 
      do k = np(2)+1,np(3)-1
         ytemp = 0.0
 
         do i=1,lg-1
            ytemp = ytemp + yf(k-i)*(-cof(i+1))
         end do
 
         yf(k) = ytemp
      end do
 
 
*
*   Then the backwards interpolation, using array YT
*
 
      do k=np(3)-1,np(2)+1,-1
         ytemp = 0.0
 
         do i=1,lg-1
            ytemp = ytemp + yt(k+i)*(-cof(i+1))
         end do
 
         yt(k) = ytemp
      end do
 
 
*
*   Then combine the two, using triangualar weighting functions
*   and restore the original values from YTEM to YT
*
 
      do i = np(2)+1,np(3)-1
         factor = (real(i-np(2))/real(np(3)-np(2)-1))
         yf(i) = (1.0-factor)*yf(i) + factor*yt(i)
         yt(i) = ytem(i)
      end do
 
 
      end
 
 
 
