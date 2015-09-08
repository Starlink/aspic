 
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*
*
*                     ********************
*                     *                  *
*                     * Program   SQORST *
*                     *                  *
*                     ********************
*
*
*
*          CALLING SEQUENCE:-
*               SQORST
*
*
*          FUNCTION:-
*               It uses bi-linear interpolation to change  the  spacing  of
*               pixels  in  an  image.  The  output  image may be larger or
*               smaller than, or of a different shape to, the original.
*
*
*          USE:-
*               It may be used to SQuash OR STretch any image to a standard
*               size,  or  to  a  scale  which  is  more  convenient. [ cf.
*               COMPAVE, COMPICK, EXPAND , PIXDUPE ]
*
*
*
*         USER PARAMETERS:-
*
*         INPUT                               The input 2-D Starlink image.
*
*         DIMS                                The dimensions of  the  image
*                                             to be created.
*
*         OUTPUT                              The name of the output image.
*
*
*         K F Hartley              RGO                            13-JAN-82
*
*
*--------------------------------------------------------------------------
*      K F Hartley   RGO   10/11/81   Version # 1
* modified by D.J.King             RGO                            30-JAN-85
      include 'INTERIM(ERRPAR)'
      include 'INTERIM(FMTPAR)'
 
*
*   Get input image and check that it is 2-d.
*
      integer axin(2),axout(2),status
      call rdimag('INPUT',fmt_r,2,axin,i,ipin,status)
 
      if (status.gt.err_normal.or.i.ne.2) then
         call wrerr('HELLIN')
         go to 800
 
      end if
 
 
*
*   Set up defaults for the dimensions of the output image.
*
      axout(1)=axin(1)
      axout(2)=axin(2)
 
*
*   Get the new dimensions.
*
100   call rdkeyi('DIMS',.true.,2,axout,i,status)
 
      if (status.gt.err_parnul) then
         call cnpar('DIMS',status)
         go to 100
 
      end if
 
 
*
*   Note that a null response causes an immediate exit.
*
 
      if (status.eq.err_parnul) then
         call wruser('INPUT IS THE SAME AS OUTPUT',status)
         call wruser('SO NO OUTPUT HAS BEEN CREATED',status)
         go to 800
 
      end if
 
 
*
*   Get an output imaage.
*
      call wrimag('OUTPUT',fmt_r,axout,2,ipout,status)
 
      if (status.ne.err_normal) then
         call wrerr('HELLOUT')
         go to 800
 
      end if
 
 
*
*   Do the actual work.
*
      call asp_sqorst (%val(ipin),axin(1),axin(2),axout(1),axout(2)
     : ,%val(ipout))
 
*
*   Tidy up and exit.
*   Branches to here if there was an error earlier.
*
800   continue
      call frdata(' ',status)
      call exit
 
      end
 
 
 
      subroutine asp_sqorst (din,ni,mi,no,mo,dout)
 
*
*      Subroutine ASP_SQORST
*
*      Applies linear interpolation to an input array to generate
*      a new array with differant dimensions.
*
*      Given
*         DIN      RA      BDF input array (2-d)
*         NI       I       First dimension of DIN
*         MI       I       Second dimension of DIN
*         NO       I       First dimension of DOUT
*         MO       I       Second dimension of DOUT
*
*      Returned
*         DOUT     RA      BDF output array (2-d)
*
*   Note the use of 0 to N-1 etc. for specifying X and Y
*
*   Written by K.F.Hartley at RGO ages ago (as part of ASPIC
*   program SQORST).
*
      real din(0:ni-1,0:mi-1)
      real dout(0:no-1,0:mo-1)
 
*
*   First compute the factors in X and Y
*   These correspond to the separation of each new pixel in units
*   of the pixel size in the old array.
*
      xfac=real(ni)/real(no)
      yfac=real(mi)/real(mo)
 
      if (xfac.gt.1.0) then
         fx=-(xfac-1.0)/2.0
 
      else
         fx=(xfac-1.0)/2.0
      endif
 
 
      if (yfac.gt.1.0) then
         fy=-(yfac-1.0)/2.0
 
      else
         fy=(yfac-1.0)/2.0
      endif
 
 
*
*   Loop through each pixel in the output array
*   finding its location (X,Y) in the input array.
*
 
      do j=0,mo-1
 
*
*      Here and in the corresponding expression for X
*      the second term ensures that the centre of the
*      first output pixel coincides with the centre
*      of the first input pixel.
*
         y=yfac*real(j) - fy
 
         do i=0,no-1
            x=xfac*real(i) - fx
 
*
*   Having found out where it is, use linear interpolation to find
*   out the VALUE at that point.
*
            call asp_linint(din,ni,mi,x,y,value)
 
*
*   And then store the value in the new array.
*
            dout(i,j)=value
         end do
 
      end do
 
 
      end
 
 
 
      subroutine asp_linint(data,n,m,x,y,value)
 
*
*      Subroutine ASP_LININT
*
*      Applies linear interpolation at a point (X,Y) in an array
*      to give a single data value.
*
*      Given
*         DATA    RA    Data array (2-d)
*         N       I     First dimension of DATA
*         M       I     Second dimension of DATA
*         X       R     X position for interpolation in range 0 to N-1
*         Y       R     Y position for interpolation in range 0 to M-1
*
*      Returned
*         VALUE   R     The (estimated) value at point (X,Y)
*                       in array DATA.
*
*      Calls no subroutines.
*
*      K F Hartley   RGO   10/11/81   Version # 1
*
*
      real data(0:n-1,0:m-1)
 
*
*   First check for really bad X and Y
*
 
      if (x.lt.0.0.or.x.ge.real(n).or.y.lt.0.0.or.y.ge.real(m)) then
         value=0.0
 
      else
 
*
*   Find the integer and fractional parts of X and Y
*
         ix=int(x)
         fx=x-real(ix)
         jy=int(y)
         fy=y-real(jy)
 
*
*   Possibly save a little time by storing the locations at the other
*   vertices of the square.
*
         ix1=ix+1
         jy1=jy+1
 
*
*   Protect against bad values for the corner points.
*
 
         if (ix1.gt.(n-1).or.jy1.gt.(m-1)) then
            value=data(ix,jy)
 
         else
 
*
*   If OK then do the interpolation.
*
            fx1=1.0-fx
            fy1=1.0-fy
            value = fx1*fy1*data(ix,jy) +fx1*fy*data(ix,jy1) +fx*fy1
     :       *data(ix1,jy) +fx*fy*data(ix1,jy1)
         end if
 
      end if
 
 
      end
 
 
 
