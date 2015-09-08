      subroutine plyimg(ia,npix,nlines,invala,idx,idy,nxpar,nypar,
     :maxbin,invalb,ib,rms,w,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT AND EVALUATE A POLYNOMIAL SURFACE BY LEAST SQUARES TO
*       AN INPUT IMAGE
*
*METHOD
*       CALL BININT TO BIN THE INPUT DATA. FORM NORMAL EQUATIONS FOR
*       THE LEAST SQUARES FIT, USING CHEBYSHEV POLYNOMIALS ALONG EACH
*       AXIS AS THE BASIS FUNCTIONS. EVALUATE THE SURFACE BY
*       APPROXIMATING IT OVER EACH BIN BY A BILINEAR SURFACE WHICH
*       MATCHES IT AT EACH BIN CORNER.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       IDX,IDY (IN)
*       INTEGER
*               RECTANGULAR BIN SIZE IN THE X AND Y DIRECTIONS
*       NXPAR,NYPAR (IN)
*       INTEGER
*               NUMBER OF COEFFICIENTS IN THE X AND Y DIRECTIONS
*       MAXBIN (IN)
*       INTEGER
*               MAXIMUM NUMBER OF BINS WHICH CAN CONTAIN DATA
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       IB (OUT)
*       INTEGER*2(NPIX,NLINES)
*               OUTPUT IMAGE
*       RMS (OUT)
*       REAL
*               RMS ERROR OF FIT
*       W (WORKSPACE)
*       DOUBLE PRECISION(MAXBIN,4)
*               WORKSPACE
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               BININT
*       NAG LIBRARY:
*               E02AKF,F04ATF,E02CBF
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
* DEFINE THE MAXIMUM NUMBER OF PARAMETERS WHICH CAN BE HANDLED IN EACH
* DIRECTION
*
      parameter (mxpar=15,mxcoef=(mxpar*(mxpar+1))/2)
 
*
* DECLARE ARRAYS, ETC.
*
      integer*2 ia(npix,nlines),ib(npix,nlines)
      double precision x(4),y(4),f(4),fsumsq,w(maxbin,4),xmin,xmax,ymin
     : ,ymax,px(mxpar),py(mxpar),pxy(mxcoef),cc(mxpar),a(mxcoef,mxcoef)
     :  ,b(mxcoef),ans(mxcoef),aa(mxcoef,mxcoef),wks1(mxcoef),
     :  wks2(mxcoef)
 
*
* RESET ERROR FLAG AND CALL BININT TO BIN THE DATA INTO SQUARE
* OR RECTANGULAR AREAS
*
      ierr=0
      call binint(ia,npix,nlines,invala,idx,idy,w(1,1),w(1,2),w(1,3)
     : ,w(1,4),nbin)
 
*
* CALCULATE THE NUMBER OF FREE FITTING PARAMETERS AND QUIT IF THERE
* ARE INSUFFICIENT DATA
*
      ncoef=(min(nxpar,nypar)*(min(nxpar,nypar)+1))/2+abs(nxpar-nypar)
 
      if(nbin.lt.ncoef)then
         ierr=3
         go to 99
 
      endif
 
 
*
* SET UP IMAGE BOUNDARIES
*
      xmin=0.5d0
      xmax=npix+0.5d0
      ymin=0.5d0
      ymax=nlines+0.5d0
 
*
* CONVERT DATA WEIGHTS TO REPRESENT NUMBER OF PIXELS IN BIN
*
 
      do 404 i=1,nbin
         w(i,4)=w(i,4)**2
404   continue
 
 
*
* INITIALLISE ARRAYS TO HOLD COEFFICIENTS OF NORMAL EQUATIONS
*
 
      do 1 j=1,ncoef
         b(j)=0.0d0
 
         do 2 i=1,ncoef
            a(i,j)=0.0d0
2        continue
 
1     continue
 
 
*
* FORM SUMS FOR NORMAL EQUATIONS, SUMMING OVER ALL DATA POINTS
*
 
      do 333 ibin=1,nbin
 
*
* INITIALLISE COEFFICIENTS FOR EVALUATING CHEBYSHEV POLYNOMIALS
*
 
         do 3 i=1,max(nxpar,nypar)
            cc(i)=0.0d0
3        continue
 
 
*
* EVALUATE CHEBYSHEV POLYNOMIALS OF THE ORDERS REQUIRED ALONG
* EACH AXIS AT THE DATA POSITION
*
 
         do 4 i=1,max(nxpar,nypar)
            cc(i)=1.0d0
 
            if(i.le.nxpar)then
               ifail=1
               call e02akf(i,xmin,xmax,cc,1,mxpar,w(ibin,1),px(i),
     :         ifail)
            endif
 
 
            if(i.le.nypar)then
               ifail=1
               call e02akf(i,ymin,ymax,cc,1,mxpar,w(ibin,2),py(i),
     :         ifail)
            endif
 
            cc(i)=0.0d0
4        continue
 
 
*
* NOW FORM THE REQUIRED PRODUCTS OF THE X AND Y POLYNOMIALS
*
         nc=0
 
         do 6 j=1,nypar
 
            do 5 i=1,nxpar
 
*
* OMIT THOSE PRODUCTS WHICH ARE OF TOO HIGH AN ORDER IN X OR Y
*
 
               if(i.gt.1.and.i+j-1.gt.nxpar)go to 5
 
               if(j.gt.1.and.i+j-1.gt.nypar)go to 5
 
*
* STORE THE REQUIRED PRODUCTS IN PXY
*
               nc=nc+1
               pxy(nc)=px(i)*py(j)
5           continue
 
6        continue
 
 
*
* ADD TO THE NORMAL EQUATION SUMS (A TERM FOR EACH PRODUCT)
*
 
         do 414 j=1,nc
            b(j)=b(j)+pxy(j)*w(ibin,3)*w(ibin,4)
 
            do 413 i=1,nc
               a(i,j)=a(i,j)+pxy(i)*pxy(j)*w(ibin,4)
413         continue
 
414      continue
 
333   continue
 
 
*
* CALL F04ATF TO SOLVE THE NORMAL EQUATIONS
*
      ifail=1
      call f04atf(a,mxcoef,b,nc,ans,aa,mxcoef,wks1,wks2,ifail)
 
      if(ifail.ne.0)then
         ierr=2
         go to 99
 
      endif
 
 
*
* PUT THE SOLUTION COEFFICIENTS INTO AN ARRAY WITH ZERO ELEMENTS
* FOR THE TERMS NOT REQUIRED
*
      nc=0
 
      do 14 j=1,nypar
 
         do 15 i=1,nxpar
            pxy((i-1)*nypar+j)=0.0d0
 
*
* LEAVE ZEROS IN IF THE TERM IS OF TOO HIGH AN ORDER IN X OR Y
*
 
            if(i.gt.1.and.i+j-1.gt.nxpar)go to 15
 
            if(j.gt.1.and.i+j-1.gt.nypar)go to 15
 
*
* OTHERWISE PUT THE APPROPRIATE COEFFICIENT IN
*
            nc=nc+1
            pxy((i-1)*nypar+j)=ans(nc)
15       continue
 
14    continue
 
 
*
* NOW EVALUATE THE FITTED SURFACE
* -------------------------------
*
* INITIALLISE SUMS TO FORM THE RMS ERROR OF FIT
*
      sumsq=0.0
      npt=0
 
*
* SCAN THROUGH THE BINS, CALCULATING THE MIN AND MAX X AND Y
* COORDINATES FOR EACH BIN
*
 
      do 66 jmin=1,nlines,idy
         jmax=min(jmin+idy-1,nlines)
 
         do 65 imin=1,npix,idx
            imax=min(imin+idx-1,npix)
 
*
* FIND THE X,Y COORDINATES OF THE FOUR BIN CORNERS
*
            x(1)=imin-0.5d0
            y(1)=jmin-0.5d0
            x(2)=imax+0.5d0
            y(2)=y(1)
            x(3)=x(2)
            y(3)=jmax+0.5d0
            x(4)=x(1)
            y(4)=y(3)
 
*
* EVALUATE THE FITTED SURFACE AT THE 4 BIN CORNERS
*
 
            do 303 i=1,4
               ifail=1
               call e02cbf(1,1,nxpar-1,nypar-1,x(i),xmin,xmax,y(i),ymin
     :          ,ymax,f(i),pxy,mxcoef,px,mxpar,ifail)
 
               if(ifail.ne.0) then
                  ierr=2
                  go to 99
 
               endif
 
303         continue
 
 
*
* CALCULATE THE COEFFICIENTS OF A BI-LINEAR SURFACE WHICH PASSES
* THROUGH THE FOUR CORNERS
*
            za=f(1)
            zb=(f(2)-za)/idx
            zc=(f(4)-za)/idy
            zd=(f(3)-za-zb*idx-zc*idy)/(idx*idy)
 
*
* NOW SCAN ALL PIXELS IN THE BIN, EVALUATING THE BI-LINEAR SURFACE
*
            dy=-0.5
 
            do 64 j=jmin,jmax
               dy=dy+1.0
               fa=za+zc*dy
               fb=zb+zd*dy
               dx=-0.5
 
               do 63 i=imin,imax
                  dx=dx+1.0
                  fun=fa+fb*dx
 
*
* IF RESULT IS OUTSIDE THE RANGE OF INTEGERS, SET OUTPUT PIXEL
* INVALID
*
 
                  if(abs(fun).gt.32767.0) then
                     ib(i,j)=invalb
 
                  else
                     ib(i,j)=nint(fun)
 
*
* FORM SUMS FOR THE RMS ERROR OF FIT
*
 
                     if(ia(i,j).ne.invala) then
                        sumsq=sumsq+(fun-ia(i,j))**2
                        npt=npt+1
                     endif
 
                  endif
 
63             continue
 
64          continue
 
65       continue
 
66    continue
 
 
*
* CALCULATE RMS ERROR OF FIT
*
 
      if(npt.ge.1) then
         rms=sqrt(sumsq/npt)
 
      else
         rms=0.0
      endif
 
 
   99 end
 
 
 
