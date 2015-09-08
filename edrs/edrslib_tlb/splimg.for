      subroutine splimg(ia,npix,nlines,invala,idx,idy,nxknot,nyknot
     : ,maxbin,invalb,ib,rms,work,point,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO GENERATE AN IMAGE WHICH IS A LEAST-SQUARES BI-CUBIC SPLINE
*       FIT TO AN INPUT IMAGE
*
*METHOD
*       CALL BININT TO BIN THE INPUT DATA INTO RECTANGULAR BINS. SORT
*       THE BIN COORDINATES IN THE X AND Y DIRECTION AND CALL SETKNT
*       TO PLACE THE INTERIOR SPLINE KNOTS. ADD DUMMY DATA POINTS AT
*       THE IMAGE CORNERS TO FORCE THE FIT TO EXTEND TO THE IMAGE EDGES.
*       CALL NAG ROUTINE E02ZAF TO SORT THE DATA POINTS, THEN CALL
*       E02DAF TO FIT THE SPLINE SURFACE. EVALUATE THE FITTED SURFACE
*       AT THE FOUR CORNERS OF EACH BIN, AND APPROXIMATE IT BY A
*       BI-LINEAR SURFACE. EVALUATE THE BI-LINEAR SURFACES OVER THE
*       OUTPUT IMAGE AREA
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
*       NXKNOT,NYKNOT (IN)
*       INTEGER
*               NUMBER OF INTERIOR KNOTS IN THE X AND Y DIRECTIONS
*       MAXBIN (IN)
*       INTEGER
*               MAXIMUM NUMBER OF BINS WHICH CAN CONTAIN DATA +2
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       IB (OUT)
*       INTEGER*2(NPIX,NLINES)
*               OUTPUT IMAGE
*       RMS (OUT)
*       REAL
*               RMS ERROR OF FIT
*       WORK (WORKSPACE)
*       DOUBLE PRECISION(MAXBIN,5)
*               INTERMEDIATE STORAGE
*       POINT (WORKSPACE)
*       INTEGER(MAXBIN+(NXKNOT+1)*(NYKNOT+1))
*               INTERMEDIATE STORAGE
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               BININT,SETKNT
*       NAG LIBRARY:
*               M01ANF,E02ZAF,E02DAF,E02DBF
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
*
* SET MAXIMUM NUMBER OF INTERIOR KNOTS AND THE CORRESPONDING
* MAXIMUM DIMENSIONS OF WORK ARRAYS (N.B. WORKSPACE INCREASES
* RAPIDLY WITH NO. OF KNOTS)
*
      parameter (mxknot=11,ncmax=(mxknot+4)**2,np=3*(mxknot+4)+4,nws=2
     : *ncmax*(np+2)+np,mxadrs=(mxknot+1)**2   )
 
*
* DIMENSION ARRAYS
*
      integer*2 ia(npix,nlines),ib(npix,nlines)
      integer adres(mxadrs),point(maxbin+(nxknot+1)*(nyknot+1))
      double precision xknot(mxknot+8),yknot(mxknot+8),dl(ncmax),
     :coeff(ncmax),ws(nws),work(maxbin,5),sigma,x(4),y(4),f(4)
 
*
* CALCULATE THE TOTAL NO. OF SPLINE COEFFICIENTS AND THE
* NUMBER OF PANELS PRODUCED BY THE KNOTS
* (SEE NAG DOCUMENTATION FOR E02DAF,E02ZAF FOR DETAILS)
*
      nc=(nxknot+4)*(nyknot+4)
      nadres=(nxknot+1)*(nyknot+1)
 
*
* CALL BININT TO BIN THE IMAGE INTO RECTANGULAR BINS, RETURNING
* THE X,Y POSITIONS, THE MEAN VALUE AND A WEIGHT FOR EACH BIN
* IN WORK(*,N),N=1,4
*
      call binint(ia,npix,nlines,invala,idx,idy,work(1,1),work(1,2)
     : ,work(1,3),work(1,4),nbin)
 
*
* IF INSUFFICIENT BINS CONTAINED VALID DATA, SET IERR=3 AND QUIT
*
 
      if(nbin.le.1) then
         ierr=3
         go to 99
 
      endif
 
 
*
* COPY THE X POSITIONS INTO SPARE WORKSPACE AND SORT INTO ORDER
* USING NAG ROUTINE M01ANF
*
 
      do 1 i=1,nbin
         work(i,5)=work(i,1)
1     continue
 
      ifail=1
      call m01anf(work(1,5),1,nbin,ifail)
 
*
* CALL SETKNT TO SET THE INTERIOR X KNOTS AN EQUAL NUMBER OF
* DATA POINTS APART IN THE X DIRECTION
*
 
      if(nxknot.ge.1)call setknt(work(1,5),nbin,xknot(5),nxknot)
 
*
* NOW REPEAT THE ABOVE FOR THE INTERIOR Y KNOTS
*
 
      do 2 i=1,nbin
         work(i,5)=work(i,2)
2     continue
 
      ifail=1
      call m01anf(work(1,5),1,nbin,ifail)
 
      if(nyknot.ge.1)call setknt(work(1,5),nbin,yknot(5),nyknot)
 
*
* SCALE DATA VALUES TO LIE IN THE RANGE -1 TO +1 TO IMPROVE
* PERFORMANCE OF THE FITTING ROUTINES
*
 
      do 601 i=1,nbin
         work(i,3)=work(i,3)*3.051850947d-5
601   continue
 
 
*
* ADD 2 MORE BINS, WITH ZERO WEIGHT, AT OPPOSITE CORNERS OF
* THE IMAGE, TO ENSURE THAT THE SPLINE FIT IS VALID OVER
* THE ENTIRE IMAGE AREA
*
      work(nbin+1,1)=0.0d0
      work(nbin+1,2)=0.0d0
      work(nbin+1,3)=0.0d0
      work(nbin+1,4)=0.0d0
      work(nbin+2,1)=npix+1.0d0
      work(nbin+2,2)=nlines+1.0d0
      work(nbin+2,3)=0.0d0
      work(nbin+2,4)=0.0d0
      nbin=nbin+2
 
*
* CALCULATE THE NUMBER OF POINTERS REQUIRED TO ACCESS THE DATA
* POINTS IN PANEL ORDER
*
      npoint=nbin+nadres
 
*
* CALL NAG ROUTINE E02ZAF TO SORT DATA POINTS INTO PANEL ORDER
*
      ifail=1
      call e02zaf(nxknot+8,nyknot+8,xknot,yknot,nbin,work(1,1),work(1
     : ,2),point,npoint,adres,nadres,ifail)
 
*
* CALL NAG ROUTINE E02DAF TO OBTAIN THE SPLINE COEFFICIENTS OF
* THE LEAST SQUARES FIT
*
      ifail=1
      call e02daf(nbin,nxknot+8,nyknot+8,work(1,1),work(1,2),work(1
     : ,3),work(1,4),xknot,yknot,point,npoint,dl,coeff,nc,ws,nws,1.0e
     :  -9,sigma,irank,ifail)
 
      if(ifail.ne.0) then
         ierr=2
         go to 99
 
      endif
 
 
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
* COORDINATES OF EACH BIN
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
* CALL NAG ROUTINE E02ZAF TO SORT THE CORNERS INTO PANEL ORDER
*
            ifail=1
            npoint=4+nadres
            call e02zaf(nxknot+8,nyknot+8,xknot,yknot,4,x,y,point,
     :      npoint,adres,nadres,ifail)
 
*
* CALL NAG ROUTINE E02DBF TO EVALUATE THE FITTED SURFACE AT THE
* 4 BIN CORNERS
*
            ifail=1
            call e02dbf(4,nxknot+8,nyknot+8,x,y,f,xknot,yknot,point
     :       ,npoint,coeff,nc,ifail)
 
            if(ifail.ne.0) then
               ierr=2
               go to 99
 
            endif
 
 
*
* CONVERT SCALE BACK TO THE RANGE -32767 TO +32767
*
 
            do 666 l=1,4
               f(l)=f(l)*32767.0d0
666         continue
 
 
*
* CALCULATE THE COEFFICIENTS OF A BI-LINEAR SURFACE WHICH PASSES
* THROUGH THE FOUR CORNERS
*
            a=f(1)
            b=(f(2)-a)/idx
            c=(f(4)-a)/idy
            d=(f(3)-a-b*idx-c*idy)/(idx*idy)
 
*
* NOW SCAN ALL PIXELS IN THE BIN, EVALUATING THE BI-LINEAR SURFACE
*
            dy=-0.5
 
            do 64 j=jmin,jmax
               dy=dy+1.0
               fa=a+c*dy
               fb=b+d*dy
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
 
 
*
* SER IERR TO INDICATE IF THE FIT WAS NOT UNIQUE
*
      ierr=0
 
      if(irank.ne.nc) ierr=1
99    return
 
      end
 
 
 
