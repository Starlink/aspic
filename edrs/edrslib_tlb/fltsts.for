      subroutine fltsts(wmin,wmax,idev,icon,totint,tphot,wmean,wpeak
     : ,tpeak,bwidth)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PLOT A FILTER RESPONSE AS PRODUCED BY FLTFUN, AND TO
*       CALCULATE IMPORTANT STATISTICS FOR THE RESPONSE
*
*METHOD
*       THE RESPONSE PROVIDED BY FLTFUN IS PLOTTED OVER THE WAVELENGTH
*       RANGE WMIN TO WMAX. NUMERICAL QUADRATURE IS THEN USED TO OBTAIN
*       THE INTEGRATED INTENSITY, TOTAL NUMBER OF PHOTONS AND
*       MEAN WAVELENGTH OF THE SPECTRUM. THE PEAK VALUE IS FOUND AND
*       THE EFFECTIVE RECTANGULAR BANDWIDTH CALCULATED. FINALLY THE
*       EQUIVALENT RECTANGULAR RESPONSE IS PLOTTED OVER THE ACTUAL
*       RESPONSE.
*
*ARGUMENTS
*       WMIN,WMAX (IN)
*       REAL
*               THE WAVELENGTH RANGE TO BE CONSIDERED
*       IDEV (IN)
*       INTEGER
*               GKS DEVICE NUMBER FOR PLOTTING (0 IF NO PLOTTING
*               REQUIRED). THIS ROUTINE PERFORMS OPENING AND
*               CLOSING OF GKS VIA THE HIGR PACKAGE.
*       ICON (IN)
*       INTEGER
*               FORTRAN STREAM NUMBER FOR QUEUEING PLOTTED OUTPUT
*       TOTINT (OUT)
*       REAL
*               INTEGRATED INTENSITY IN RESPONSE
*       TPHOT (OUT)
*       REAL
*               NUMBER OF PHOTONS PER SECOND IN WHOLE OF SPECTRUM
*       WMEAN (OUT)
*       REAL
*               MEAN WAVELENGTH
*       WPEAK (OUT)
*       REAL
*               PEAK WAVELENGTH
*       TPEAK (OUT)
*       REAL
*               PEAK RESPONSE VALUE
*       BWIDTH (OUT)
*       REAL
*               FULL WIDTH OF EQUIVALENT RECTANGULAR SPECTRUM
*
*CALLS
*       HIGR_:
*               GZBGN,DRPLOT,DRDASH,GZEND
*       NAG LIBRARY:
*               D01AHF,E04ABF
*
*NOTES
*       USES COMMON BLOCK /FLTBL2/ TO CONTROL THE ACTION OF FLTFUN
*       (AND HENCE FLTFN2)
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
* INCLUDE COMMON BLOCK TO PERMIT CONTROL OF THE ROUTINE FLTFUN
*
      include 'edrslib(filt2com.inc)'
 
*
* DECLARATIONS, ETC.
*
      double precision fltfun,w,d01ahf,relerr,dwpeak,dtpeak,e1,e2
      parameter (nplot=100)
      real x(nplot),y(nplot)
      external fltfun,fltfn2
 
*
* IF REQUIRED GENERATE PLOT OF THE COMBINED FILTER RESPONSE
*
 
      if(.true.)then
         iwgt=0
 
         do 1 i=1,nplot
            w=wmin+((wmax-wmin)*(i-1))/(nplot-1)
            y(i)=fltfun(w)
            x(i)=w
1        continue
 
 
*
* PLOT GRAPH
*
         call higr_gzbgn(4,icon,idev,2)
         call higr_drplot(x,y,nplot,'Wavelength (nm)#','Transmitted po'/
     :    / 'wer (W/nm)#','Spectral energy density#','#')
      endif
 
 
*
* FIND THE TOTAL TRANSMITTED INTENSITY
*
      totint=0.0
      tphot=0.0
      wmean=0.0
      wpeak=0.0
      tpeak=0.0
      bwidth=0.0
      iwgt=0
      ifail=1
      totint=d01ahf(dble(wmin),dble(wmax),0.0001d0,np,relerr,fltfun
     : ,10000,ifail)
 
      if(ifail.ne.0)then
         totint=0.0
         go to 88
 
      endif
 
 
*
* REPEAT, WEIGHTING THE INTENSITY WITH THE WAVELENGTH AND HENCE FIND
* THE MEAN WAVELENGTH
*
      iwgt=1
      ifail=1
      t1=d01ahf(dble(wmin),dble(wmax),0.0001d0,np,relerr,fltfun,10000
     : ,ifail)
 
      if(ifail.ne.0)then
         go to 88
 
      endif
 
      wmean=t1/max(totint,1.0e-30)
 
*
* CALCULATE THE EMERGENT PHOTON FLUX
*
      tphot=t1*5.03403e15
 
*
* SEARCH FOR THE WAVELENGTH OF MAXIMUM INTENSITY
*
      iwgt=0
      ifail=1
      maxcal=1000
      e1=0.00001d0
      e2=0.0d0
      call e04abf(fltfn2,e1,e2,dble(wmin),dble(wmax),maxcal,dwpeak,
     :dtpeak,ifail)
 
      if(ifail.ne.0)then
         go to 88
 
      endif
 
      wpeak=dwpeak
      tpeak=-dtpeak
 
*
* CALCULATE THE EFFECTIVE BANDWIDTH
*
      bwidth=totint/tpeak
 
*
* IF REQUIRED, DISPLAY RESULTS ON GRAPH
*
 
88    if(idev.ne.0)then
         x(1)=wmean-0.5*bwidth
         y(1)=0.0
         x(2)=x(1)
         y(2)=tpeak
         x(3)=wmean+0.5*bwidth
         y(3)=y(2)
         x(4)=x(3)
         y(4)=0.0
         call higr_drdash(x,y,4,0.1,0.1,0.8)
         x(1)=wmean
         y(1)=0.0
         x(2)=x(1)
         y(2)=tpeak
         call higr_drdash(x,y,2,0.33,0.33,0.33)
         call higr_gzend(4)
      endif
 
 
      end
 
 
 
