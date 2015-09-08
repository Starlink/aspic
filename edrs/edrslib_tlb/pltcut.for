      subroutine pltcut(ia,npixa,nlinea,scale,zero,invala,device,title
     : ,ttlx,ttly,xa,ya,xcoeff,ylim,ib,npixb,nlineb,invalb,ierr)
 
*
*
      character title*(*),ttlx*(*),ttly*(*),device*(*)
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb)
      logical ok(2),lastok
      real x(3),y(3),xa(2),ya(2),xb(2),yb(2),c(6),xcoeff(2),ylim(2)
      byte tt(31),tx(31),ty(31)
      data ok/2*.true./
 
*
* SET X,Y LIMITS OF PROFILE IN THE OUTPUT ARRAY
*
      ierr=0
      xb(1)=1.0
      yb(1)=nlineb/2+1
      xb(2)=npixb
      yb(2)=yb(1)
 
*
* CALCULATE THE TRANSFORMATION COEFFICIENTS C FOR SAMPLING THE
* PROFILE
*
      ifit=3
      call lintrn(xb,yb,xa,ya,ok,2,c,ifit,ierr1)
 
*
* SAMPLE THE IMAGE, PUTTING THE PROFILE INTO IB
*
      call rebin(ia,npixa,nlinea,invala,invalb,1,npixb,1,nlineb,c,sc
     : ,2,ib,npixb,nlineb,ierr1)
 
*
* SCAN THE PROFILE, AVERAGING OVER ITS WIDTH
*
      imin=32767
      imax=-32767
      ngood=0
 
      do 14 i=1,npixb
         np=0
         ip=0
 
         do 13 j=1,nlineb
 
*
* IGNORE INVALID PIXELS
*
 
            if(ib(i,j).ne.invalb) then
               np=np+1
               ip=ip+ib(i,j)
            endif
 
13       continue
 
 
*
* CALCULATE THE MEAN, IF THERE WERE ANY GOOD PIXELS AND NOTE
* THE MIN AND MAX VALUES FOR SCALING THE GRAPH
*
 
         if(np.gt.0) then
            ib(i,1)=nint(real(ip)/real(np))
            ngood=ngood+1
            imin=min(imin,ib(i,1))
            imax=max(imax,ib(i,1))
         endif
 
14    continue
 
 
*
* IF THERE ARE NO GOOD PIXELS IN THE PROFILE, RETURN WITH ERROR
*
 
      if(ngood.le.0) then
         ierr=1
         go to 99
 
      endif
 
 
*
* IF NO PLOTTING IS REQUIRED, STOP AT THIS POINT
*
 
      if(device.eq.'NONE') go to 99
 
*
* OBTAIN DATA RANGE FOR GRAPH
*
 
      if(imax.le.imin)imax=imin+1
      s1=scale*imin+zero
      s2=scale*imax+zero
      amin=min(s1,s2)
      amax=max(s1,s2)
 
*
* CALCULATE PLOT X AXIS LIMITS
*
      xmin=xcoeff(1)
      xmax=(npixb-1)*xcoeff(2)+xcoeff(1)
 
*
* IF AUTOSCALING IS OVER-RIDDEN, GET NEW Y LIMITS
*
 
      if(abs(ylim(1)-ylim(2)).gt.1.0e-8*max(1e-20,abs(ylim(1)),
     :abs(ylim(2))))then
         amin=ylim(1)
         amax=ylim(2)
      endif
 
*
* OPEN NCAR FOR GRAPHICS
*
      call sgs_widen(device,itype,iconid,ierr)
      if(itype.eq.161.or.itype.eq.3201) then
         call ncropn(device,.false.,ierr)
      else
         call ncropn(device,.true.,ierr)
      endif
      if(ierr.ne.0) goto 99

*
* SET TEXT FONT
*
      call gtfont('FONT',ifont)
      call sgs_sfont(ifont)

*
* DRAW PLOT BACKGROUND
*
      call ncrbck(xmin,xmax,amin,amax,title,ttlx,ttly) 
*
* PLOT A HISTOGRAM STYLE GRAPH, IGNORING INVALID DATA VALUES
*
* LASTOK INDICATES IF THE PREVIOUS PIXEL WAS VALID
*
      lastok=.false.
 
      do 100 i=1,npixb
 
         if(ib(i,1).ne.invalb) then
            n=0
 
            if(lastok) then
               x(1)=i-1.5
               x(1)=x(1)*xcoeff(2)+xcoeff(1)
               y(1)=ib(i-1,1)*scale+zero
               n=1
            endif
 
            x(n+1)=i-1.5
            x(n+1)=x(n+1)*xcoeff(2)+xcoeff(1)
            y(n+1)=ib(i,1)*scale+zero
            x(n+2)=i-0.5
            x(n+2)=x(n+2)*xcoeff(2)+xcoeff(1)
            y(n+2)=y(n+1)
            n=n+2
            lastok=.true.
 
*
* PLOT ONE BIN
*
            call agcurv(x,1,y,1,n,1)
 
         else
            lastok=.false.
         endif
 
100   continue
 
 
*
* CLOSE NCAR
*
      call sgs_close
99    return
 
      end
 
 
 
