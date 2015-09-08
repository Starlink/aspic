      subroutine profil
 
*
*
      character cval*1,title(1)*30,device*30,ttl(1)*30,ttlx(1)*40,
     :ttly(1)*30,opts*255,xysrc*20
      real xy(2),x(2),y(2),ylim(2),xcoeff(2)
      logical info(1)
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierrin)
 
*
* IF NOT OBTAINED SUCCESSFULLY, QUIT
*
 
      if(ierrin.ne.0) go to 99
 
*
* OBTAIN REQUIRED DESCRIPTOR ITEMS
*
      invala=-100000
      scale=1.0
      zero=0.0
      title(1)=' '
      call gtdscr('INPUT','INVAL','INTEGER',invala,rval,cval,ierr)
      call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
      call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
      call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1),ierr)
      invalb=invala
 
      if(abs(invalb).gt.32767) invalb=-32767
 
*
* DETERMINE WHERE THE COORDINATES ARE COMING FROM
*
      ixy=3
      call getcmd('XYS','IKON,ARGS,PARAMETERS.',1,ixy,xysrc,lxy,ierr)
 
*
* IF FROM THE SCREEN......
*
 
      if(xysrc.eq.'ARGS'.or.xysrc.eq.'IKON')then
*
* DETERMINE IF USER IS TO BE TOLD HOW TO USE DEVICE BUTTONS
*
         info(1)=.true.
         call rdkeyl('INFO',.true.,1,info,nval,istat)
      
*
* OPEN THE DEVICE AND DISPLAY INFORMATION ABOUT USING BUTTONS
*
         if(xysrc.eq.'ARGS') then
            call argsop(info,.true.,ierr)
         else
            call ikonop(info,.true.,ierr)
         endif
         if(ierr.ne.0) goto 99
 
*
* SET UP OVERLAY PLANES FOR PLOTTING
*
         call ovopen(xysrc,9,'RED')
 
 
*
* GET COORDINATES
*
 
         if(info(1))call wruser('      Enter first point...',istat)
         call xyscrn(ix,iy,x(1),y(1),exit,xysrc,ierr)
         call ovcros(x(1),y(1),5,5,xysrc)
 
         if(info(1))call wruser(' ',istat)
 
         if(info(1))call wruser('      Enter second point...',istat)
         call xyscrn(ix,iy,x(2),y(2),exit,xysrc,ierr)
         call ovcros(x(2),y(2),5,5,xysrc)

         call ovpoly(x,y,2,xysrc)
         if(info(1))call wruser (' ',istat)

*
* CLOSE OVERLAYS
*
         call ovclos(xysrc,9)
 
*
* CLOSE THE DEVICE 
*
         if(xysrc.eq.'ARGS') then
            call argscl
         else
            call ikoncl
         endif
 
*
* OTHERWISE OBTAIN COORDS FROM USER
*
 
      else
         xy(1)=1.0
         xy(2)=1.0
         call rdkeyr('XY1',.true.,2,xy,nval,ierr)
         x(1)=xy(1)
         y(1)=xy(2)
         xy(1)=npix
         xy(2)=nlines
         call rdkeyr('XY2',.true.,2,xy,nval,ierr)
         x(2)=xy(1)
         y(2)=xy(2)
      endif
 
 
*
* OBTAIN WIDTH OF PROFILE
*
      iwidth=1
      call getpar('WIDTH','INTEGER',1,1.0,1.0e3,.true.,iwidth,rval,
     :ierr)
 
      if(mod(iwidth,2).eq.0) iwidth=iwidth+1
 
*
* CALCULATE THE LENGTH TO THE NEAREST PIXEL AND OBTAIN THE REQUIRED
* WORKSPACE
*
      length=nint(sqrt((x(2)-x(1))**2+(y(2)-y(1))**2))+1
 
      if(length.lt.2) length=2
      call getdyn('WORK',102,length*iwidth,ipwk,ierr)
 
*
* IF WORKSPACE WAS NOT AVAILABLE, QUIT WITH ERROR MESSAGE
*
 
      if(ierr.ne.0) then
         call wrerr('NOSPACE')
         go to 99
 
      endif
 
 
*
* OBTAIN THE PLOTTING DEVICE
*
      call defdev(device)
      call getdev('DEVICE',device,.true.,ierr)
      if(ierr.ne.0) goto 99

 
*
* GENERATE DEFAULT GRAPH HEADINGS
*
      ttl(1)=title(1)
      write(ttlx(1),32)nint(x(1)),nint(y(1)),nint(x(2)),nint(y(2))
32    format('LENGTH (',i5,',',i5,') TO (',i5,',',i5,')')
      call lbgone(ttlx(1)(32:))
      call lbgone(ttlx(1)(26:))
      call lbgone(ttlx(1)(15:))
      call lbgone(ttlx(1)(9:))
      ttly(1)='DATA VALUE'
 
*
* OBTAIN NEW HEADINGS IF REQUIRED
*
 
      if(ndev.gt.0) then
         call rdkeyc('HEADING',.true.,1,ttl,nval,istat)
         call rdkeyc('XHEAD',.true.,1,ttlx,nval,istat)
         call rdkeyc('YHEAD',.true.,1,ttly,nval,istat)
      endif
 
 
*
* OBTAIN Y LIMITS FOR GRAPH (DEFAULT IS AUTO SCALING)
*
      ylim(1)=0.0
      ylim(2)=0.0
      call rdkeyr('YLIM',.true.,2,ylim,nval,istat)
 
*
* OBTAIN COEFFICIENTS FOR TRANSFORMING GRAPH X COORDINATES
*
      xcoeff(1)=0.0
      xcoeff(2)=1.0
      call rdkeyr('XCOEFFS',.true.,2,xcoeff,nval,istat)
 
*
* CALL PLTCUT TO EXTRACT THE REQUIRED PROFILE AND PLOT IT
*
      call pltcut(%val(ipin),npix,nlines,scale,zero,invala,device,ttl(1)
     : ,ttlx(1),ttly(1),x,y,xcoeff,ylim,%val(ipwk),length,iwidth,invalb
     : ,ierr)
 
*
* QUIT IF AN ERROR WAS FOUND
*
 
      if(ierr.ne.0) then
         call wrerr('NONEVAL')
         go to 99
 
      endif
 
 
*
* OBTAIN THE OPTIONAL OUTPUT IMAGE
*
      call gt2diw('OUTPUT',102,.true.,length,1,ipout,ierr)
 
*
* IF IT WAS GIVEN, COPY THE EXTRACTED PROFILE INTO IT
*
 
      if(ierr.eq.0) then
         call imgcpy(%val(ipwk),length,1,%val(ipout))
 
*
* UPDATE OUTPUT DESCRIPTOR ITEMS
*
         call cydscr('INPUT','OUTPUT',istat)
         call ptdscr('OUTPUT','NAXIS1','INTEGER',length,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS2','INTEGER',1,rval,cval,ierr)
         call rdkeyc('TITLE',.true.,1,title,nval,istat)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
      endif
 
 
*
* RELEASE DATA AREAS AND RETURN
*
99    return
 
      end
 
 
 
