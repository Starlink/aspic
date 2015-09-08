      subroutine pixmap
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PERFORM ARBITRARY PIXEL POSITION TRANSFORMATIONS ON AN IMAGE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      character*30 title(1),xtitle(1),ytitle(1)
 
*
* OBTAIN INPUT DATA IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierr)
 
      if(ierr.ne.0)go to 99
 
*
* SET DEFAULT OUTPUT IMAGE SIZE
*
      nx=npix
      ny=nlines
 
*
* GET INPUT DESCRIPTOR ITEMS
*
      call gtstds('INPUT',1,inval,bscale,bzero,title)
 
*
* GET INPUT X COORDINATE IMAGE
*
      call gt2dir('XIMAGE',102,.true.,npx,nlx,ipx,ierr)
 
*
* IF GIVEN, OBTAIN DESCRIPTOR ITEMS
*
 
      if(ierr.eq.0)then
         call gtstds('XIMAGE',1,invalx,xscale,xzero,xtitle)
 
*
* OTHERWISE GET A DUMMY IMAGE AND FILL IT WITH 1'S
*
 
      else
         npx=npix
         nlx=nlines
         nx=1
         call getdyn('X',102,npx*nlx,ipx,istat)
 
         if(istat.ne.0)then
            call wrerr('NOSPACE')
            go to 99
 
         endif
 
         call imgset(%val(ipx),npx,nlx,1)
         invalx=-100000
         xscale=1.0
         xzero=0.0
      endif
 
 
*
* GET INPUT Y COORDINATE IMAGE
*
      call gt2dir('YIMAGE',102,.true.,npy,nly,ipy,ierr)
 
*
* IF GIVEN, GET DESCRIPTOR ITEMS
*
 
      if(ierr.eq.0)then
         call gtstds('YIMAGE',1,invaly,yscale,yzero,ytitle)
 
*
* OTHERWISE GET A DUMMY IMAGE AND FILL IT WITH 1'S
*
 
      else
         npy=npix
         nly=nlines
         ny=1
         call getdyn('Y',102,npy*nly,ipy,istat)
 
         if(istat.ne.0)then
            call wrerr('NOSPACE')
            go to 99
 
         endif
 
         call imgset(%val(ipy),npy,nly,1)
         invaly=-100000
         yscale=1.0
         yzero=0.0
      endif
 
 
*
* OBTAIN DIMENSIONS OF OUTPUT IMAGE, USING CALCULATED DEFAULTS
*
      call getpar('NX','INTEGER',1,1.0,1.0e8,.true.,nx,rval,ierr)
      call getpar('NY','INTEGER',1,1.0,1.0e8,.true.,ny,rval,ierr)
 
*
* OBTAIN OUTPUT IMAGE
*
      call gt2diw('OUTPUT',102,.false.,nx,ny,ipout,ierr)
 
      if(ierr.ne.0)go to 99
 
*
* OBTAIN WORKSPACE
*
      call getdyn('W1',104,nx*ny,ipw1,istat1)
      call getdyn('W2',104,nx*ny,ipw2,istat2)
 
      if(max(istat1,istat2).ne.0)then
         call wrerr('NOSPACE')
         go to 99
 
      endif
 
 
*
* CALL PXMAP TO MAP THE PIXEL LOCATIONS
*
      call pxmap(%val(ipin),npix,nlines,inval,bscale,bzero,%val(ipx)
     : ,npx,nlx,invalx,xscale,xzero,%val(ipy),npy,nly,invaly,yscale
     :  ,yzero,%val(ipout),nx,ny,invalz,zscale,zzero,%val(ipw1),
     :  %val(ipw2))
 
*
* OBTAIN OUTPUT TITLE
*
      title(1)='Output from PIXMAP'
      call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* UPDATE OUTPUT DESCRIPTOR
*
      call cydscr('INPUT','OUTPUT',istat)
      call ptstds('OUTPUT',nx,ny,1,invalz,zscale,zzero,title)
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
