      subroutine disply(image,npix,nlines,scale,zero,inval,c,ierr)
      integer np(4),nl(4),id(1)
      integer*2 image(npix,nlines),mtrx(2560)
      real c(6),d(6),wk1(512),wk2(512),x1(1),y1(1),x2(1),y2(1)
      character commnd*20,device*20
      logical roll,init
      data ndev/1/,device/'NEWBURY'/,roll/.false./
      data np/60,80,1,1/,nl/23,32,1,1/
      save ndev,roll
      init=.false.
      xmin=0.5
      xmax=npix+0.5
      ymin=0.5
      ymax=nlines+0.5
      thresh=0.0
      range=32767.0*scale
      c(1)=0.0
      c(2)=1.0
      c(3)=0.0
      c(4)=0.0
      c(5)=0.0
      c(6)=1.0
11    ncmd=1
      call getcmd('DSPLYCMD','DISPLAY,THRESHOLD,RANGE,ROLL,SATURATE,CU'/
     : / 'RSOR,SCREEN,KEYBOARD,DEVICE,REDRAW,RETURN.',1,ncmd,commnd
     :     ,lcmd,ierr)
      call cnpar('DSPLYCMD',kstat)
 
      if(commnd.eq.'DISPLAY'.or.commnd.eq.'REDRAW') then
 
         if(commnd.eq.'REDRAW') then
            xmin=0.5
            xmax=npix+0.5
            ymin=0.5
            ymax=nlines+0.5
            init=.false.
         endif
 
 
         if(.not.init) then
            call imgmag(image,npix,nlines,inval,xmin,xmax,ymin,ymax
     :       ,mtrx,np(ndev),nl(ndev),-32767,wk1,wk2,redfx,redfy,ierr)
            init=.true.
         endif
 
 
         if(device.eq.'NEWBURY') then
            call nwbpic(mtrx,-32767,scale,zero,range,thresh,roll)
            c(1)=-xmin/redfx
            c(2)=1.0/redfx
            c(3)=0.0
            c(4)=-ymin/redfy
            c(5)=1.0/redfy
            c(6)=0.0
         endif
 
 
      else if(commnd.eq.'CURSOR') then
         continue
 
      else if(commnd.eq.'SCREEN') then
         continue
 
      else if(commnd.eq.'KEYBOARD') then
         init=.false.
         call cnpar('CORNER1',istat)
         call xyinpt('CORNER1','ID','KEYBOARD',.false.,0.5,npix+0.5
     :    ,0.5,nlines+0.5,x1,y1,id,1,nin1,ierr)
 
         if(nin1.ne.1) then
            x1(1)=xmin
            y1(1)=ymin
         endif
 
         call cnpar('CORNER2',istat)
         call xyinpt('CORNER2','ID','KEYBOARD',.false.,0.5,npix+0.5
     :    ,0.5,nlines+0.5,x2,y2,id,1,nin2,ierr)
 
         if(nin2.ne.1) then
            x2(1)=xmax
            y2(1)=ymax
         endif
 
         xmin=min(x1(1),x2(1))
         xmax=max(x1(1),x2(1))
         ymin=min(y1(1),y2(1))
         ymax=max(y1(1),y2(1))
 
      else if(commnd.eq.'DEVICE') then
         call cnpar('DEVICE',istat)
         call getcmd('DEVICE','NEWBURY.',1,ndev,device,ldev,ierr)
 
      else if(commnd.eq.'RANGE') then
         call cnpar('RANGE',istat)
         call getpar('RANGE','REAL',1,1.0e-20,-1.0e-20,.true.,ival,
     :   range,ierr)
 
      else if(commnd.eq.'THRESHOLD') then
         call cnpar('THRESH',istat)
         call getpar('THRESH','REAL',1,-1.0e20,1.0e20,.true.,ival,
     :   thresh,ierr)
 
      else if(commnd.eq.'ROLL') then
         roll=.true.
         call wruser(' OK',istat)
 
      else if(commnd.eq.'SATURATE') then
         roll=.false.
         call wruser(' OK',istat)
 
      else if(commnd.eq.'RETURN') then
         return
      endif
 
      go to 11
 
 
      end
 
 
 
