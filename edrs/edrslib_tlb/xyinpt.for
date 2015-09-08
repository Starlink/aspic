      subroutine xyinpt(namexy,nameid,source,needid,xmin,xmax,ymin,ymax
     : ,x,y,id,nmax,ninput,ierr)
      character namexy*(*),nameid*(*),source*(*)
      real x(nmax),y(nmax),xy(2)
      integer id(nmax)
      logical needid,exit
      ierr=0
 
      if(nmax.lt.1) then
         ierr=1
 
      else
         maxid=nmax-1
         rmaxid=maxid
         idxy=-1
         ninput=0
 
54       if(ninput.lt.nmax) then
            exit=.false.
 
            if(source.eq.'KEYBOARD') then
               call rdkeyr(namexy,.false.,2,xy,nval,istat)
               call cnpar(namexy,jstat)
 
               if(istat.ne.0) then
                  exit=.true.
 
                  if(istat.eq.4) ierr=2
               endif
 
 
            else if(source.eq.'CURSOR') then
               nval=2
               exit=.true.
            endif
 
 
            if(.not.exit) then
 
               if(nval.lt.2) then
                  call wrerr('TOOFEW')
 
               else if(xy(1).lt.xmin.or.xy(1).gt.xmax) then
                  call wrerr('BADVALUE')
                  call rngerr('X VALUE','REAL',xmin,xmax)
 
               else if(xy(2).lt.ymin.or.xy(2).gt.ymax) then
                  call wrerr('BADVALUE')
                  call rngerr('Y VALUE','REAL',ymin,ymax)
 
               else
                  ninput=ninput+1
                  x(ninput)=xy(1)
                  y(ninput)=xy(2)
 
                  if(needid) then
                     idxy=idxy+1
 
                     if(idxy.gt.maxid)idxy=0
                     call getpar(nameid,'INTEGER',1,0.0,rmaxid,.true
     :                .,idxy,rval,idstat)
                     id(ninput)=idxy
 
                  else
                     id(ninput)=ninput-1
                  endif
 
               endif
 
               go to 54
 
            endif
 
         endif
 
      endif
 
      return
 
      end
 
 
 
