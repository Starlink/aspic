      subroutine trim
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO TRIM AN IMAGE, OR A 3D STACK OF IMAGES TO SIZE, USING
*       THE TWO CORNER COORDINATES OF THE REQUIRED AREA.
*
*METHOD
*       OBTAIN THE CORNER COORDINATES FROM THE IKON, ARGS OR AS 
*	PARAMETERS, THEN EXTRACT THE REQUIRED REGION OF EACH INPUT 
*	IMAGE INTO THE OUTPUT IMAGE
*
*ARGUMENTS
*       NONE
*
*CALLS
*       THIS PACKAGE:
*
*       STARLINK:
*
*       ASPIC LIBRARY:
*
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      parameter (mxim=100)
      character xysrc*10,ccval(1)*1,cval*1,device*30
      real xya(2),xyb(2),rrval(1)
      integer ipin(mxim),inval(mxim),ipout(mxim),invout(mxim)
      logical exit,info(1)
*
* OBTAIN INPUT IMAGE(S)
*
      call gt3dir('INPUT',102,.false.,npix,nlines,nim,ipin,ierr)
 
      if(ierr.ne.0)go to 99
 
*
* DETERMINE IF USER IS TO BE TOLD HOW TO USE ARGS BUTTONS
*
      info(1)=.true.
      call rdkeyl('INFO',.true.,1,info,nval,istat)
 
*
* DETERMINE THE DEFAULT SOURCE OF COORDINATES
*
      call defdev(device)
      call sgs_widen(device,itype,iconid,ierr)
      if(itype.eq.3200) then
         ixy=1
      else
         ixy=2
      endif

*
* DETERMINE WHERE THE CORNER COORDINATES ARE ACTUALLY COMING FROM
*
      call getcmd('XYSOURCE','IKON,ARGS,PARAMETERS.',1,ixy,xysrc,lxy,
     :            ierr)
 
*
* IF FROM THE IKON OR ARGS, CHECK IT IS AVAILABLE
*
 
      if(xysrc.eq.'ARGS'.or.xysrc.eq.'IKON')then
         if(xysrc.eq.'ARGS') then
            call argsop(info,.false.,ierr)
         else
            call ikonop(info,.false.,ierr)
         endif
         if(ierr.ne.0) goto 99

*
* OPEN OVERLAY PLANE 9 FOR GREEN GRAPHICS ON SELECTED DEVICE
* 
        call ovopen(xysrc,9,'GREEN')

*
* GET COORDS FROM THE SCREEN
*
         if(info(1))call wruser('      Enter first corner...',istat)
         call xyscrn(ix,iy,xya(1),xya(2),exit,xysrc,ierr)
         call ovcros(xya(1),xya(2),1024,780,xysrc)
 
         if(info(1))call wruser(' ',istat)
 
         if(info(1))call wruser('      Enter second corner...',istat)
         call xyscrn(ix,iy,xyb(1),xyb(2),exit,xysrc,ierr)
         call ovcros(xyb(1),xyb(2),1024,780,xysrc)
 
         if(info(1))call wruser (' ',istat)

*
* CLOSE DOWN OVERLAY PLANES AND SHUT DEVICE
*
         call ovclos(xysrc,9)
         if(xysrc.eq.'ARGS') then
            call argscl
         else
            call ikoncl
         endif 
 
*
* OTHERWISE OBTAIN COORDS FROM USER
*
 
      else
         xya(1)=1.0
         xya(2)=1.0
         xyb(1)=npix
         xyb(2)=nlines
         call rdkeyr('XYA',.true.,2,xya,nval,ierr)
         call rdkeyr('XYB',.true.,2,xyb,nval,ierr)
      endif
 
 
*
* OBTAIN OUTPUT IMAGE SIZE
*
      ixa=nint(min(xya(1),xyb(1)))
      iya=nint(min(xya(2),xyb(2)))
      ixb=nint(max(xya(1),xyb(1)))
      iyb=nint(max(xya(2),xyb(2)))
      npout=ixb-ixa+1
      nlout=iyb-iya+1
 
*
* OBTAIN OUTPUT IMAGE(S)
*
      call gt3diw('OUTPUT',102,.false.,npout,nlout,nim,ipout,ierr)
 
      if(ierr.ne.0)go to 99
 
*
* OBTAIN INPUT INVALID PIXEL FLAG
*
 
      do 41 i=1,nim
         inval(i)=-100000
41    continue
 
      call gtdscn('INPUT','INVAL','INTEGER',1,nim,inval,rrval,ccval
     : ,ierr)
 
*
* TRIM EACH INPUT IMAGE IN TURN
*
 
      do 42 i=1,nim
         invout(i)=inval(i)
 
         if(abs(invout(i)).gt.32767)invout(i)=-32767
         call imgtrm(%val(ipin(i)),npix,nlines,inval(i),ixa,iya,
     :   %val(ipout(i)),npout,nlout,invout(i))
42    continue
 
 
*
* UPDATE OUTPUT DESCRIPTOR
*
      call cydscr('INPUT','OUTPUT',istat)
      call ptdscn('OUTPUT','INVAL','INTEGER',1,nim,invout,rrval,ccval
     : ,ierr)
      call ptdscr('OUTPUT','NAXIS1','INTEGER',npout,rval,cval,ierr)
      call ptdscr('OUTPUT','NAXIS2','INTEGER',nlout,rval,cval,ierr)
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
