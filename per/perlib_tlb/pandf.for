      subroutine per_pandf(in,inax,x,data,xs,yapr,temp,np,nptsout)
 
*
*   This is C.D.Pike's subroutine PLOTANFDIT which does just that -
*   it plots data and fits splines to it.
*
*   Written by C.D.Pike at RGO
*
      include 'INTERIM(FMTPAR)'
      include 'INTERIM(ERRPAR)'
      integer inax(2)
      double precision in(inax(1),inax(2))
      double precision step,start,end
      double precision x(np),data(np),xs(np),yapr(np)
      real ax(2),temp(np)
      real size(2)
      integer status
      character*8 dev
      logical cur,first
      data first/.true./
      npts=inax(2)
 
      do i=1,npts
         x(i) = in(1,i)
         data(i) = in(2,i)
      end do
 
 
*
*   Now obtain a graphics device
*   if it is the first call to this subroutine.
*
 
      if (first) then
100      continue
         dev='ARGS'
         call rdkeyc('DEVICE',.true.,1,dev,i,status)
 
*
*   Now validate the device
*
         call devtran(dev,idev,iconid,status)
 
         if (status.ne.0) then
            call wruser('Failed to recognize that device',status)
            call per_devhlp
            call cnpar('DEVICE',status)
            go to 100
 
         end if
 
 
*
*   Now open that device
*
         call jbdev(dev)
 
         if (idev.eq.12) print *,'-I'
 
*
*   and find out what size the axes should be
*
         call jbinq(xl,yl,cur)
 
*
*      If it has no cursor then it is assumed to be a plotter
*      and so might give a large plot.
*
 
         if (.not.cur) then
            size(1)=25.0
            size(2)=16.0
            call wruser('Specify X and Y size of plot in cms.',status)
            call wruser('The default fits on A4 paper',status)
            call rdkeyr('SIZE',.true.,2,size,i,status)
 
*
*        This ensures that a sensible plot is produced, even
*        if only one size was specified.
*
 
            if (i.ne.2) then
               size(2)=size(1)
            end if
 
            xl=size(1)
            yl=size(2)
         end if
 
      end if
 
12    continue
      ax(1) = x(1)
      ax(2) = x(npts)
 
*
*   Store in REAL TEMP
*
 
      do i=1,npts
         temp(i) = sngl(data(i))
      end do
 
 
*
*   Plot some axes
*
      call pen(1)
      call jbaxes(ax,2,xl,'TIME',4,temp,npts,yl,'DATA',4)
 
*
*   and the data
*
 
      do i=1,npts
         call mark pt(sngl(x(i)),temp(i),4)
      end do
 
 
*
*   Compute the X values for the output samples, assuming that
*   there are as many as in the input.
*
 
      if (first) then
         step = (x(npts)-x(1))/(dble(npts)-1.0d0)
 
*
*      On the first call the starting epoch is not vital
*
         start=x(1)
 
      else
 
*
*      On the subsequent calls it is essential that the
*      STEP remains unchanged and that the start epoch
*      is a whole number of steps from the end of the previous
*      one.
*
         start = real(int((x(1)-start)/step))*step+start
      end if
 
 
      if (first) then
         call wruser('Choose Start time and sample interval',status)
         call rdkeyd('START',.true.,1,start,i,status)
         call rdkeyd('STEP',.true.,1,step,i,status)
      end if
 
      nptsout=np
 
      do i=1,np
         xs(i) = start + dble(i-1)*step
 
         if(xs(i).gt.x(npts))  then
            nptsout = i-1
            go to 124
 
         end if
 
      end do
 
124   continue
 
*
*   Obtain the number of splines to fit
*
      ipen=2
123   continue
 
*
*      This limit is defined in subroutine PER_BELLS
*      by variable NSPLMX
*
*      There are notes there about how to increase the limit.
*
      call wruser('Enter number of splines to fit',status)
      call wruser('(No more than 500 splines can be fitted)',istat)
      call wruser('(Enter a <CR> to exit)',status)
      call rdkeyi('NSPL',.false.,1,nspl,i,status)
 
      if(status.eq.err_parnul)  go to 999
 
      if(status.ne.err_normal.or.nspl.eq.0)  then
         call cnpar('NSPL',status)
         go to 123
 
      end if
 
 
*
*   Fit the splines
*
      call per_bells(x,data,npts,x(1),x(npts),nptsout,nspl,xs,yapr,ier)
 
      if (ier.le.0) then
         call wrerr('HELLSPL',status)
         call exit
      end if
 
 
*
*   Plot the resulting fit
*
      call break
      call pen(ipen)
      ipen=ipen+1
 
      if (ipen.gt.8) ipen=2
 
      do i=1,nptsout
         call join pt(sngl(xs(i)),sngl(yapr(i)))
      end do
 
 
*
*   Go back and ask for new parameters (or exit)
*
      call cnpar('NSPL',status)
      go to 123
 
999   continue
      first=.false.
      call cnpar('NSPL',status)
 
      end
 
 
 
