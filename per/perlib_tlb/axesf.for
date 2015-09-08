      subroutine per_axesf(lx,ly)
 
*
*   This subroutine obtains a plotting device and, if it is a plotter,
*   allows the user to define the size of the plot.
*
*   It then opens the plotting device and draws some axes,
*   allowing data between the limit arrays LX and LY to be plotted.
*
*
*   Written by K.F.Hartley at RGO on 13-1-84
*
      real lx(2),ly(2)
      real size(2)
      character*8 dev
      logical cur,first
      data first/.true./
 
      if (.not.first) go to 200
 
*
*   First get, and validate the device.
*
100   continue
      dev='ARGS'
      call rdkeyc('DEVICE',.true.,1,dev,i,status)
      call str$upcase(dev,dev)
 
      if (status.ne.err_normal.and.status.ne.err_parnul) then
         call wrerr('HELLDEV')
         call cnpar('DEVICE',status)
         go to 100
 
      end if
 
 
*
*   Check for a valid device name
*
      call devtran(dev,idev,iconid,istat)
 
      if (istat.ne.0) then
         call wruser('Failed to recognize the device.',istat)
         call per_devhlp
         call cnpar('DEVICE',status)
         go to 100
 
      end if
 
      first=.false.
 
*
*   So, we are now ready to open the Simpleplot device
*
      call jbdev(dev)
 
*
*   Now inquire (a) what size of axes and (b) whether it has a
*   a cursor (ie if a terminal or a plotter).
*
      call jbinq(xl,yl,cur)
 
*
*   If it is a plotter ask for the size of the required plot
*   The default will plot it on a sheet of A4 paper.
*
 
      if (.not.cur) then
         call wruser(' Input X & Y dimensions of plot (cms.)',status)
         call wruser(' The default size fits on A4 paper',status)
         size(1)=25.0
         size(2)=16.0
         call rdkeyr('SIZE',.true.,2,size,i,status)
         xl=size(1)
 
*
*      This ensures a square plot if only one size was specified.
*
 
         if (size(2).ne.0.0) then
            yl=size(2)
 
         else
            yl=size(1)
         end if
 
      end if
 
 
*
*   and plot some axes.
*
200   continue
      call jbaxes(lx,2,xl,'FREQUENCY',9,ly,2,yl,'POWER',5)
 
      end
 
 
 
