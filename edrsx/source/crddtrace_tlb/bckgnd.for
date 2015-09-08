      subroutine bckgnd(boxxlo,boxxhi,boxylo,boxyhi,ymax,ymin,xlims,
     :                  title,xtitle,units,pen1,pen2,scndir)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Draws the trace-independant background features for CRDDTRACE
*
*SOURCE
*       BCKGND.FOR in CRDDTRACE.TLB
*
*ARGUMENTS
*   INPUTS:
*       boxxlo  real    Lower x limit of the plotting box in SGS units
*       boxxhi  real    Upper x limit of the plotting box in SGS units
*       boxylo  real    Lower y limit of the plotting box in SGS units
*       boxyhi  real    Upper y limit of the plotting box in SGS units
*       ymax    real    Upper limit of Y axis in flux units
*       ymin    real    Lower limit of y axis in flux units
*       xlims(2)real    Lower and upper limits of x axis in arcmins from
*                       field centre
*       title   character       Title for top of display
*       xtitle  character       Title for x axis
*       units   character       Title for y axis
*       pen1    Integer SGS pen number for features in pen 1
*       pen2    Integer SGS pen number for features in pen 2
*       scndir  character       Scan direction, NORTH or SOUTH.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               tickmk
*       SGS:
*               sgs_shtx,sgs_spen,sgs_box,sgs_stxj,sgs_line,sgs_tx,
*               sgs_flush,
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real              boxxlo,boxxhi,boxylo,boxyhi,ymax,ymin,xlims(2)
      character*(*)     title,xtitle,units,scndir
      integer           pen1,pen2
*
* DECLARE LOCAL VARIABLES
*
      real      deltic  ! The sub tick spacing in base zone sgs units
      integer   i       ! Loop count
      real      j       ! Loop count
      real      midx    ! X value of middle of box
      real      mntick  ! Spacing of main tick marks in scaled data units
      integer   mticks  ! The no. of main ticks drawn
      integer   nsbtic  ! The no. of sub tick marks between main tick marks
      character numbuf*9!Buffer for formating numbers prior to prunsing
      real      sbtick  ! Spacing of sub tick marks in scaled data units
      real      sbticy  ! The scaled data value of the next sub-tick
      integer   strlen  ! A function which returns the no. of non-blank leading
                        ! characters in a string
      real      temp    ! Temporary real storage
      real      textht  ! SGS text height
      integer   tick    ! Tick loop count
      real      ticky   ! The y value in base zone sgs units of next y tick
      real      tickx   ! The x value in base zone sgs units of next x tick
      real      unsbtm  ! Unscaled data value of bottom of plotting area
      real      unsdel  ! The offset between traces in unscaled data units
      real      unstop  ! Unscaled data value of top of plotting area
      real      unszer  ! Unscaled data value for scaled data value of zero
      real      xscal   ! Base zone sgs units per data sample
      real      yfull   ! The full extent of plotting zone before limiting
      real      yhigh   ! High limit of current plotting zone after limiting
      real      ylow    ! Low limit of current plotting zone in base units
      real      yrange  ! Scaled data range covered by scale at right of box
      real      yscal   ! Base zone sgs units per unscaled data unit
*
* SET TEXT HEIGHT
*
      textht=0.012
      call sgs_shtx(textht)
*
* DRAW BOX ROUND PLOTTING AREA
*
      call sgs_spen(pen2)
      call sgs_box(boxxlo,boxxhi,boxylo,boxyhi)
      call sgs_spen(1)
*
* CALCULATE SCALING OF Y AXIS IN SGS UNITS PER SCALED DATA UNIT
*
      yscal=(boxyhi-boxylo)/(ymax-ymin)
*
* CALCULATE TCIK MARK SPACING FOR FLUX SCALE
*
      call tickmk(ymax,ymin,mntick,sbtick,nsbtic)
*
* CALCULATE THE POSITION OF THE LOWEST MAIN TICK MARK
*
      mticks=int(ymin/mntick)-1
      ticky=boxylo+(mntick*mticks-ymin)*yscal
*
* CALCULATE THE SUB-TICK SPACING IN SGS UNITS
*
      deltic=sbtick*yscal
*
* LOOP TO DRAW ALL TICKS
*
      call sgs_stxj('CL')
      do while(ticky.le.boxyhi)
         if(ticky.ge.boxylo.and.ticky.le.boxyhi) then
            call sgs_line(boxxhi,ticky,boxxhi+0.014,ticky)
            write(numbuf,'(g9.2)') mntick*mticks
            call sgs_tx(boxxhi+0.0144,ticky,numbuf)
         endif
         ticky=ticky+deltic
         mticks=mticks+1
         do tick=1,nsbtic
            if(ticky.ge.boxylo.and.ticky.le.boxyhi) then
               call sgs_line(boxxhi,ticky,boxxhi+0.005,ticky)
            endif
            ticky=ticky+deltic
         enddo
      enddo
      call sgs_flush
*
* CALCULATE THE X AXIS SCALING IN BASE ZONE SGS UNITS PER ARCMIN
*
      xscal=(boxxhi-boxxlo)/(xlims(2)-xlims(1))
*
* CALCULATE TICK MARK SPACING FOR DISTANCE SCALE
*
      call tickmk(xlims(2),xlims(1),mntick,sbtick,nsbtic)
*
* CALCULATE THE POSITION OF THE LEFT HAND-MOST MAIN TICK MARK
*
      mticks=int(xlims(1)/mntick)-1
      tickx=boxxlo+(mntick*mticks-xlims(1))*xscal
*
* CALCULATE THE SUB-TICK SPACING IN SGS UNITS
*
      deltic=sbtick*xscal
*
* LOOP TO DRAW ALL TICKS
*
      call sgs_stxj('TC')
      do while(tickx.le.boxxhi)
         if(tickx.ge.boxxlo.and.tickx.le.boxxhi) then
            call sgs_line(tickx,boxylo,tickx,boxylo-0.015)
            write(numbuf,'(f6.1)') mntick*mticks
            call lbgone(numbuf)
            call sgs_tx(tickx,boxylo-0.017,numbuf(:strlen(numbuf)))
         endif
         tickx=tickx+deltic
         mticks=mticks+1
         do tick=1,nsbtic
            if(tickx.ge.boxxlo.and.tickx.le.boxxhi) then
               call sgs_line(tickx,boxylo,tickx,boxylo-0.005)
            endif
            tickx=tickx+deltic
         enddo
      enddo
      call sgs_flush
*
* DISPLAY TITLES
*
      midx=0.5*(boxxlo+boxxhi)
      call sgs_tx(midx,boxylo-0.023-textht,xtitle(:strlen(xtitle)))
      call sgs_stxj('BC')
      call sgs_tx(midx,boxyhi+0.025,title(:strlen(title)))
      call sgs_stxj('BL')
      call sgs_tx(boxxhi+0.01,boxyhi+0.02,units)
      temp=boxylo-0.035-2.5*textht
      call sgs_stxj('CC')
      call sgs_tx(boxxlo,temp,'South')
      call sgs_tx(boxxhi,temp,'North')
      call sgs_tx(midx,temp,'Scan direction')
      if(scndir.eq.'NORTH') then
         call sgs_line(midx+6*textht,temp,midx+9*textht,temp)
         call sgs_line(midx+8.5*textht,temp+0.5*textht,midx+9*textht,
     :                 temp)
         call sgs_line(midx+8.5*textht,temp-0.5*textht,midx+9*textht,
     :                 temp)
      else
         call sgs_line(midx-6*textht,temp,midx-9*textht,temp)
         call sgs_line(midx-8.5*textht,temp+0.5*textht,midx-9*textht,
     :                 temp)
         call sgs_line(midx-8.5*textht,temp-0.5*textht,midx-9*textht,
     :                 temp)
      endif
      call sgs_flush
*
* FINISH
*
      end
