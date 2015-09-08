      subroutine trdisp(ymax,xlims,title,xtitle,units,scale,
     :                  zero,inval,refsmp,smpsiz,ndtout,data,npix,
     :                  nlin,dets,ballno,ndets,colour,scndir,spaces,
     :                  offset,avrges,boxxlo,boxxhi,boxylo,boxyhi,
     :                  samplo,samphi,unsmax,unsmin,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To draw the data display
*
*SOURCE
*       TRDISP.FOR in CRDDTRACE.TLB
*
*METHOD
*       First define size of plotting box, then produce background
*       (trace independant features), then loop to display information
*       associated with each detector trace. Note, the data is accessed
*       in its unscaled form U, where flux = scale*U+zero. The traces
*       always have the southern end of the scan at the left.
*
*ARGUMENTS
*   INPUTS:
*       ymax            real    The upper limit of the y axis in flux
*                               units
*       xlims(2)        real    The lower and upper limits of the x
*                               axis in arcmins north of the field
*                               centre
*       title        character  A title to put at the top of the display
*       xtitle       character  A title for the x axis
*       units        character  A title for the y axis
*       scale           real    The scale factor for the unscaled data
*       zero            real    The zero factor for the unscaled data
*       inval           integer The data value of invalid or blank
*                               pixels
*       refsmp          real    The fractional pixel position of the
*                               field centre within each line of input
*       smpsiz          real    The in-scan size in arcmins of each
*                               pixel
*       ndtout          integer The number of traces to display
*       data(npix,nlin) real    The input CRDD data to display
*       npix            integer The number of pixels in each input line
*       nlin            integer The number of lines in the input data
*       dets(ndets)     integer A list of detectors to display given as
*                               the detectors focal plane z position
*       ballno(ndets)   integer A list of Ball numbers corresponding to
*                               the list of detectors in 'dets'.
*       ndets           integer The size of the arrays 'dets' and
*                               'ballno'
*       colour          integer 1 if current graphics device supports
*                               colour graphics, 0 otherwise
*       scndir        character The direction of the scan, NORTH or
*                               SOUTH.
*       spacing       character The method to use for calculating the Y
*                               axis offsets for each data trace. Either
*                                 CONSTANT - The zero levels are equally
*                                            spaced
*                                 AVERAGE  - The mean data levels are
*                                            equally spaced
*                                 FREE     - The user specifies the
*                                            offsets
*       offset(ndets)   real    The offset values for each data trace.
*       avrges(ndets)   real    The 'average' unscaled data value of
*                               each detector stream
*   OUTPUTS:
*       boxxlo,boxxhi   reals   The limits of the plotting box in
*       boxylo,boxyhi           SGS base zone units
*       samplo,samphi   reals   The extremal samples displayed
*       unsmax,unsmin   reals   The limits of the plotting box scale
*                               in unscaled data units
*       ierr            integer Status return : (* => reported)
*                                0 - Success
*                                1 -*Y axis is of insufficient extent
*                                2 -*Pixels have zero in-scan size
*                                3 -*X axis is of insufficient extent
*                                4 -*Scale factor for data is zero
*                                5 - Error generating background
*                                6 -*No traces requested
*                                7 - Error setting zone to plot data in
*                                8 - Error setting world co-ords to plot
*                                    data in
*                                9 - Error re-instating original zone
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr,drline
*       THIS PACKAGE (CRDDTRACE.TLB):
*               bckgnd,unslim,caloff,taboff
*       EDRS:
*               lbgone
*       SGS:
*               sgs_icurz,sgs_izone,sgs_zone,sgs_stxj,sgs_sw,sgs_line,
*               sgs_opoly,sgs_bpoly,sgs_flush,sgs_shtx,sgs_tx,sgs_spen,
*               sgs_sartx,sgs_selz,sgs_clrz
*
*STARLINK PARAMETERS
*       NOYEXT/error/   Accessed if the y scale is of insufficient
*                       extent to display
*       NOSMPSIZ/error/ Accessed if the input argument smpsiz is zero
*       NOXEXT/error/   Accessed if the x scale is of insufficient
*                       extent to display
*       ZEROSCA/error/  Accessed if the input argument scale is zero
*       NOTRACES/error/ Accessed if input argument ndtout is zero
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer           inval,ndtout,npix,nlin,ndets,ierr,colour
      integer           dets(ndets),ballno(ndets)
      real              ymax,xlims(2),scale,zero,refsmp,smpsiz
      real              data(npix,nlin),offset(ndets),avrges(ndets)
      character*(*)     title,xtitle,units,scndir,spaces
      real              boxxlo,boxxhi,boxylo,boxyhi,samplo,samphi
      real              unsmax,unsmin
*
* DECLARE LOCAL VARIABLES
*
      logical   first   ! True if doing first sample, used by routine
                        ! DRLINE
      real      holxhi  ! Upper limit in x of trace identifier hole
      real      holxlo  ! Lower limit in x of trace identifier hole
      real      holyhi  ! Y position of trace on leaving in-line trace
                        ! identifier hole
      real      holylo  ! Y position of trace on entering in-line trace
                        ! identifier hole
      character idbuf*3 ! Buffer for trace identifier text strings
      integer   iz      ! Identifier of zone used for plotting traces
      real      offlen  ! Fractional length of zero offset tick marks
      real      oldxlo  ! Low x limit of world co-ords active on entry
      real      oldxhi  ! High x limit of world co-ords active on entry
      real      oldylo  ! Low y limit of world co-ords active on entry
      real      oldyhi  ! High y limit of world co-ords active on entry
      real      oldxm   ! Extent of x in metres of zone active on entry
      real      oldym   ! Extent of y in metres of zone active on entry
      integer   oldzon  ! Identifier of SGS zone active on entry
      real      overlp  ! Extent of space left for trace identifiers
      integer   pen1    ! SSG pen number for first CRDDTRACE pen
      integer   pen2    ! SSG pen number for second CRDDTRACE pen
      logical   penup   ! Represents state of graphics pen
      integer   trace   ! Trace counter from bottom to top
      real      sample  ! Pointer to sample (pixel) being drawn
      real      sampx   ! X position used for drawing sample value
      real      unsval  ! The unscaled data value of the current sample
      real      unszer  ! Unscaled data value for zero scaled data value
      real      ymin    ! Lower limit of the y axis scale in flux units
*
* CHECK THAT THERE ARE SOME TRACES TO DISPLAY
*
      if(ndtout.eq.0) then
         call wrerr('NOTRACES')
         ierr=6
         goto 999
      endif
*
* CALCULATE HIGHEST AND LOWEST PIXELS WHICH NEED TO BE DISPLAYED TO GIVE
* THE REQUIRED X AXIS RANGE
*
      if(smpsiz.ne.0) then
         if(scndir.eq.'NORTH') then
            samplo=refsmp+xlims(1)/smpsiz
            samphi=refsmp+xlims(2)/smpsiz
         else
            samplo=refsmp-xlims(2)/smpsiz
            samphi=refsmp-xlims(1)/smpsiz
         endif
      else
         call wrerr('NOSMPSIZ')
         ierr=2
         goto 999
      endif
*
* CHECK THAT X AXIS IS OF SUFFICIENT EXTENT TO DISPLAY
*
      if(samphi-samplo.lt.2) then
         call wrerr('NOXEXT')
         ierr=3
         goto 999
      endif
*
* CHECK THAT SCALE FACTOR IS NOT ZERO AND CALCULATE UNSCALED VALUE WHICH
* CORRESPONDS TO ZERO SCALED VALUE
*
      if(scale.eq.0) then
         call wrerr('ZEROSCA')
         ierr=4
         goto 999
      endif
      unszer=-zero/scale
*
* GET UPPER AND LOWER LIMITS OF UNSCALED DATA VALUES IN THE LOWEST
* DISPLAYED TRACE, RESTRICTING SEARCH TO JUST THE DATA WHICH WILL BE
* DISPLAYED
*
      call unslim(data,npix,nlin,dets(1),samplo,samphi,inval,
     :            unsmin,unsmax)
      if(unsmin.eq.inval) unsmin=unszer
*
* SAVE IDENTIFIER AND WORLD CO-ORDINATES OF CURRENTLY ACTIVE ZONE
*
      call sgs_icurz(oldzon)
      call sgs_izone(oldxlo,oldxhi,oldylo,oldyhi,oldxm,oldym)
*
* CLEAR DISPLAY SURFACE
*
      call sgs_clrz
*
* IF GRAPHICS DEVICE HAS COLOUR SET PEN 1 TO WHITE AND PEN 2 TO GREEN
* (ON ARGS), OTHERWISE SET BOTH TO WHITE
*
      if(colour.eq.1) then
         pen1=1
         pen2=3
      else
         pen1=1
         pen2=1
      endif
*
* CALCULATE THE LOWER LIMIT OF THE FLUX SCALE
*
      ymin=scale*unsmin+zero
*
* CHECK THAT Y AXIS IS OF SUFFICIENT EXTENT TO DISPLAY
*
      if(ymax-ymin.lt.1.0e-24) then
         call wrerr('NOYEXT')
         ierr=1
         goto 999
      endif
*
* DEFINE SIZE OF BOX IN WHICH TRACES WILL BE PLOTTED
*
      boxxlo=0.18
      boxxhi=0.88
      boxylo=0.24
      boxyhi=0.94
*
* DISPLAY THE BACKGROUND (TRACE INDEPENDANT) FEATURES
*
      call bckgnd(boxxlo,boxxhi,boxylo,boxyhi,ymax,ymin,xlims,title,
     :            xtitle,units,pen1,pen2,scndir)
      if(ierr.ne.0) then
         ierr=5
         goto 999
      endif
*
* CALCULATE CONSTANTS NEEDED TO DISPLAY TRACES (SEE DECLARATION OF EACH
* VARIABLE FOR A DESCRIPTION OF WHAT THEY ARE )
*
      unsmax=(ymax-zero)/scale
      holxlo=xlims(1)*0.95+xlims(2)*0.05
      holxhi=xlims(1)*0.90+xlims(2)*0.10

*
* CALCULATE OFFSETS FOR EACH TRACE
*
      call caloff(offset,ndets,ndtout,unszer,inval,unsmax,unsmin,
     :            spaces,avrges,scale,zero,dets)
*
* SET CURRENT ZONE TO BE THE PLOTTING BOX PLUS A 5% OVERLAP AT RIGHT
* HAND EDGE FOR PLOTTING TRACE IDENTIFIERS
*
      overlp=0.05
      call sgs_zone((1+overlp)*boxxlo-overlp*boxxhi,boxxhi,
     :              boxylo,boxyhi,iz,ierr)
      if(ierr.ne.0) then
         ierr=7
         goto 999
      endif
*
* SET TEXT JUSTIFICATION AND ASPECT RATIO FOR TRACE IDENTIFIERS
*
      call sgs_stxj('CC')
      call sgs_sartx(0.667*(xlims(2)-xlims(1))/(unsmax-unsmin))
*
* LOOP TO DRAW ALL TRACES
*
      do trace=1,ndtout
*
* SET WORLD CO-ORDS SO THAT USING UNSCALED DATA VALUES FOR PLOTTING WILL
* RESULT IN THE TRACE BEING IN THE RIGHT PART OF THE DISPLAY
*
         call sgs_sw((1+overlp)*xlims(1)-overlp*xlims(2),xlims(2),
     :               unsmin-(offset(trace)-offset(1)),
     :               unsmax-(offset(trace)-offset(1)),ierr)
         if(ierr.ne.0) then
            ierr=8
            goto 999
         endif
*
* DRAW LEFT HAND ZERO OFFSET MARKER FOR THIS TRACE
*
         offlen=0.036
         call sgs_spen(pen2)
         call sgs_line(xlims(1),unszer,
     :                 (1-offlen)*xlims(1)+offlen*xlims(2),unszer)
*
* DRAW RIGHT HAND ZERO OFFSET MARKER FOR THIS TRACE
*
         call sgs_line(offlen*xlims(1)+(1-offlen)*xlims(2),unszer,
     :                 xlims(2),unszer)
*
* INITIALIZE GRAPHICS PEN UP
*
         penup=.true.
         call sgs_spen(pen1)
         call sgs_opoly
         call sgs_flush
         first=.true.
*
* LOOP THROUGH ALL UNSCALED DATA VALUES TO DISPLAY TRACE
*
         do sample=samplo,samphi
*
* SAMPLE IS A FRACTIONAL MEASURE IN WHICH A CHANGE OF 1 IS EQUIVALENT TO
* A MOVEMENT OF ONE SAMPLE SIZE ON THE SKY. SINCE THE LOWEST ACTUAL
* PIXEL NUMBER IS 1, THE LOWER LIMIT OF SAMPLE IS 0.5 (I.E. HALF A
* SAMPLE LOWER THAN THE MIDDLE OF PIXEL 1) AND A VALUE OF SAMPLE OF
* NPIX+0.5 IS THE POSITION ON THE SKY AT THE UPPER END OF THE LAST PIXEL
* THE PIXEL WITH CENTRE NEAREST TO THE REQUIRED SAMPLE POSITION IS USED
* FOR THE SAMPLE VALUE, UNLESS THE DISTANCE TO ANY PIXEL IS GREATER THAN
* 0.5 IN WHICH CASE THERE IS NO DATA FOR THAT REGION AND THE SAMPLE
* TAKES THE INVALID VALUE
*
            if(sample.ge.0.5.and.sample.le.npix-0.5) then
               unsval=data(nint(sample),dets(trace))
            else
               unsval=inval
            endif
*
* THE DATA VALUE IS PLOTTED FROM THE MIDDLE OF THE PIXEL USED AND IS
* REVERSED IN X IF THE SCAN IS FROM NORTH TO SOUTH TO ENSURE THAT SOUTH
* IS ALWAYS AT THE LEFT
*
            if(scndir.eq.'NORTH') then
               sampx=(nint(sample)-refsmp)*smpsiz
            else
               sampx=-(nint(sample)-refsmp)*smpsiz
            endif
*
* IF DATA IS INVALID THEN DONT DRAW IT
*
            if(unsval.eq.inval) then
               if(.not.penup) then
                  penup=.true.
                  call sgs_opoly
                  call sgs_flush
               endif
*
* IF DATA WAS VALID AND .....
*
            else
*
* THE PEN IS CURRENTLY UP, PUT PEN DOWN.
*
               if(penup) then
                  call sgs_bpoly(sampx,unsval)
                  penup=.false.
*
* OTHERWISE CONTINUE POLY LINE WITH PEN DOWN, LEAVING A HOLE IN THE
* TRACE FOR THE TRACE IDENTIFIER
*
               else
                  call drline(sampx,unsval,holxlo,holxhi,holylo,
     :                        holyhi,first)
               endif
            endif
*
* DRAW NEXT SAMPLE
*
         enddo
*
* DRAW LEFT HAND EDGE TRACE IDENTIFIER
*
         call sgs_shtx(0.015*(unsmax-unsmin))
         call sgs_spen(pen2)
         write(idbuf,10) ballno(dets(trace))
 10      format('#',I2)
         call lbgone(idbuf(2:))
         call sgs_tx((1+0.5*overlp)*xlims(1)-0.5*overlp*xlims(2),unszer,
     :                idbuf)
*
* DRAW IN-LINE TRACE IDENTIFIERS
*
         call sgs_shtx(0.013*(unsmax-unsmin))
         call sgs_spen(pen1)
         call sgs_tx(0.5*(holxlo+holxhi),0.5*(holylo+holyhi),idbuf)
*
* DRAW NEXT TRACE
*
      enddo
*
* REINSTATE ORIGINAL SGS ZONE AND WORLD CO-ORDINATES
*
      call sgs_selz(oldzon,ierr)
      if(ierr.eq.0) call sgs_sw(oldxlo,oldxhi,oldylo,oldyhi,ierr)
      if(ierr.ne.0) ierr=9
*
* DISPLAY OFFSETS
*
      call taboff(offset,ndets,ndtout,dets,ndets,ballno,scale,zero)
*
* FINISH
*
 999  continue

      end
