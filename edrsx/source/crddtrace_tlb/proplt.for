      subroutine proplt(par,npar,prof,lprof,pscale,pzero,psize,
     :                  pinval,boxxlo,boxxhi,boxylo,boxyhi,xlim,
     :                  scndir,offset,ndets,unsmin,unsmax,trace,scale,
     :                  zero,plin,plines,x,y)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To plot a profile overlayed on the data display.
*
*SOURCE
*       PROPLT.FOR in CRDDTRACE.TLB
*
*ARGUMENTS
*   INPUTS:
*       par(npar)       real*8  The source fitting parameters (see FUNCT3)
*       npar            integer The number of fitting parameters
*       prof(lprof,plines) integer*2    Unscaled profile image
*       lprof           integer Length of each line in profile image
*       pscale          real    Scale factor for unscaled profile image
*       pzero           real    Zero offset for unscaled profile image
*       psize           real    Size of each profile pixel in arcmins
*       pinval          integer The value stored at invalid profile pixels
*       boxxlo,boxxhi   reals   The lower and upper x limits of plotting
*                               box in SGS base zone co-ords
*       boxylo,boxyhi   reals   The lower and upper y limits of plotting
*                               box in SGS base zone co-ords
*       xlim(2)         real    The lower and upper limits of the x axis
*                               scale in arcmins north of centre
*       scndir        character The scan direction 'NORTH' or 'SOUTH'
*       offset(ndets)   real    Offsets for each trace in unscaled data units
*       ndets           integer The size of array 'offset'
*       unsmin          real    The unscaled data value at the bottom
*                               of the plotting box
*       unsmax          real    The unscaled data value at the top of
*                               the plotting box
*       trace           integer The trace no. on which the source is located
*       scale           real    The scale factor for unscaled data values
*       zero            real    The zero offset for unscaled data values
*       plin            integer The line within the profile image to plot
*       plines          integer The number of lines in the profile image
*   OUTPUTS:
*       x,y     reals   The source peak position in SGS base zone co-ords
*
*SUBROUTINES CALLED
*       SGS:
*               sgs_ipen,sgs_spen,sgs_icurz,sgs_izone,sgs_zone,sgs_sw,
*               sgs_bpoly,sgs_apoly,sgs_opoly,sgs_selz,sgs_relz
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npar,lprof,pinval,ndets,trace,plin,plines
      real      pscale,pzero,boxxlo,boxxhi,boxylo,boxyhi,scale,zero
      real*8    par(npar)
      real      xlim(2),offset(ndets),unsmin,unsmax,psize,x,y
      integer*2 prof(lprof,plines)
      character scndir*(*)
*
* DECLARE LOCAL VARIABLES
*
      real      arcmin  ! The position in arcmins north of centre, of
                        ! the current profile pixel
      logical   base    ! True if current profile pixel is in the base area
      real      factor  ! Used to reverse the direction of the profile
                        ! if the scan was from north to south
      integer   ierr    ! Error status
      integer   iz      ! SGS zone identifier of zone covering plotting box
      integer   j       ! Profile pixel count
      real      lval    ! The value of the previous profile pixel
      real      nxtval  ! The value of the next profile pixel
      integer   oldpen  ! The SGS pen number active on entry
      real      oldxlo  ! Low x limit of world co-ords active on entry
      real      oldxhi  ! High x limit of world co-ords active on entry
      real      oldylo  ! Low y limit of world co-ords active on entry
      real      oldyhi  ! High y limit of world co-ords active on entry
      real      oldxm   ! Extent of x in metres of zone active on entry
      real      oldym   ! Extent of y in metres of zone active on entry
      integer   oldzon  ! Identifier of SGS zone active on entry
      logical   penup   ! True if pen is currently up (ie not drawing)
      real      proval  ! The value of the current profile pixel
      real      u       ! Offset from the current pixel to the profile
                        ! centre scanwards, in arcmins.
      real      unsdat  ! The value of the current profile pixel in
                        ! unscaled data units
*
* SAVE OLD ZONE AND PEN INFO
*
      call sgs_ipen(oldpen)
      call sgs_icurz(oldzon)
      call sgs_izone(oldxlo,oldxhi,oldylo,oldyhi,oldxm,oldym)
*
* SET ZONE TO PLOTTING BOX AND SET PEN NUMBER TO 2
*
      call sgs_spen(2)
      call sgs_zone(boxxlo,boxxhi,boxylo,boxyhi,iz,ierr)
*
* SET X CO-ORDS TO BE ARCMINS AND Y CO-ORDS TO BE UNSCALED DATA UNITS
*
      call sgs_sw(xlim(1),xlim(2),unsmin-(offset(trace)-offset(1)),
     :               unsmax-(offset(trace)-offset(1)),ierr)
*
* REVERSE PROFILE DIRECTION IF SCAN WAS NORTH TO SOUTH
*
      if(scndir.eq.'NORTH') then
         factor=1
      else
         factor=-1
      endif
*
* INITIALIZE PEN UP AND PREVIOUS PROFILE VALUE TO 0
*
      penup=.true.
      lval=0.0
*
* LOOP THROUGH PROFILE PIXELS
*
      do j=1,lprof
*
* CALCULATE WHAT THE PROFILE VALUE WILL BE ON THE NEXT LOOP
*
         if(j.lt.lprof) then
            if(prof(j+1,plin).ne.pinval) then
               nxtval=pscale*prof(j+1,plin)+pzero
            else
               nxtval=0
            endif
         else
            nxtval=0
         endif
*
* IF THE NEXT PROFILE VALUE AND THE PREVIOUS PROFILE VALUE ARE BOTH
* ZERO THEN THE CURRENT PIXEL IS ON THE PROFILE BASE AND WILL NOT BE
* DISPLAYED
*
         if(nxtval.ne.0.or.lval.ne.0) then
            base=.false.
         else
            base=.true.
         endif
*
* IF THE CURRENT PIXEL IS VALID AND NOT PART OF THE PROFILE BASE THEN
* ENSURE PEN IS DOWN AND DRAW IT
*
         if(prof(j,plin).ne.pinval.and.(.not.base)) then
            u=(j-(int(lprof*0.5)+1))*psize
            arcmin=factor*(par(2)+u)
            proval=pscale*prof(j,plin)+pzero
            unsdat=(par(1)*proval+par(3)*u+par(4)-zero)/scale
            if(penup) then
               call sgs_bpoly(arcmin,unsdat)
               penup=.false.
            else
               call sgs_apoly(arcmin,unsdat)
            endif
            lval=proval
*
* OTHERWISE LIFT PEN UP
*
         else
            if(.not.penup) then
               call sgs_opoly
               penup=.true.
            endif
            lval=0
         endif
*
* LOOP FOR NEXT PROFILE PIXEL
*
      enddo
*
* CALCULATE X AND Y OF SOURCE PEAK IN SGS CO-ORDS
*
      x=boxxlo+(factor*par(2)-xlim(1))*(boxxhi-boxxlo)/(xlim(2)-xlim(1))
      y=boxylo+((par(1)+par(4)-zero)/scale-unsmin+(offset(trace)-
     :  offset(1)))*(boxyhi-boxylo)/(unsmax-unsmin)
*
* RE-INSTATE PREVIOUS SGS STATE
*
      call sgs_spen(oldpen)
      call sgs_selz(oldzon,ierr)
      call sgs_relz(iz)
      if(ierr.eq.0) call sgs_sw(oldxlo,oldxhi,oldylo,oldyhi,ierr)
*
* FINISH
*
      end
