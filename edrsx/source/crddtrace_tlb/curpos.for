      subroutine curpos(boxxlo,boxxhi,boxylo,boxyhi,xlim,samplo,samphi,
     :                  unsmax,unsmin,offset,dets,ndets,ndtout,data,
     :                  npix,nlin,ballno,scale,zero,inval,scndir,lgunit)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Allows the user to position the cursor and displays the data
*       value of the nearest data trace at the pixel closest to the
*       x position of the cursor. The x position is given as arcmins
*       north of the centre .
*
*SOURCE
*       CURPOS.FOR in CRDDTRACE.TLB
*
*METHOD
*       It is assumed that the currently active SGS zone is the
*       base zone. The user is repeatedly asked for positions
*       until a position is given which is outside the plotting box.
*       The final scaled data value displayed is also written to the
*       output parameter DATA.
*
*ARGUMENTS
*   INPUTS:
*       boxxlo,boxxhi   reals   The limits of the plotting box
*       boxylo,boxyhi
*       xlim(2) real    The south and north limits of the x
*                               axis in arcmins north of centre
*       samplo,samphi   reals   The lower and upper limit of the x axis
*                               in data sample numbers (i.e. pixels)
*       unsmax,unsmin   reals   The limits of the y axis scale in
*                               unscaled data units
*       offset(ndets)   real    The offsets in y (in unscaled data
*                               units) of each detector
*       dets(ndets)     integer The list of detectors displayed
*       ndets           integer Size of arrays dets,ballno and offset
*       ndtout          integer The number of displayed detectors
*       data(npix,nlin) real    The unscaled time-corrected data
*       npix,nlin       integer The dimensions of array data
*       ballno(ndets)   integer The Ball no.s of the detectors in
*                               the current band
*       scale           real    The scale factor for converting from
*                               unscaled to scaled data values
*       zero            real    The zero factor for converting from
*                               unscaled to scaled data values
*       inval           integer The value stored in invalid pixels
*       scndir        character Direction of scan north or south
*       lgunit          integer Fortran logical unit no. to which logged
*                               values are written. 0 implies no
*                               logging required. On entry a -ve value
*                               implies no data has yet been logged.
*   OUTPUTS:
*       lgunit          integer A +ve value on exit implies some data
*                               has been logged
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               putpar,wrerr,setcup
*       THIS PACKAGE (CRDDTRACE.TLB):
*               poscon
*       EDRS:
*               lbgone
*       SGS:
*               sgs_icuav,sgs_reqcu,sgs_shtx,sgs_sartx,sgs_flush,
*               sgs_stxj,sgs_tx,sgs_clrbl,sgs_atxi,sgs_txr
*
*STARLINK PARAMETERS
*       DATA/write/     The final scaled data value displayed
*       NOCURS/error/   Accessed if the current device does not have a
*                       cursor.
*       NODATPAR/error/ Accessed if parameter DATA was not succesfully
*                       written to the environment
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 23/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   ndets,dets(ndets),ballno(ndets),ndtout,npix,nlin,inval
      integer   lgunit
      real      data(npix,nlin),boxxlo,boxxhi,boxylo,boxyhi,xlim(2)
      real      samplo,samphi,unsmax,unsmin,offset(ndets),scale,zero
      character scndir*(*)
*
* DECLARE LOCAL VARIABLES
*
      real      arcmin  ! The cursor x position in arcmins north of
                        ! field centre
      logical   cursor  ! True if cursor exists on current device
      character cval*1  ! Dummy character argument for putpar
      real      detdat  ! The unscaled data value of the trace nearest
                        ! to the cursor
      logical   exit    ! True when no more positions are required
      logical   first   ! True for the first position displayed
      real      jansky  ! The scaled data value of the nearest trace
      integer   ierr    ! Error status from putpar
      integer   ilin    ! Detector id of trace nearest to cursor
      integer   ival    ! Dummy integer argument for putpar
      real      midx    ! X position of output numbers
      integer   n       ! The cursor choice key identifier
      integer   nstdet  ! The nearest trace to the cursor
      integer   pixel   ! The cursor x position in pixels
      real      textht  ! SGS text height in base zone co-ords
      integer   trace   ! Trace nearest to cursor
      character txtbuf*80 ! Buffer for logged text
      real      txtgap  ! Size of gap between lines of text
      real      txttop  ! Y position of centre of top line of text
      real      x       ! The cursor x position in base zone co-ords
      real      y       ! The cursor y position in base zone co-ords

*
* SEE IF CURSOR EXISTS. IF NOT GIVE MESSAGE AND RETURN
*
      call sgs_icuav(cursor)
      if(.not.cursor) then
         call wrerr('NOCURS')
         goto 999
      endif
*
* IF THERE IS A CURSOR, DISPLAY HELPFUL MESSAGE
*
      textht=0.014
      txttop=0.09
      txtgap=0.009
      midx=boxxlo+11*textht
      call sgs_shtx(textht)
      call sgs_sartx(0.6667)
      call sgs_stxj('CC')
      call sgs_tx(0.5*(boxxlo+boxxhi),0.14,
     :            'Position cursor and press any key')
      call sgs_tx(0.5*(boxxlo+boxxhi),0.14-textht-txtgap,
     :            '(position cursor ouside data box to quit)')
      call sgs_stxj('CL')
      call sgs_tx(boxxlo,txttop,'Detector')
      call sgs_tx(boxxlo,txttop-(textht+txtgap),'Data value')
      call sgs_tx(boxxlo,txttop-2*(textht+txtgap),'X position')
      call sgs_tx(midx-textht,txttop,':')
      call sgs_tx(midx-textht,txttop-(textht+txtgap),':')
      call sgs_tx(midx-textht,txttop-2*(textht+txtgap),':')
      call sgs_flush
*
* LOOP UNTIL EXIT IS REQUESTED
*
      first=.true.
      exit=.false.
      do while(.not.exit)
*
* DISPLAY CURSOR AND GET A POSITION IN BASE ZONE SGS WORLD CO-ORDS
*
         call sgs_reqcu(x,y,n)
*
* IF INSIDE THE PLOTTING BOX THEN ....
*
         if(x.ge.boxxlo.and.x.le.boxxhi.and.
     :      y.ge.boxylo.and.y.le.boxyhi) then
*
* CONVERT SCREEN CO-ORDS TO DATA VALUES
*
            call poscon(x,y,boxxlo,boxxhi,boxylo,boxyhi,arcmin,pixel,
     :                  detdat,ilin,trace,xlim,samplo,samphi,
     :                  unsmax,unsmin,offset,dets,ndets,ndtout,
     :                  data,npix,nlin,inval,scndir,.true.,ierr)

            if(ierr.eq.0) then
*
* POSITION CURSOR ON THE TRACE AT THE PIXEL MEASURED
*
               call setcup(x,y,ierr)
               if(ierr.ne.0) goto 999
*
* CONVERT UNSCALED DATA VALUE TO SCALED DATA VALUE IN JANSKIES
*
               jansky=scale*detdat+zero
*
* DISPLAY FINAL X AND Y VALUES ON GRAPHICS DEVICE
*
               call sgs_clrbl(midx,midx+8*textht,0.0,txttop+textht)
               call sgs_tx(midx,txttop,'#')
               call sgs_atxi(ballno(dets(trace)),0)
               call sgs_txr(midx,txttop-(textht+txtgap),jansky,0,2)
               call sgs_txr(midx,txttop-2*(textht+txtgap),arcmin,0,2)
               call sgs_flush
*
* WRITE VALUES TO LOG FILE IF REQUIRED
*
               if(lgunit.ne.0) then
                  txtbuf=' '
                  write(txtbuf,10) ballno(dets(trace))
  10              format(' Data value:    Detector #',I2)
                  call lbgone(txtbuf(27:))
                  write(txtbuf(29:),20) jansky
  20              format('    value=',G13.6)
                  call lbgone(txtbuf(39:))
                  write(txtbuf(52:),30) arcmin
  30              format(' position=',f7.2)
                  call lbgone(txtbuf(65:))
                  lgunit=abs(lgunit)
                  write(lgunit,*) txtbuf
               endif
            else
               call sgs_clrbl(midx,midx+8*textht,0.0,txttop+textht)
            endif
*
* IF USER GAVE A POSITION OUTSIDE THE PLOTTING BOX, THEN CLEAR TEXT AND
* RETURN
*
         else
            call sgs_clrbl(boxxlo,midx+8*textht,0.0,txttop+textht)
            midx=0.5*(boxxlo+boxxhi)
            call sgs_clrbl(midx-17*textht,midx+17*textht,
     :                     0.14+textht,0.14-2*textht-txtgap)
            call sgs_flush
            exit=.true.
        endif
*
* LOOP FOR ANOTHER CURSOR POSITION
*
      enddo
*
* WRITE THE LAST DATA VALUE TO THE OUTPUT PARAMETER, DATA
*
      call putpar('DATA','REAL',ival,jansky,cval,ierr)
      if(ierr.ne.0) call wrerr('NODATPAR')
*
* FINISH
*
 999  continue

      end
