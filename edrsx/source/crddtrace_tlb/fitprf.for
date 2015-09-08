      subroutine fitprf(boxxlo,boxxhi,boxylo,boxyhi,xlim,samplo,samphi,
     :                  unsmax,unsmin,offset,dets,ndets,ndtout,npix,
     :                  nlin,ballno,scale,zero,inval,scndir,avrges,
     :                  ipdata,refpix,smpsiz,band,usecus,lgunit)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Overlays a point source profile on the data at a position
*       determined either by the cursor or by input from the user.
*       The profile is overlayed on the data, and the strength and
*       position of the source are displayed and written to output
*       parameters.
*
*SOURCE
*       FITPRF.FOR in CRDDTRACE.TLB
*
*METHOD
*       It is assumed that the currently active SGS zone is the
*       base zone. The user is repeatedly asked to select features
*       until a position is given which is outside the plotting box.
*       A standard source profile is read in (in EDRS format), and
*       must have either 1 or 4 lines with an odd no. of pixels in each.
*       The size in arcmins of a profile pixel must be contained within
*       the title descriptor as 'SIZE=1.25 ARCMINS' for instance.
*               The final profile fitted has its peak at the position
*       of the cursor and is added to a linear background determined
*       from the data in the locality of the source.
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
*       npix,nlin       integer The dimensions of array data
*       ballno(ndets)   integer The Ball no.s of the detectors in
*                               the current band
*       scale           real    The scale factor for converting from
*                               unscaled to scaled data values
*       zero            real    The zero factor for converting from
*                               unscaled to scaled data values
*       inval           integer The value stored in invalid pixels
*       scndir        character Direction of scan north or south
*       avrges(ndets)   real    The 'average' value of each detector trace
*       ipdata          integer Pointer to the CRDD data
*       refpix          real    The fractional pixel location of field centre
*       smpsiz          real    The size of a pixel of CRDD data in arcmins
*       band            integer The band no. of the CRDD data (1-4)
*       usecus          logical True if cursor is being used for getting
*                               parameters.
*       lgunit          integer Fortran logical unit no. to which logged
*                               values are to be written. 0 implies no
*                               logging to be done. On entry a -ve value
*                               implies that no data has yet been logged
*
*   OUTPUTS:
*       lgunit          integer On exit, a +ve value indicates that some
*                               data has been logged.
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               putpar,wrerr,maxmin,fstlst
*       THIS PACKAGE (CRDDTRACE.TLB):
*               poscon,proplt,linbak
*       SGS:
*               sgs_icuav,sgs_reqcu,sgs_shtx,sgs_sartx,
*               sgs_stxj,sgs_tx,sgs_clrbl,sgs_flush,sgs_atxi,sgs_txr
*       EDRS:
*               getpar,gt2dir,gtdscr,lbgone
*       INTERIM:
*               ctor
*
*STARLINK PARAMETERS
*       SRCSIZE/read/   The users first guess of the source strength
*       SRCPOSN/read/   The users first guess of the source position
*       SRCDET/read/    The detector Ball no of the trace with the source
*       PROFILE/read/   The BDF file containing the source profiles
*       STRNTH/write/   The strength of the final source in Jy
*       POSN/write/     The position of the final source in arcmins
*       BADDET/error/   Accessed if SRCDET value is invalid
*       PROFDIM/error/  Accessed if the dimensions of PROFILE are wrong
*       PROFPIX/error/  Accessed if the pixel size in PROFILE is invalid
*       PROFSIZE/error/ Accessed if there is an even no. of pixels in
*                       each line of PROFILE
*       PROFINV/error/  Accessed if PROFILE contains no valid pixels
*       NOSTRN/error/   Accessed if STRNTH not successfully written
*       NOPOSN/error/   Accessed if POSN not successfully written
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       %val
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
      integer   ipdata,band,lgunit
      real      boxxlo,boxxhi,boxylo,boxyhi,xlim(2),avrges(ndets)
      real      samplo,samphi,unsmax,unsmin,offset(ndets),scale,zero
      real      refpix,smpsiz
      character scndir*(*)
      logical   usecus
*
* DECLARE LOCAL VARIABLES
*
      real      arcmin  ! The cursor x position in arcmins north of
                        ! field centre
      real      const   ! The constant term in the linear background
      logical   cursor  ! True if cursor exists on current device
      character cval*1  ! Dummy character argument for putpar
      logical   exit    ! True when no more positions are required
      logical   first   ! A first time flag
      integer   fstpix  ! The 1st non-zero pixel in the profile
      integer   ierr    ! Error status from called routines
      integer   ilin    ! The line of CRDD data being fitted
      integer   ipprof  ! Pointer to basic unscaled source profile, max
                        ! scaled value must be 1
      integer   ival    ! Dummy integer argument for putx
      integer   lprof   ! The length of profile array. Must be odd
      integer   lstpix  ! The last non-zero pixel in the profile
      real      midx    ! X position of output numbers
      integer   n       ! The cursor choice key identifier
      real*8    par(4)  ! The fitting parameters
      integer   pinval  ! Invalid profile pixel value
      integer   pixel   ! The cursor x position in pixels
      integer   plin    ! Profile line no. to use
      character prbuf*13! Buffer for formatted source strength
      integer   promax  ! The maximum unscaled profile value
      integer   promin  ! The minimum unscaled profile value
      integer   prouse  ! The length of the non-zero part of the
                        ! profile in units of data pixels
      real      pscale  ! Scale factor for prof
      real      psize   ! Size of one profile pixel in arcmins
      real      pzero   ! Zero factor for prof
      character ptitle*30       ! Profile title
      real      rval    ! Dummy real argument
      real      slope   ! Slope of the linear background at source
      integer   sprof   ! No. of lines in profile image
      real      strnth  ! Source strength in Janskies without backgrnd
      real      textht  ! SGS text height in base zone co-ords
      integer   trace   ! The trace on which the feature lays
      character txtbuf*80 ! Buffer for log text
      real      txtgap  ! Size of gap between lines of text
      real      txttop  ! Y position of centre of top line of text
      real      unsdat  ! A buffer for an input data value
      integer   userdt  ! Detector for fitting given by user
      real      userst  ! Value of strength given by user if no cursor
      real      userpn  ! Value of position given by user if no cursor
      real      x       ! The cursor x position in base zone co-ords
      real      y       ! The cursor y position in base zone co-ords
*
* SEE IF CURSOR EXISTS.
*
      call sgs_icuav(cursor)
*
* IF NOT, ASK USER FOR SOURCE POSITION,STRENGTH AND DETECTOR
*
      if(.not.cursor) then
         call getpar('SRCSIZE','REAL',1,0.0,1.0e20,.false.,ival,
     :               userst,ierr)
         if(ierr.ne.0) goto 999
         call cnpar('SRCSIZE',ierr)
         call getpar('SRCPOSN','REAL',1,xlim(1),xlim(2),.false.,ival,
     :               userpn,ierr)
         if(ierr.ne.0) goto 999
         call cnpar('SRCPOSN',ierr)
         call getpar('SRCDET','INTEGER',1,0.0,100.0,.false.,userdt,
     :               rval,ierr)
         if(ierr.ne.0) goto 999
         call cnpar('SRCDET',ierr)
*
* CHECK DETECTOR GIVEN IS VALID
*
         first=.true.
         do ival=1,ndtout
            if(ballno(dets(ival)).eq.userdt.and.first) then
               userdt=dets(ival)
               trace=ival
               first=.false.
            endif
         enddo
         if(first) then
            call wrerr('BADDET')
            goto 999
         endif
*
* CONVERT POSITION IN ARCMINS TO PIXEL POSITION
*
         if(scndir.eq.'NORTH') then
            pixel=nint(samplo+(samphi-samplo)*(userpn-xlim(1))/
     :            (xlim(2)-xlim(1)))
         else
            pixel=nint(samphi-(samphi-samplo)*(userpn-xlim(1))/
     :            (xlim(2)-xlim(1)))
         endif
      endif
*
* GET PROFILE IMAGE AND CHECK DIMENSIONS ARE OK
*
      call gt2dir('PROFILE',102,.false.,lprof,sprof,ipprof,ierr)
      if(ierr.ne.0) then
         goto 999
      else if(sprof.ne.4.and.sprof.ne.1) then
         call wrerr('PROFDIM')
         goto 999
      else if(mod(lprof,2).ne.1) then
         call wrerr('PROFSIZE')
         goto 999
*
* IF DIMENSIONS ARE OK, GET EDRS SPECIFIC DESCRIPTORS
*
      else
         pinval=-100000
         pscale=1.0
         pzero=0.0
         ptitle=' '
         call gtdscr('PROFILE','INVAL','INTEGER',pinval,rval,cval,ierr)
         call gtdscr('PROFILE','TITLE','CHARACTER',ival,rval,ptitle,
     :               ierr)
*
* EXTRACT PROFILE PIXEL SIZE FROM TITLE DESCRIPTOR AND CHECK ITS OK
*
         ival=max(index(ptitle,'size='),index(ptitle,'SIZE='))
         if(ival.ne.0) call ctor(ptitle(ival+5:),psize,ierr)
         if(ival.eq.0.or.ierr.ne.0) then
            call wrerr('PROFPIX')
            goto 999
         endif
      endif
*
* SELECT WHICH LINE OF PROFILE IMAGE IS TO BE USED
*
      if(sprof.eq.1) then
         plin=1
      else
         plin=band
      endif
*
* MAKE SURE MAX PROFILE VALUE IS 1 AND MIN IS 0
*
      call maxmin(%val(ipprof),lprof,sprof,pinval,promax,promin,ierr)
      if(ierr.ne.0) then
         call wrerr('PROFINV')
         goto 999
      endif
      pscale=1.0/(promax-promin)
      pzero=-promin*pscale
*
* GET THE EXTENT OF THE NON-ZERO PART OF THE PROFILE IN PIXELS
*
      call fstlst(%val(ipprof),lprof,sprof,pinval,pscale,pzero,fstpix,
     :            lstpix,plin,ierr)
*
* CONVERT TO THE EQUIVALENT NO. OF DATA PIXELS
*
      prouse=(lstpix-fstpix+1)*psize/smpsiz
*
* DISPLAY HELPFUL MESSAGES
*
      textht=0.014
      txttop=0.09
      txtgap=0.009
      midx=boxxlo+15*textht
      call sgs_shtx(textht)
      call sgs_sartx(0.6667)
      call sgs_stxj('CC')
      if(cursor) then
         call sgs_tx(0.5*(boxxlo+boxxhi),0.14,
     :               'Position cursor at source peak and press any key')
         call sgs_tx(0.5*(boxxlo+boxxhi),0.14-textht-txtgap,
     :               '(position cursor ouside data box to quit)')
      endif
      call sgs_stxj('CL')
      call sgs_tx(boxxlo,txttop,'Detector')
      call sgs_tx(boxxlo,txttop-(textht+txtgap),'Source strength')
      call sgs_tx(boxxlo,txttop-2*(textht+txtgap),'X position')
      call sgs_tx(midx-textht,txttop,':')
      call sgs_tx(midx-textht,txttop-(textht+txtgap),':')
      call sgs_tx(midx-textht,txttop-2*(textht+txtgap),':')
      call sgs_flush
*
* LOOP UNTIL EXIT IS REQUESTED
*
      exit=.false.
      do while(.not.exit)
         first=.true.
*
* DISPLAY CURSOR AND GET A POSITION IN BASE ZONE SGS WORLD CO-ORDS
*
         if(cursor) then
            call sgs_reqcu(x,y,n)
         else
            x=boxxlo
            y=boxylo
         endif
*
* IF INSIDE THE PLOTTING BOX THEN ....
*
         if(x.ge.boxxlo.and.x.le.boxxhi.and.
     :      y.ge.boxylo.and.y.le.boxyhi) then
*
* IF CURSOR IS BEING USED CONVERT SCREEN POSITION TO USEFUL UNITS
*
            if(cursor) then
               call poscon(x,y,boxxlo,boxxhi,boxylo,boxyhi,arcmin,pixel,
     :                     unsdat,ilin,trace,xlim,samplo,samphi,
     :                     unsmax,unsmin,offset,dets,ndets,ndtout,
     :                     %val(ipdata),npix,nlin,inval,scndir,.false.,
     :                     ierr)
*
* OTHERWISE USE VALUES GIVEN BY USER
*
            else
               ilin=userdt
               arcmin=userpn
               ierr=0
            endif
            if(ierr.eq.0) then
*
* DETERMINE A LINEAR BACKGROUND UNDERNEATH THE SOURCE
*
               call linbak(%val(ipdata),npix,nlin,ilin,smpsiz,prouse,
     :                     inval,pixel,slope,const,ierr)
*
* CALCULATE SET OF 4 PARAMETERS TO DESCRIBE THE SOURCE
*
* 1: SOURCE STRENGTH IN JANSKIES
*
               if(cursor) then
                  par(1)=scale*(unsdat-const)
               else
                  par(1)=userst
               endif
               if(par(1).lt.0) par(1)=0

*
* 2: POSITION IN ARCMINS SCANWARDS OF FIELD CENTRE
*
               if(scndir.eq.'NORTH') then
                  par(2)=arcmin
               else
                  par(2)=-arcmin
               endif
*
* 3: SLOPE OF BACKGROUND IN JANKSIES PER ARCMIN
*
               par(3)=scale*slope
*
* 4: CONSTANT TERM IN BACKGROUND IN JANKIES
*
               par(4)=scale*const+zero
*
* DISPLAY PROFILE OVER THE DATA
*
               call proplt(par,4,%val(ipprof),lprof,pscale,pzero,
     :                     psize,pinval,boxxlo,boxxhi,boxylo,boxyhi,
     :                     xlim,scndir,offset,ndets,unsmin,unsmax,
     :                     trace,scale,zero,plin,sprof,x,y)
*
* DISPLAY FINAL FIT PARAMETERS ON GRAPHICS DEVICE
*
               strnth=par(1)
               if(scndir.eq.'NORTH') then
                  arcmin=par(2)
               else
                  arcmin=-par(2)
               endif
               call sgs_clrbl(midx,midx+13*textht,0.0,txttop+textht)
               call sgs_tx(midx,txttop,'#')
               call sgs_atxi(ballno(ilin),0)
               write(prbuf,'(G13.6)') strnth
               call sgs_tx(midx,txttop-(textht+txtgap),prbuf)
               call sgs_txr(midx,txttop-2*(textht+txtgap),arcmin,0,2)
               call sgs_flush
            endif
*
* IF REQUIRED, LOG THESE VALUES
*
            if(lgunit.ne.0) then
               write(txtbuf,10) ballno(ilin)
  10           format(' Point source:  Detector #',I2)
               call lbgone(txtbuf(27:))
               write(txtbuf(29:),20) strnth
  20           format('    strength=',G13.6)
               call lbgone(txtbuf(42:))
               write(txtbuf(55:),30) arcmin
  30           format('    position=',f7.2)
               call lbgone(txtbuf(68:))
               lgunit=abs(lgunit)
               write(lgunit,*) txtbuf
            endif
*
* IF CURSOR IS NOT BEING USED, QUIT AFTER 1ST PROFILE
*
            if(.not.cursor) exit=.true.
*
* IF USER GAVE A POSITION OUTSIDE THE PLOTTING BOX, THEN CLEAR TEXT AND
* RETURN
*
         else
            call sgs_clrbl(boxxlo,midx+13*textht,0.0,txttop+textht)
            midx=0.5*(boxxlo+boxxhi)
            call sgs_clrbl(0.0,1.0,0.14+textht,0.14-2*textht-txtgap)
            call sgs_flush
            exit=.true.
        endif
*
* LOOP FOR ANOTHER CURSOR POSITION
*
      enddo
*
* WRITE THE LAST SOURCE STRENGTH AND POSITION TO THE OUTPUT PARAMETERS
* 'STRENGTH' AND 'POSITION'
*
      call putpar('STRNTH','REAL',ival,strnth,cval,ierr)
      if(ierr.ne.0) call wrerr('NOSTRN')
      call putpar('POSN','REAL',ival,arcmin,cval,ierr)
      if(ierr.ne.0) call wrerr('NOPOSN')
*
* FINISH
*
 999  continue

      end
