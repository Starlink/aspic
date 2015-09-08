      subroutine argpic
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO RESCALE AN IMAGE TO MAKE IT SUITABLE FOR DISPLAY
*       AND TO PLOT IT ANY GKS 7.2 IMAGING DEVICE
*
*METHOD
*       OBTAIN INPUT IMAGE AND EXTRACT DESCRIPTOR ITEMS. OBTAIN
*       PERCENTAGE HISTOGRAM POINTS FOR AUTOSCALING AND CALL PCHIST
*       TO DETERMINE THE DATA RANGE TO BE DISPLAYED. OBTAIN DATA RANGE
*       AND MAXIMUM IMAGE BRIGHTNESS FROM ENVIRONMENT AND RESCALE THE 
*	IMAGE TO LIE IN THE RANGE 0-MAX BRIGHTNESS USING DSPSCL. IF THE
*	IMAGE IS TO BE DISPLAYED ON THE ARGS, THEN USE ARGSLIB TO PLOT 
*	THE IMAGE ON THE ARGS AND UPDATE THE ARGS DATABASE. IF IT IS TO
*	BE DISPLAYED ON ANY OTHER DEVICE, USE GKS TO PLOT THE IMAGE AND
*	UPDATE THE AGI GRAPHICS DATABASE. IF REQUIRED COPY THE RESCALED 
*	IMAGE TO AN OUTPUT IMAGE FRAME. OUTLINE BOXES ARE ALLOWED ONLY
*	ON ARGS OR IKONS.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       INPUT
*               THE INPUT IMAGE
*       AUTO
*               LOGICAL PARAMETER SPECIFYING IF AUTOSCALING IS TO BE
*               PERFORMED
*       PCRANGE
*               PERCENTAGE HISTOGRAM POINTS CORRESPONDING TO THE
*               REQUIRED INPUT DATA RANGE TO BE USED FOR AUTOSCALING
*       DRANGE
*               THE INPUT DATA RANGE TO BE DISPLAYED
*	URANGE 
*		THE INPUT DATA RANGE ACTUALLY USED (AN OUTPUT PARAMETER)
*	DEVICE
*		THE SGS NAME OF THE DEVICE ON WHICH IMAGE IS TO BE
*		DISPLAYED
*	BRIGHT
*		THE BRIGHTNESS CORRESPONDING TO THE UPPER DATA RANGE
*		LIMIT
*	BOX
*		IF TRUE, THEN A COLOURED BOX IS DRAWN ROUND THE IMAGE
*	COLOUR
*		THE COLOUR FOR THE OUTLINE BOX
*       XC
*               THE X COORDINATE OF THE ARGS SCREEN ON WHICH THE IMAGE
*               IS TO BE CENTRED
*       YC
*               THE Y COORDINATE OF THE ARGS SCREEN ON WHICH THE IMAGE
*               IS TO BE CENTRED
*       NONEVAL/ERROR
*               ACCESSED IF THE INPUT IMAGE HAS NO VALID PIXELS
*       NOSPACE/ERROR
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       NOARGS/ERROR
*               ACCESSED IF THE ARGS IS NOT AVAILABLE
*       NODB/ERROR
*               ACCESSED IF THE ARGS DATABASE CANNOT BE UPDATED
*	NODEVICE/ERROR
*		ACCESSED IF SPECIFIED DEVICE CANNOT BE OPENED
*	NOTRAST/ERROR
*		ACCESSED IF SPECIFIED DEVICE IS NOT A RASTER DISPLAY
*       OUTPUT
*               THE OUTPUT IMAGE
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*       BADVALUE/ERROR
*               ACCESSED IF INVALID PERCENTAGE POINTS ARE GIVEN
*
*CALLS
*       THIS PACKAGE:
*               GT2DIR,GTDSCR,GETPAR,GT2DIW,DSPSCL,PTDSCR,RNGERR,
*               PCHIST,IMGCPY,GETDEV,GETCMD,I2TOI4,IKON_OVOP,GTINAM,
*		IKON_OVCL,DEFDEV
*       STARLINK:
*               CYDSCR,RDKEYC,FRDATA,RDKEYR,RDKEYI,WRERR,CNPAR,
*               GETDYN,RDKEYL
*       ARGS LIBRARY:
*               SRINIT,SRPXI2,SRSEND
*	ROE ARGS LIBRARY:
*		ARGS_OVCOL,ARGS_DSPALL,ARGS_OVCG,ARGS_OVWRT,ARGS_ALLWRT
*       ARGS DATABASE:
*               ARGS_WRIM,ARGS_NUMIM,ARGS_POLYL
*	AGI DATABASE:
*		AGI_PNEW,AGS_SZONE
*	SGS:
*		SGS_WIDEN,SGS_CLRFG,SGS_OPEN,SGS_ICURW,SGS_SW,SGS_CLOSE
*	GKS:
*		GQMDS,GQECI,GCA,GPL,GQWKCL
*
*NOTES
*       USES VAX %VAL FACILITY
*       USES ARGS AND AGI DATABASES
*
*WRITTEN BY
*       R.F. WARREN-SMITH (modified by D.S. Berry to use any GKS device)
*-----------------------------------------------------------------------
*
*
      logical auto(1),box
      character cval*1,title(1)*30,colour*7,bdfnam*40,wkname*15
      character utrn(6)*5,device*30
      real pcrange(2),drange(2),x(5),y(5),devc(4),ndc(4),wc(4)
      
 
*
* DIMENSION ARRAY FOR HISTOGRAM FOR USE IN AUTOSCALING THE IMAGE
*
      parameter (minint=-32768,maxint=32767)
      integer ihist(minint:maxint)
      integer ilim(2)
      integer*2 idummy(1)
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierrin)
 
      if(ierrin.eq.0) then
 
*
* INPUT IMAGE OBTAINED SUCCESSFULLY... EXTRACT REQUIRED DESCRIPTOR
* ITEMS
*
         title(1)=' '
         inval=-100000
         scale=1.0
         zero=0.0
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
         call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
 
*
* DETERMINE IF AUTOSCALING IS REQUIRED
*
         auto(1)=.true.
         call rdkeyl('AUTO',.true.,1,auto,nval,istat)
 
*
* SET DEFAULT INPUT DATA RANGE
*
         drange(1)=scale*(-32768.0)+zero
         drange(2)=scale*32767.0+zero
 
*
* IF AUTOSCALING IS REQUIRED, OBTAIN THE UPPER AND LOWER HISTOGRAM
* POINTS
*
 
         if(auto(1)) then
            pcrange(1)=5.0
            pcrange(2)=95.0
67          call rdkeyr('PCRANGE',.true.,2,pcrange,nval,istat)
 
*
* IF THE VALUES LIE OUTSIDE 0 TO 100 PERCENT, CANCEL THEM,
* GIVE ERROR MESSAGE AND GET NEW VALUES
*
 
            if(max(pcrange(1),pcrange(2)).gt.100.0.or.min(pcrange(1)
     :       ,pcrange(2)).lt.0.0) then
               call wrerr('BADVALUE')
               call rngerr('***REAL VALUES','REAL',0.0,100.0)
               call cnpar('PCRANGE',istat)
               go to 67
 
            endif
 
            pcrange(1)=pcrange(1)*0.01
            pcrange(2)=pcrange(2)*0.01
 
*
* CALL PCHIST TO FIND THE CORRESPONDING INTEGER VALUES
*
            ilim(1)=-32768
            ilim(2)=32767
            call pchist(%val(ipin),npix,nlines,inval,pcrange,ilim,2
     :       ,ihist,minint,maxint,ierr)
 
            if(ierr.ne.0) call wrerr('NONEVAL')
 
*
* CONVERT TO DATA VALUES
*
            drange(1)=ilim(1)*scale+zero
            drange(2)=ilim(2)*scale+zero
         endif
 
 
*
* OBTAIN MINIMUM AND MAXIMUM DATA VALUES FOR INPUT IMAGE
*
         call rdkeyr('DRANGE',.true.,2,drange,nval,istat)

*
* WRITE THE USED DATA RANGE OUT TO THE ENVIRONMENT FOR USE IN
* LATER RUNS OF ARGPIC
*
         call wrkeyr('URANGE',drange,2,istat) 

*
* OBTAIN WORKSPACE FOR SCALED IMAGE
*
         call getdyn('WRK',102,npix*nlines,ipwrk,istwrk)
 
*
* IF SPACE WAS NOT AVAILABLE, ABORT WITH ERROR MESSAGE
*
         if(istwrk.ne.0) then
            call wrerr('NOSPACE')
            go to 99
 
         endif

*
* SEE IF A BOX IS REQUIRED ROUND THE IMAGE. IF SO, GET REQUIRED COLOUR
*
         box=.false.
         call rdkeyl('BOX',.true.,1,box,nval,istat)
         if(box) then
            icol=3
            call getcmd('COLOUR','RED,GREEN,BLUE,YELLOW,CYAN,'//
     :                   'MAGENTA,BLACK,WHITE.',1,icol,colour,lcol,
     :                   ierr)
         endif

*
* OBTAIN DEVICE ON WHICH IMAGE IS TO BE DISPLAYED
*
         call defdev(device)
         call getdev('DEVICE',device,.false.,istat)
         if(istat.ne.0) goto 99
         call sgs_widen(device,itype,iconid,istat)
         if(istat.ne.0) goto 99

*
*-------------------------------------------------------------------
*
* **********   ARGS SPECIFIC SECTION  *************
*
*
* IF THE IMAGE IS TO BE DISPLAYED ON AN ARGS, USE THE CODE FROM THE
* ORIGINAL VERSION OF ARGPIC WHICH USES ARGSLIB. THIS IS DONE TO 
* MAINTAIN COMPLETE COMPATIBILITY WITH ASPIC WHEN USING THE ARGS.
*
         if(itype.eq.160) then

*
* OBTAIN CENTRE COORDINATES ON ARGS SCREEN
*
            ixc=256
            iyc=256
            call getpar('XC','INTEGER',1,0.0,511.0,.true.,ixc,rval,ierr)
            call getpar('YC','INTEGER',1,0.0,511.0,.true.,iyc,rval,ierr)
 
*
* GET MAXIMUM BRIGHTNESS TO USE FROM USER
*
            maxbr=255
            call getpar('BRIGHT','INTEGER',1,0.0,255.0,.true.,maxbr,
     :                   rval,ierr)

*
* CALL DSPSCL TO RESCALE THE IMAGE INTO THE RANGE 0-255
*
            call dspscl(%val(ipin),npix,nlines,inval,scale,zero,
     :                  drange(1),drange(2),0,maxbr,%val(ipwrk))
 
*
* INITIALLISE ARGS, QUITTING WITH AN ERROR MESSAGE IF NOT AVAILABLE
*
            call srinit(0,.false.,istat)
 
            if(istat.ne.0) then
               call wrerr('NOARGS')
               go to 99
 
            endif
 
 
*
* DETERMINE HOW MUCH IMAGE WILL FIT ON SCREEN
*
            nx=min(npix,512)
            ny=min(nlines,512)
 
*
* PLOT IMAGE ON ARGS SCREEN
*
            call srpxi2(%val(ipwrk),npix,nx,ny,ixc-nx/2,iyc-ny/2,16,
     :                  .false.,idummy,1)
 
*
* UPDATE ARGS DATABASE
*
            call args_wrim(ixc,iyc,nx,ny,nx,ny,istat)
 
            if(istat.ne.0) call wrerr('NODB')
            utrn(1)='1.0'
            utrn(2)='0.0'
            utrn(3)='0.0'
            utrn(4)='1.0'
            utrn(5)='1.0'
            utrn(6)='1.0'
            call args_wrpar('PTOU',utrn,6,istat)


*
* IF REQUIRED DRAW BOX AROUND IMAGE IN ARGS OVERLAY PLANE 8 WITHOUT 
* CLEARING OVERLAY FIRST
*
            if(box) then
               call args_numim(ilast)
               call args_ovcol(9,colour)
               call args_dspall(iswit)
               call args_ovcg('W')
               call args_ovwrt(9)
               x(1)=0
               x(2)=npix+1
               x(3)=npix+1
               x(4)=0
               x(5)=0
               y(1)=0
               y(2)=0
               y(3)=nlines+1
               y(4)=nlines+1
               y(5)=0
               call args_polyl(ilast,5,x,y,ierr)
               x(1)=-1
               x(2)=npix+2
               x(3)=npix+2
               x(4)=-1
               x(5)=-1
               y(1)=-1
               y(2)=-1
               y(3)=nlines+2
               y(4)=nlines+2
               y(5)=-1
               call args_polyl(ilast,5,x,y,ierr)
               call srsend
               call args_allwrt
            endif
*
*-----------------------------------------------------------------------
*
* **********   OTHER GKS DEVICES (NON-ARGS) SECTION  *************
*
*
* IF THE IMAGE IS TO BE DISPLAYED ON ANY GRAPHICS DEVICE OTHER THAN AN 
* ARGS, USE GKS TO DISPLAY IT AND UPDATE THE AGI GRAPHICS DATABASE
* INSTEAD OF THE ARGS DATABASE
*
         else
 
*
* OPEN IMAGE DEVICE USING SGS WITHOUT CLEARING THE DISPLAY OR CHANGING
* LOOK UP TABLE
*
            call sgs_init(6,ierr)
            call sgs_clrfg(1)
            call sgs_opnwk(device,izone,istat)            
            if(istat.ne.0) then
               call wrerr('NODEVICE')
               goto 99
            endif
*
* CHECK IT IS A RASTER DISPLAY
*
            call gqwkcl(itype,ierr,irv)
            if(ierr.ne.0.or.irv.ne.1) then
               call wrerr('NOTRAST')
               goto 99
            endif

*
* GET GKS WORKSTATION IDENTIFIER
*
            call sgs_icurw(iwkid)

*
* GET SIZE OF DISPLAY SCREEN IN PIXELS 
*
            call gqmds(itype,ierr,ival,rx,ry,ixsize,iysize)
            if(ierr.ne.0) goto 99

*
* OBTAIN CENTRE COORDINATES ON DEVICE SCREEN
*
            ixc=int(ixsize/2)+1
            iyc=int(iysize/2)+1
            call getpar('XC','INTEGER',1,1.0,real(ixsize),.true.,ixc,
     :                   rval,ierr)
            call getpar('YC','INTEGER',1,1.0,real(iysize),.true.,iyc,
     :                   rval,ierr)
 
*
* OBTAIN FRACTION OF DISPLAY TO BE COVERED BY THE IMAGE.
*
            frac = 0.75
            call getpar('FRACTION','REAL',1,0.01,1.0,.true.,ival,frac,
     :                   ierr)

*
* OBTAIN MAGNIFICATION REQUIRED WHEN DISPLAYING THE IMAGE.
*
            amagn = frac*min( real(ixsize)/real(npix),
     :                       real(iysize)/real(nlines) )
            call getpar('MAGN','REAL',1,0.01,1.0E6,.true.,ival,amagn,
     :                   ierr)

*
* GET NO. OF POSSIBLE GREY LEVELS ON CURRENT DEVICE
*
            call gqeci(iwkid,1,ierr,ngrey,ival)
            if(ierr.ne.0) goto 99 

*
* DETERMINE MAXIMUM IMAGE INTENSITY TO USE
*
            maxbr=ngrey-1
            call getpar('BRIGHT','INTEGER',1,0.0,real(ngrey-1),.true.,
     :                   maxbr,rval,ival)

*
* CALL DSPSCL TO RESCALE THE IMAGE INTO THE RANGE 0-MAXBR
*
            call dspscl(%val(ipin),npix,nlines,inval,scale,zero,
     :                  drange(1),drange(2),0,maxbr,%val(ipwrk))
 
*
* COPY THE I*2 SCALED IMAGE TO AN I*4 IMAGE FOR USE BY ROUTINE GCA
*
            npxw = nint( amagn*real(npix) )
            nlnw = nint( amagn*real(nlines) )
            call getdyn('WRK2',104,npxw*nlnw,ipwrk2,istwrk)
            if(istwrk.ne.0) then
               call wrerr('NOSPACE')
               go to 99
            endif
            call i2toi4(%val(ipwrk),npix,nlines,1.0,0.0,-1000,
     :                  %val(ipwrk2),npxw,nlnw,-1000,ierr)

*
* CALCULATE THE CORRECTED MAGNIFICATIONS
*
            xmagn = real( npxw )/real( npix )
            ymagn = real( nlnw )/real( nlines )
         
*
* SET UP WORLD CO-ORDS CORRESPONDING TO PIXELS COVERING ENTIRE 
* SCREEN, WITH (0,0) BEING BOTTOM LEFT CORNER OF THE IMAGE.
*
            xlo=(npxw*0.5-ixc+0.5)/xmagn
            xhi=xlo+ixsize/xmagn
            ylo=(nlnw*0.5-iyc+0.5)/ymagn
            yhi=ylo+iysize/ymagn
            call sgs_sw(xlo,xhi,ylo,yhi,ierr)
            if(ierr.ne.0) goto 99
 
*
* PLOT IMAGE ON DEVICE SCREEN (GKS BOOK GCA WRITE UP APPEARS TO BE WRONG
* THE 1ST PAIR OF ARGS ARE CO-ORDS OF TOP LEFT, 2ND PAIR BOTTOM RIGHT)
*
            call gca(0.0,real(nlines),real(npix),0.0,npxw,nlnw,npxw,
     :               %val(ipwrk2))

*
* GET NAME OF BDF FILE GIVEN AS INPUT TO STORE IN AGI GRAPHICS
* DATABASE
*
            call gtinam('INPUT',bdfnam,ierr)
            if(ierr.ne.0) then
               bdfnam='An image displayed by ARGPIC'
            endif

*
* CREATE AN ENTRY IN THE AGI GRAPHICS DATABASE FILE DESCRIBING THE
* IMAGE CO-ORDINATE SYSTEMS
*
            call ags_szone(wkname,devc,ndc,wc,ierr)
            call agi_pnew(wkname,'IMAGE',-1,bdfnam,devc,ndc,wc,
     :                    ipnum,ierr)
*
* CLOSE GRAPHICS ON MAIN SCREEN
*
            call sgs_close

*
* IF DEVICE IS AN IKON AND IF IT IS REQUIRED, OPEN IKON OVERLAY FOR 
* OUTLINE BOX WITHOUT CLEARING OVERLAYS
*
            if(box) then
               if(itype.eq.3200) then
                  call sgs_init(6,ierr)
                  call sgs_clrfg(1)
                  call sgs_opnwk('3201',izone,istat)
                  if(istat.ne.0) then
                     call wrerr('NODEVICE')
                     goto 99
                  endif

*
* SET UP WORLD CO-ORDS
*
                  xlo=npxw*0.5-ixc+0.5
                  xhi=xlo+ixsize
                  ylo=nlnw*0.5-iyc+0.5
                  yhi=ylo+iysize
                  call sgs_sw(xlo,xhi,ylo,yhi,ierr)
                  if(ierr.ne.0) goto 99

*
* OPEN OVERLAY PLANE 0 WITH SPECIFIED COLOUR
*
                  call ikon_ovop(1,colour)

*
* DRAW THE BOX
*
                  x(1)=-0.5
                  x(2)=-0.5
                  x(3)=real(npxw)+0.5
                  x(4)=real(npxw)+0.5
                  x(5)=-0.5
                  y(1)=-0.5
                  y(2)=real(nlnw)+0.5
                  y(3)=real(nlnw)+0.5
                  y(4)=-0.5
                  y(5)=-0.5
                  call gpl(5,x,y)

                  x(1)=-1.5
                  x(2)=-1.5
                  x(3)=real(npxw)+1.5
                  x(4)=real(npxw)+1.5
                  x(5)=-1.5
                  y(1)=-1.5
                  y(2)=real(nlnw)+1.5
                  y(3)=real(nlnw)+1.5
                  y(4)=-1.5
                  y(5)=-1.5
                  call gpl(5,x,y)

*
* CLOSE THE OVERLAYS
*
                  call ikon_ovcl(1,.false.)
                  call sgs_close
               endif
            endif

*
*-----------------------------------------------------------------------
* END OF DEVICE DEPENDANT SECTIONS
*
         endif

*
* OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.true.,npix,nlines,ipout,ierrou)
 
*
* IF AN OUTPUT FRAME WAS GIVEN, ADD DESCRIPTOR ITEMS
*
 
         if(ierrou.eq.0) then
            call cydscr('INPUT','OUTPUT',istat)
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',-100000,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,1.0,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,0.0,cval,ierr)
 
*
* COPY ARGS IMAGE TO OUTPUT
*
            call imgcpy(%val(ipwrk),npix,nlines,%val(ipout))
         endif
 
      endif
 
99    call frdata(' ',istat)
 
      end
