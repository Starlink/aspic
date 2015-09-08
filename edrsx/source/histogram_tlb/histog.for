      subroutine histog
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To produce a plot of the histogram of data values stored in an
*       EDRS image.
*
*SOURCE
*       HISTOG.FOR IN HISTOGRAM.TLB
*
*METHOD
*       Aquire image and all input parameters. The histogram data is
*       obtained from routine PCHIST as a by-product of calculating
*       the auto-scaling data range for the X axis. Bin the histogram
*       and calculate the co-ordinates of the histogram vertices.
*       Open NCAR and select the required GKS font. Draw the AUTOGRAPH
*       background and finally draw the histogram.
*             An optional output image is produced which holds the full
*       histogram of integer values with bin size of 1.
*
*ARGUMENTS
*       NONE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir,wrerr,ncropn,gtfont,ncrbck,gtbool
*       THIS PACKAGE (HISTOGRAM.TLB):
*              wrhist
*       EDRS:
*              gtdscr,rngerr,pchist,getpar,lbgone
*       INTERIM:
*              rdkeyl,rdkeyr,rdkeyc,cnpar,frdata
*       SGS:
*              sgs_sfont,sgs_close
*       AUTOGRAPH:
*              agcurv
*
*STARLINK PARAMETERS
*       INPUT/read/     Input image
*       PCRANGE/read/   The percentage histogram points to use for auto
*                       scaling.
*       DRANGE/read/    The data values of the ends of the plot X axis.
*       BINSIZE/read/   The size of each histogram bin in data units.
*       YLIMS/read/     The limits of the Y axis in data value counts.
*       TITLE/read/     The title for the top of the plot.
*       XTITLE/read/    The title for the bottom of the plot.
*       YTITLE/read/    The title for the left of the plot.
*       DEVICE/read/    The SGS name of the graphics device
*       FONT/read/      A valid GKS font number to use for text.
*       OUTPUT/read/    The name of an output image for full histogram
*       CUMULATE/read/  If YES then cumulative histogram is produced
*       BADVALUE/error/ Accessed if a bad value is given for PCRANGE
*       NODATA/error/   Accessed if the plot would contain no data
*       NOVALID/error/  Accessed if image contains no valid pixels.
*       ZEROSCA/error/  Accessed if the scale factor of the image is zero
*
*WRITTEN BY
*       D.S. Berry (MAVAD::DSB) 23/3/88
*-----------------------------------------------------------------------
*
*
      parameter (minint=-32768,maxint=32767,maxbin=400)

      character cval*1,title*30,ptitle*80,device*30,xtitle*80,ytitle*80
      real      pcrange(2),drange(2),x(2*maxbin),y(2*maxbin),ylim(2)
      integer   ihist(minint:maxint),ilim(2),ustrln
      logical   cumul

*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierrin)

      if(ierrin.eq.0) then

*
* INPUT IMAGE OBTAINED SUCCESSFULLY... EXTRACT REQUIRED DESCRIPTOR
* ITEMS
*
         title=' '
         inval=-100000
         scale=1.0
         zero=0.0
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title,ierr)
         call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
         call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)


*
* OBTAIN THE UPPER AND LOWER HISTOGRAM POINTS FOR AUTO-SCALING
*

         pcrange(1)=5.0
         pcrange(2)=95.0
 67      call rdkeyr('PCRANGE',.true.,2,pcrange,nval,ierr)

*
* IF THE VALUES LIE OUTSIDE 0 TO 100 PERCENT, CANCEL THEM,
* GIVE ERROR MESSAGE AND GET NEW VALUES
*

         if(max(pcrange(1),pcrange(2)).gt.100.0.or.min(pcrange(1)
     :     ,pcrange(2)).lt.0.0) then
            call wrerr('BADVALUE')
            call rngerr('***REAL VALUES','REAL',0.0,100.0)
            call cnpar('PCRANGE',ierr)
            go to 67
         endif

         pcrange(1)=pcrange(1)*0.01
         pcrange(2)=pcrange(2)*0.01

*
* CALL PCHIST TO FIND THE CORRESPONDING INTEGER VALUES
* AND TO OBTAIN THE HISTOGRAM
*
         ilim(1)=-32768
         ilim(2)=32767
         call pchist(%val(ipin),npix,nlines,inval,pcrange,ilim,2
     :       ,ihist,minint,maxint,ierr)
         if(ierr.ne.0) then
            call wrerr('NOVALID')
            goto 999
         endif
*
* CONVERT TO DATA VALUES
*
         drange(1)=ilim(1)*scale+zero
         drange(2)=ilim(2)*scale+zero
*
* OBTAIN MINIMUM AND MAXIMUM DATA VALUES FOR INPUT IMAGE AND CONVERT TO
* INTEGER VALUES
*
         call rdkeyr('DRANGE',.true.,2,drange,nval,ierr)
         if(scale.ne.0) then
            ilim(1)=nint((drange(1)-zero)/scale)
            ilim(2)=nint((drange(2)-zero)/scale)
         else
            call wrerr('ZEROSCA')
            goto 999
         endif
*
* CALCULATE SIZE OF A BIN
*
         sizmin=max(scale,(drange(2)-drange(1))/maxbin)
         sizmax=(drange(2)-drange(1))*0.5
         binsiz=(drange(2)-drange(1))/150.0
         call getpar('BINSIZE','REAL',1,sizmin,sizmax,.true.,ival,binsiz
     :               ,ierr)
         if(ierr.ne.0) goto 999
         idelt=max(1,nint(binsiz/scale))
         binsiz=idelt*scale
         nbin=min(maxbin,int((drange(2)-drange(1))/binsiz))
*
* SEE IF NORMAL OR CUMULATIVE HISTOGRAM IS REQUIRED, AND ACCUMULATE
* THE HISTOGRAM IF REQUIRED
*
         cumul=.false.
         call gtbool('CUMULATE',.true.,cumul,ierr)
         if(cumul) then
            do i=minint+1,maxint
               ihist(i)=ihist(i)+ihist(i-1)
            enddo
         endif
*
* BIN HISTOGRAM FOR DISPLAY
*
         ilow=ilim(1)
         ylim(1)=0
         ylim(2)=0
         do ibin=1,nbin
            isum=0
            do i=ilow,ilow+idelt-1
               if(i.le.maxint.and.i.ge.minint) isum=isum+ihist(i)
            enddo
            if(isum.gt.0) then
               y(2*ibin-1)=isum
               y(2*ibin)=isum
               ylim(2)=max(ylim(2),real(isum))
            else
               y(2*ibin-1)=0
               y(2*ibin)=0
            endif
            x(2*ibin-1)=scale*ilow+zero
            x(2*ibin)=scale*(ilow+idelt-1)+zero
            ilow=ilow+idelt
         enddo
*
* IF NO DATA PIXELS HAVE VALUES WITHIN THE RANGE OF THE X AXIS, THEN
* GIVE A MESSAGE AND QUIT
*
         if(ylim(2).eq.0) then
            call wrerr('NODATA')
            goto 999
         endif
*
* GET Y AXIS LIMITS
*
         ylim(2)=1.1*ylim(2)
         call rdkeyr('YLIMS',.true.,2,ylim,nval,ierr)
*
* CONSTRUCT THE DEFAULT TITLES FOR THE DISPLAY
*
         write(ptitle,10) binsiz
   10    format(' (Bin size=',g10.3,')')
         call lbgone(ptitle(18:))
         call lbgone(ptitle(12:))
         ptitle=title(:ustrln(title))//ptitle
         xtitle='Image data value'
         if(cumul) then
            ytitle='Cumulative data frequency'
         else
            ytitle='Data value frequency'
         endif
*
* GET TITLES FROM USER
*
         call rdkeyc('TITLE',.true.,1,ptitle,nval,ierr)
         call rdkeyc('XTITLE',.true.,1,xtitle,nval,ierr)
         call rdkeyc('YTITLE',.true.,1,ytitle,nval,ierr)
*
* OPEN NCAR
*
         device=' '
         call ncropn('DEVICE',device,.true.,ierr)
         if(ierr.ne.0) goto 999
         if(device.ne.'NONE') then
*
* GET REQUIRED FONT FROM USER AND SET IT
*
            call gtfont('FONT',ifont)
            call sgs_sfont(ifont)
*
* DRAW HISTOGRAM USING NCAR. FIRST DRAW THE BCAKGROUND.
*
            call ncrbck(drange(1),drange(2),ylim(1),ylim(2),ptitle,
     :                  xtitle,ytitle)
*
* NOW DRAW THE CURVE
*
            call agcurv(x,1,y,1,2*nbin,1)
*
* CLOSE NCAR
*
            call sgs_close
         endif
*
* IF REQUIRED, STORE HISTOGRAM AS A BDF FRAME
*
         npixo=maxint-minint+1
         call gt2diw('OUTPUT',102,.true.,npixo,1,ipo,ierr)
         if(ierr.eq.0) then
            call wrhist(ihist,maxint,minint,bscale,bzero,%val(ipo))
            call cydscr('INPUT','OUTPUT',ierr)
            call ptdscr('OUTPUT','NAXIS1','INTEGER',npixo,rval,cval,
     :                   ierr)
            call ptdscr('OUTPUT','NAXIS2','INTEGER',1,rval,cval,ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',-100000,rval,cval,
     :                   ierr)
            call rdkeyc('TITLE',.true.,1,title,ival,ierr)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,
     :                   ierr)
            call ptdscr('OUTPUT','XSCALE','REAL',ival,scale,cval,ierr)
            xzero=scale*(minint-1)+zero
            call ptdscr('OUTPUT','XZERO','REAL',ival,xzero,cval,ierr)
         endif

      endif

 999  call frdata(' ',ierr)

      end
