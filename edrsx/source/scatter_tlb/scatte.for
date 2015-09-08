      subroutine scatte
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Produces a scatter plot of pixel values in one image against
*	corresponding pixel values in another image.
*
*SOURCE
*       SCATTE.FOR in SCATTER.TLB
*
*METHOD
*	Aquire the input images and descriptors. Get the axis limits and
*	title from the user. Open NCAR. Plot the background. Plot the
*	scatter plot. Close NCAR. Release data areas and end.
*       
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir, gtinam, gtlims, ncropn, ncrbck,gtwork,gtfont
*       THIS PACKAGE (SCATTER.TLB):
*              scplot
*       EDRS:
*              gtstds,wrerr,pchist
*       INTERIM:
*              rdkeyc,frdata
*       SGS
*	       sgs_close,sgs_sfont
*
*STARLINK PARAMETERS
*	XIMAGE		Image to plot on X axis
*	YIMAGE		Image to plot on Y axis
*	XLIMS		Limits of X axis
*	YLIMS		Limits of Y axis
*	XTITLE		Title for X axis
*	YTITLE		Title for Y axis
*	TITLE		Title for plot
*	DEVICE		Graphics device to plot on
*	PCRANGE		Histogram points to use for autoscaling
*	FONT		SGS font number
*	NOVALID(error)	Accessed if an image contains no valid data
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/11/89
*-------------------------------------------------------------------
*
      implicit none
      
*
* DECLARE LOCAL VARIABLES
*
      character	device*30! Selected graphics device
      integer	ierr	! Error status
      integer	ifont	! SGS font number
      integer	ilim(2)	! Integer autoscaling values
      integer	invalx	! Flag for invalid pixels in X image
      integer	invaly	! Flag for invalid pixels in Y image
      integer	ipinx	! Pointer to image plotted on x axis
      integer	ipiny	! Pointer to image plotted on y axis
      integer	ipwork	! Pointer to workspace holding pixel coords
      integer	ival	! Dummy integer argument
      integer	maxint	! Largest integer storable in an EDRS image
      integer	minint	! Smallest integer storable in an EDRS image
      integer	nlinx	! No. of lines in X image
      integer	nliny	! No. of lines in Y image
      integer	npixx	! No. of pixels per line in X image
      integer	npixy	! No. of pixels per line in Y image
      real	pcrang(2)! Percentage histogram points for autoscaling
      real	scalex	! Scale factor of X image
      real	scaley	! Scale factor of Y image
      character	title*30! Title for entire plot
      character	titlex*30! Title for X axis
      character	titley*30! Title for Y axis
      integer	wksize	! Size of work array
      real	xlims(2)! X axis limits
      real	ylims(2)! Y axis limits
      real	zerox	! Zero offset for X image
      real	zeroy	! Zero offset for Y image

      parameter	(maxint=32767,minint=-32768)

*
* DECLARE THE ARRAY TO HOLD THE IMAGE HISTOGRAMS
*
      integer	ihist(minint:maxint)

*
* OBTAIN THE UPPER AND LOWER HISTOGRAM POINTS FOR AUTOSCALING
*
      pcrang(1)=5.0
      pcrang(2)=95.0
      call gtlims('PCRANGE',pcrang,ierr)
 
*
* CONVERT TO VALUES IN THE RANGE 0.0 TO 1.0
*
      pcrang(1)=max(0.0,min(1.0,pcrang(1)*0.01))
      pcrang(2)=max(0.0,min(1.0,pcrang(2)*0.01))

*
* GET IMAGE TO PLOT ON X AXIS
*
      call gt2dir('XIMAGE',102,.false.,npixx,nlinx,ipinx,ierr)
      if(ierr.ne.0) goto 999
      call gtstds('XIMAGE',1,invalx,scalex,zerox,titlex)
      call gtinam('XIMAGE',titlex,ierr)

*
* CALL PCHIST TO FIND THE CORRESPONDING INTEGER VALUES IN THE X IMAGE
*
      ilim(1)=minint
      ilim(2)=maxint
      call pchist(%val(ipinx),npixx,nlinx,invalx,pcrang,ilim,2,ihist,
     :            minint,maxint,ierr)
      if(ierr.ne.0) then
         call wrerr('NOVALID')
         goto 999
      endif
 
*
* CONVERT TO DATA VALUES
*
      xlims(1)=ilim(1)*scalex+zerox
      xlims(2)=ilim(2)*scalex+zerox
       
*
* GET THE LIMITS OF THE X AXIS FROM THE USER
*
      call gtlims('XLIMS',xlims,ierr)

*
* GET IMAGE TO PLOT ON Y AXIS
*
      call gt2dir('YIMAGE',102,.false.,npixy,nliny,ipiny,ierr)
      if(ierr.ne.0) goto 999
      call gtstds('YIMAGE',1,invaly,scaley,zeroy,titley)
      call gtinam('YIMAGE',titley,ierr)
 
*
* CALL PCHIST TO FIND THE CORRESPONDING INTEGER VALUES IN THE Y  IMAGE
*
      ilim(1)=minint
      ilim(2)=maxint
      call pchist(%val(ipiny),npixy,nliny,invaly,pcrang,ilim,2,ihist,
     :            minint,maxint,ierr)
      if(ierr.ne.0) then
         call wrerr('NOVALID')
         goto 999
      endif
 
*
* CONVERT TO DATA VALUES
*
      ylims(1)=ilim(1)*scaley+zeroy
      ylims(2)=ilim(2)*scaley+zeroy
       
*
* GET THE LIMITS OF THE Y AXIS FROM THE USER
*
      call gtlims('YLIMS',ylims,ierr)

*
* GET TITLES FOR THE TWO AXES AND THE PLOT
*
      call rdkeyc('XTITLE',.true.,1,titlex,ival,ierr)
      call rdkeyc('YTITLE',.true.,1,titley,ival,ierr)
      title='Scatter plot'
      call rdkeyc('TITLE',.true.,1,title,ival,ierr)

*
* OPEN THE NCAR GRAPHICS PACKAGE
*
      call ncropn('DEVICE',device,.false.,ierr)
      if(ierr.ne.0) goto 999

*
* SELECT A NICE FONT
*
      call gtfont('FONT',ifont)
      call sgs_sfont(ifont)

*
* GENERTATE THE BACKGROUND PLOT
*
      call ncrbck(xlims(2),xlims(1),ylims(2),ylims(1),title,titlex,
     :            titley)

*
* GET WORKSPACE TO HOLD THE COORDINATES OF PLOTTED POINTS
*
      wksize=min(npixx,npixy)*min(nlinx,nliny)
      call gtwork('COORDS','REAL',2*wksize,ipwork,ierr)
      if(ierr.ne.0) goto 999

*
* CALL SCPLOT TO GENERATE THE SCATTER PLOT
*
      call scplot(%val(ipinx),npixx,nlinx,invalx,scalex,zerox,
     :            %val(ipiny),npixy,nliny,invaly,scaley,zeroy,
     :            %val(ipwork),wksize,xlims,ylims)

*
* CLOSE THE GRAPHICS PACKAGE
*
      call sgs_close

*
* RELEASE DATA AREAS AND FINISH
*
  999 call frdata(' ',ierr)

      end
