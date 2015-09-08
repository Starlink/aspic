      subroutine fourie
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs forward and inverse Fourier transforms on 2D data
*	corresonding to purely real images in image space.
*
*SOURCE
*       FOURIE.FOR in FOURIER.TLB
*
*METHOD
*       The program is based on a NAG Hermitian FFT routine. It
*	utilises the symetry present in the FFT of a purely real image
*       to reduce the size of the problem by a factor of 4 (2 in each
*	dimension). The resulting transform is initially returned as a 
*	single image in which each quadrant consisting of a linear
*	combination of the real and imaginary parts of the transform.
*	This form is useful if the user just wants to multiply the FFT
*	by some know purely real mask and then invert it to get a 
*	filtered image. However, if the user wants to multiply the FFT
*	by a complex mask (eg the FFT of another image), or do any other
*	operation involving combining complex values, then the Hermitian
*	FFT image must be untangled into seperate real and imaginary 
*	parts.
*
*       The program provides options for untangling this image into
*	either real and imaginary images, or power and phase images.
*	This untangling process reduces the CPU gain over conventional
*	non-Hermitian FFTs from around a factor of 4 to around 2.
*
*	When performing an inverse transform, the input FFT can be given
*	as either a single Hermitian image, or as a pair of images
*	containing either real and imaginary parts, or power and phase.
*
*	Invalid pixels in the image space input, are replaced by a user
*	specified value before taking the FFT.
*
*	Images need not be either square, or be a power of 2 in size.
*	Howver, there are some restrictions on the shape and size of
*	the input image, and if these restrictions are not met then the
*	input image is padded out to the next larger acceptable size.
*	This padding is done with the same value used to replace 
*	invalid pixels.
*
*	There is an option to swap the quadrants of the input image 
*	around before performing a forward FFT. This is useful if the
*	user wants to perform convolutions with the FFTs, since the PSF
*	image can be created with the PSF centre at the image centre, 
*	rather than at pixel (1,1) as is usually required.
*
*ARGUMENTS
*   none
*
*SUBROUTINES 
*       THIS PACKAGE (UTILITIES.TLB):
*	   gtstrn,gt2dir,gt2diw,wruser,i2prod,gtwork
*       THIS PACKAGE (FOURIER.TLB):
*	   imsize,prepim,quads,fftfor,powpha,realim,hermit,fftrev
*       EDRS:
*	   getpar,wrerr,gtstds,lbgone,ptdscr
*       INTERIM:
*	   cydscr,rdkeyc,frdata
*
*STARLINK PARAMETERS
*	ILEVEL	 	User information level
*	INVERSE		If YES or TRUE then inverse transform required
*	FILLVAL		Invalid pixels are set to this value before
*			transforming
*	SHIFT		Yes if transform origin is to be image centre
*	INPUT		REAL input image for forward transformation
*	HERMOUT		Hermitian output image from forward transform
*	REALOUT		Real output frame
*	IMAGOUT		Imaginary output frame
*	POWEROUT	Image containing output power spectrum
*	PHASEOUT	Image containing output phase
*	HM_TITLE	Title for Hermitian output frame
*	RL_TITLE	Title for real output frame
*	IM_TITLE	Title for imaginary output frame
*	PW_TITLE	Title for power output
*	PH_TITLE	Title for phase output
*	OUTPUT		Real space domain output
*	TITLE		Title for OUTPUT
*	HERMIN		Hermitian frequency domain input
*	REALIN		Real input frame
*	IMAGIN		Imaginary input frame
*	POWERIN		Image containing input power spectrum
*	PHASEIN		Image containing input phase
*	NOTSAME(error)	Input frequency space images are not same size
*	TOOBAD(error)	Too many bad values given. Aborting!
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/6/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE LOCAL VARIABLES
*
      character cval    ! Dummy character argument
      integer   dim(2)  ! Dimensions of output images
      real      fill    ! Fill value for invalid pixels etc
      integer   ierr    ! STatus flag
      integer   ilevel  ! User information level
      character init*1  ! Used by NAG routine C06FRF
      integer   inval   ! Invalid pixel flag in forward trans input image
      integer   invalh  ! Flag for invalid pixels in HERMITIAN input
      integer   invali  ! Flag for invalid pixels in IMAGINARY input
      integer   invalr  ! Flag for invalid pixels in REAL input
      logical   invers  ! True if an inverse transformation required
      integer   ip      ! Pointer to real version of forward trans input
      integer   iph     ! Pointer to REAL version of HERMITIAN input
      integer   ipho    ! Pointer to HERMITIAN output of forward trans
      integer   ipi     ! Pointer to double precision IMAGINARY input
      integer   ipin    ! Pointer to I*2 input image for forward trans
      integer   ipinh   ! Pointer to I*2 HERMITIAN input image
      integer   ipio    ! Pointer to integer*2 IMAGINARY output
      integer   ipo     ! Pointer to I*2 output from inverse transform
      integer   ippo    ! Pointer to I*2 power output image
      integer   ipr     ! Pointer to double precision REAL input
      integer   ipro    ! Pointer to integer*2 REAL output
      integer   ipw1    ! Pointer to real work array
      integer   ipw2    ! Pointer to real work array
      integer   ipini   ! Pointer to IMAGINARY input
      integer   ipinr   ! Pointer to REAL input
      integer   ipzo    ! Pointer to I*2 phase output image
      integer   isth    ! 0 - Hermitian i/p given, 1 - if null i/p given
      integer   isti    ! 0 - Imag i/p given, 1 - null imag i/p
      integer   istr    ! 0 - Real i/p given, 1 - null real i/p
      integer   ival    ! Dummy integer argument
      integer   lwork1  ! Length of double precision work array
      integer   nlin    ! No. of lines in input image for forward trans
      integer   nlini   ! No. of lines in IMAGINARY input
      integer   nlinr   ! No. of lines in REAL input
      real      norm    ! Normalisation factor for freq. space data
      integer   npix    ! No. of pixels in input image for forward trans
      integer   npixi   ! No. of pixels in IMAGINARY input
      integer   npixr   ! No. of pixels in REAL input
      integer   npnts   ! Total no. of pixels in output images
      logical   pandp   ! true if power and phase inputs given
      character prbuf*80! Buffer for output to screen
      integer   quart   ! 1 if quadrants are to swapped
      real      rval    ! Dummy real argument
      real      scale   ! Scale factor from forward trans input image
      real      scaleh  ! Scale factor for hermitian output
      real      scalei  ! Scale factor for imaginary input and output
      real      scaler  ! Scale factor for REAL input and output
      integer   stposn  ! Position of selected string in option list
      character title*30! Title from forward trans input image
      character titleh*30! Title from hermitian output
      character titlei*30! Title from IMAGINARY input and output
      character titler*30! Title from REAL input and output
      logical   untang  ! true if real or imaginary outputs required
      real      zero    ! Zero offset from forward trans input image
      real      zeroh   ! Zero offset for Hermitian output
      real      zeroi   ! Zero offset for IMAGINARY input and output
      real      zeror   ! Zero offset for REAL input and output

*
* GET USER INFORMATION LEVEL
*
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
      if(ierr.ne.0) goto 999

*
* SEE IF FORWARD OR REVERSE TRANSFORMATION IS REQUIRED
*
      stposn=2
      call gtstrn('INVERSE',.true.,'YES,NO,TRUE,FALSE.',1,stposn,cval,
     :             ival,ierr)
      if(ierr.ne.0) then
         goto 999
      else
         if(mod(stposn,2).eq.0) then
            invers=.false.
         else
            invers=.true.
         endif
      endif

*
* SEE IF QUADRANTS OF REAL IMAGE ARE TO BE SWAPPED
*
      stposn=2
      call gtstrn('SHIFT',.true.,'YES,NO,TRUE,FALSE.',1,stposn,cval,
     :             ival,ierr)
      if(ierr.ne.0) then
         goto 999
      else
         quart=mod(stposn,2)
      endif

*
* GET VALUE WITH WHICH TO REPLACE INVALID PIXELS
*
      fill=0
      call getpar('FILLVAL','REAL',1,-1.0e30,1.0e30,.true.,ival,fill,
     :             ierr)

*
*----------------------------------------------------------------------
* IF FORWARD TRANSFORMATION REQUIRED THEN...
*
      if(.not.invers) then

*
* GET THE (PURELY REAL) INPUT IMAGE
*
         call gt2dir('INPUT',102,.false.,npix,nlin,ipin,ierr)
         if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif
         call gtstds('INPUT',1,inval,scale,zero,title)

*
* CALCULATE SIZE OF OUTPUT IMAGES WHICH CAN BE COPED WITH BY NAG ROUTINE
*
         call imsize(npix,dim(1),ierr)
         if(ierr.eq.0) call imsize(nlin,dim(2),ierr)
         if(ierr.ne.0) goto 999

         if(ilevel.ge.2.and.(npix.ne.dim(1).or.nlin.ne.dim(2))) then
            write(prbuf,10) dim(1),dim(2)
  10        format('    The image will be padded to ',I6,' by ',I6)
            call lbgone(prbuf(43:))
            call lbgone(prbuf(33:))
            call wruser(' ',ierr)
            call wruser('    The transform routine cannot cope with'//
     :                  ' the given image size.',ierr)
            call wruser(prbuf,ierr)
            call wruser(' ',ierr)
         endif

*
* SEE WHICH OUTPUT IMAGES USER REQUIRES, SETTING POINTER TO ZERO IF
* NOT REQUIRED.
*
* 1st: HERMITAN OUTPUT
*
         call gt2diW('HERMOUT',102,.true.,dim(1),dim(2),ipho,ierr)
         if(ierr.eq.1) then
            ipho=0
         else if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif

*
* 2nd: REAL PART OF COMPLEX OUTPUT
*
         call gt2diW('REALOUT',102,.true.,dim(1),dim(2),ipro,ierr)
         if(ierr.eq.1) then
            ipro=0
         else if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif

*
* 3nd: IMAGINARY PART OF COMPLEX OUTPUT
*
         call gt2diW('IMAGOUT',102,.true.,dim(1),dim(2),ipio,ierr)
         if(ierr.eq.1) then
            ipio=0
         else if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif

*
* 4th: POWER
*
         call gt2diW('POWEROUT',102,.true.,dim(1),dim(2),ippo,ierr)
         if(ierr.eq.1) then
            ippo=0
         else if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif

*
* 5th: PHASE
*
         call gt2diW('PHASEOUT',102,.true.,dim(1),dim(2),ipzo,ierr)
         if(ierr.eq.1) then
            ipzo=0
         else if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif

*
* IF NON REQUIRED QUIT
*
         if(ipho.eq.0.and.ipro.eq.0.and.ipio.eq.0.and.
     :      ippo.eq.0.and.ipzo.eq.0) goto 999

*
* SEE IF HERMITIAN VERSION WILL HAVE TO BE UNTANGLED INTO COMPLEX PARTS
*
         if(ipro.ne.0.or.ipio.ne.0.or.ippo.ne.0.or.ipzo.ne.0) then
            untang=.true.
         else
            untang=.false.
         endif

*
* GET WORK SPACE
*
         npnts=dim(1)*dim(2)

         call gtwork('R','REAL',npnts,ip,ierr)
         if(ierr.eq.0) call gtwork('W1','REAL',npnts,ipw1,ierr)
         if(ierr.eq.0) call gtwork('W2','REAL',npnts,ipw2,ierr)
         if(ierr.ne.0) goto 999

*
* COPY INPUT IMAGE INTO REAL WORK ARRAY, SCALING THE
* DATA AND REPLACING BLANK PIXELS BY THE FILL VALUE. THE IMAGE IS
* ALSO PADDED OUT TO THE CALCULATED SIZE WITH PIXELS CONTAINING THE
* FILL VALUES.
*
         if(quart.eq.1) then

*
* IF REQUIRED SWAP THE QUADRANTS OF THE INPUT IMAGE ROUND TO PRODUCE
* A SHIFT IN THE ORIGIN OF THE TRANSFORM FROM PIXEL (1,1) TO PIXEL
* (INT((M+1)/2),INT((N+1)/2)
*
            if(ilevel.eq.3) call wruser('Preparing input image',ierr)

            call prepim(%val(ipin),npix,nlin,inval,scale,zero,
     :                  %val(ipw1),dim(1),dim(2),fill,0)

            if(ilevel.eq.3) call wruser('Quartering input image',ierr)

            call quads(%val(ipw1),%val(ip),dim(1),dim(2),npix,nlin,
     :                  quart)
         else

            if(ilevel.eq.3) call wruser('Preparing input image',ierr)

            call prepim(%val(ipin),npix,nlin,inval,scale,zero,
     :                  %val(ip),dim(1),dim(2),fill,0)
         endif

*
* DO THE TRANSFORMATION
*
         if(ilevel.eq.3) call wruser('Calling main routine',ierr)
         call fftfor(%val(ip),dim(1),dim(2),%val(ipw1),%val(ipw1),
     :               %val(ipw2),untang,ilevel,ierr)
         if(ierr.eq.0) then
            if(ilevel.eq.3) call wruser('Back from main routine',ierr)

*
* PRODUCE REQUIRED OUTPUTS IN I*2 FORMAT. NB SCALE AND ZERO FACTORS ARE
* MULTIPLIED BY THE SQUARE ROOT OF THE NO. OF PIXELS IN THE OUTPUT TO
* ENSURE THAT THE CENTRE OUTPUT PIXEL HAS A VALUE EQUAL TO THE TOTAL
* DATA SUM IN THE INPUT
*
* 1st: HERMITIAN OUTPUT
*
            norm=sqrt(real(npnts))
            if(ipho.ne.0) then
               if(ilevel.eq.3) call wruser('Producing Hermitian output',
     :                                      ierr)
               call i2prod(%val(ip),dim(1),dim(2),0.0,scaleh,
     :                     zeroh,%val(ipho),0)
               if(ilevel.eq.3) call wruser('Adding descriptors',ierr)
               call cydscr('INPUT','HERMOUT',ierr)
               call ptdscr('HERMOUT','NAXIS1','INTEGER',dim(1),rval,
     :                      cval,ierr)
               call ptdscr('HERMOUT','NAXIS2','INTEGER',dim(2),rval,
     :                      cval,ierr)
               call ptdscr('HERMOUT','INVAL','INTEGER',-100000,rval,
     :                      cval,ierr)
               call ptdscr('HERMOUT','BSCALE','REAL',ival,scaleh*norm,
     :                      cval,ierr)
               call ptdscr('HERMOUT','BZERO','REAL',ival,zeroh*norm,
     :                      cval,ierr)
               titleh=title
               call rdkeyc('HM_TITLE',.true.,1,titleh,ival,ierr)
               call ptdscr('HERMOUT','TITLE','CHARACTER',ival,rval,
     :                      titleh,ierr)
            endif

*
* 2st: REAL PART OF COMPLEX OUTPUT
*
            if(ipro.ne.0) then
               if(ilevel.eq.3) call wruser('Producing Real output',ierr)
               call i2prod(%val(ipw2),dim(1),dim(2),0.0,scaler,
     :                     zeror,%val(ipro),0)
               if(ilevel.eq.3) call wruser('Adding descriptors',ierr)
               call cydscr('INPUT','REALOUT',ierr)
               call ptdscr('REALOUT','NAXIS1','INTEGER',dim(1),rval,
     :                      cval,ierr)
               call ptdscr('REALOUT','NAXIS2','INTEGER',dim(2),rval,
     :                      cval,ierr)
               call ptdscr('REALOUT','INVAL','INTEGER',-100000,rval,
     :                      cval,ierr)
               call ptdscr('REALOUT','BSCALE','REAL',ival,scaler*norm,
     :                      cval,ierr)
               call ptdscr('REALOUT','BZERO','REAL',ival,zeror*norm,
     :                      cval,ierr)
               titler=title
               call rdkeyc('RL_TITLE',.true.,1,titler,ival,ierr)
               call ptdscr('REALOUT','TITLE','CHARACTER',ival,rval,
     :                      titler,ierr)
            endif

*
* 3rd: IMAGINARY PART OF COMPLEX OUTPUT
*
            if(ipio.ne.0) then
               if(ilevel.eq.3) call wruser('Producing Imaginary output',
     :                                      ierr)
               call i2prod(%val(ipw1),dim(1),dim(2),0.0,scalei,zeroi,
     :                     %val(ipio),0)
               if(ilevel.eq.3) call wruser('Adding descriptors',ierr)
               call cydscr('INPUT','IMAGOUT',ierr)
               call ptdscr('IMAGOUT','NAXIS1','INTEGER',dim(1),rval,
     :                      cval,ierr)
               call ptdscr('IMAGOUT','NAXIS2','INTEGER',dim(2),rval,
     :                      cval,ierr)
               call ptdscr('IMAGOUT','INVAL','INTEGER',-100000,rval,
     :                      cval,ierr)
               call ptdscr('IMAGOUT','BSCALE','REAL',ival,scalei*norm,
     :                      cval,ierr)
               call ptdscr('IMAGOUT','BZERO','REAL',ival,zeroi*norm,
     :                      cval,ierr)
               titlei=title
               call rdkeyc('IM_TITLE',.true.,1,titlei,ival,ierr)
               call ptdscr('IMAGOUT','TITLE','CHARACTER',ival,rval,
     :                      titlei,ierr)
            endif

*
* 4TH: POWER AND PHASE OUTPUTS
*
            if(ippo.ne.0.or.ipzo.ne.0) then

               if(ilevel.eq.3) call wruser('Calculating power and phase'
     :                                     ,ierr)
               call powpha(%val(ipw2),%val(ipw1),dim(1),dim(2))

               if(ippo.ne.0) then
                  if(ilevel.eq.3) call wruser('Producing Power output',
     :                                         ierr)
                  call i2prod(%val(ipw2),dim(1),dim(2),0.0,scalei,zeroi,
     :                        %val(ippo),0)
                  if(ilevel.eq.3) call wruser('Adding descriptors',ierr)
                  call cydscr('INPUT','POWEROUT',ierr)
                  call ptdscr('POWEROUT','NAXIS1','INTEGER',dim(1),rval,
     :                         cval,ierr)
                  call ptdscr('POWEROUT','NAXIS2','INTEGER',dim(2),rval,
     :                         cval,ierr)
                  call ptdscr('POWEROUT','INVAL','INTEGER',-100000,rval,
     :                         cval,ierr)
                  call ptdscr('POWEROUT','BSCALE','REAL',ival,
     :                         scalei*norm,cval,ierr)
                  call ptdscr('POWEROUT','BZERO','REAL',ival,zeroi*norm
     :                         ,cval,ierr)
                  titlei=title
                  call rdkeyc('PW_TITLE',.true.,1,titlei,ival,ierr)
                  call ptdscr('POWEROUT','TITLE','CHARACTER',ival,rval,
     :                         titlei,ierr)
               endif

               if(ipzo.ne.0) then
                  if(ilevel.eq.3) call wruser('Producing Phase output',
     :                                         ierr)
                  call i2prod(%val(ipw1),dim(1),dim(2),0.0,scalei,zeroi,
     :                        %val(ipzo),0)
                  if(ilevel.eq.3) call wruser('Adding descriptors',ierr)
                  call cydscr('INPUT','PHASEOUT',ierr)
                  call ptdscr('PHASEOUT','NAXIS1','INTEGER',dim(1),rval,
     :                         cval,ierr)
                  call ptdscr('PHASEOUT','NAXIS2','INTEGER',dim(2),rval,
     :                         cval,ierr)
                  call ptdscr('PHASEOUT','INVAL','INTEGER',-100000,rval,
     :                         cval,ierr)
                  call ptdscr('PHASEOUT','BSCALE','REAL',ival,scalei,
     :                         cval,ierr)
                  call ptdscr('PHASEOUT','BZERO','REAL',ival,zeroi,cval,
     :                         ierr)
                  titlei=title
                  call rdkeyc('PH_TITLE',.true.,1,titlei,ival,ierr)
                  call ptdscr('PHASEOUT','TITLE','CHARACTER',ival,rval,
     :                         titlei,ierr)
               endif
            endif
         endif

*
*-------------------------------------------------------------------
* IF INVERSE TRANSFORMATION REQUIRED THEN ....
*
      else

*
* GET POINTER TO "HERMITIAN" INPUT IMAGE AND IMAGE DESCRIPTORS.
*
         call gt2dir('HERMIN',102,.true.,npix,nlin,ipinh,isth)

         if(isth.eq.0) then
            call gtstds('HERMIN',1,invalh,scaleh,zeroh,titleh)

         else if(isth.ne.1) then
            call wrerr('TOOBAD')
            goto 999

         endif

*
* IF NO HERMITIAN INPUT WAS GIVEN GET POINTER TO "REAL" AND "IMAGINARY
* INPUT IMAGES AND IMAGE DESCRIPTORS . FIRST THE REAL IMAGE:
*
         if(isth.eq.1) then

            call gt2dir('REALIN',102,.true.,npixr,nlinr,ipinr,istr)

            if(istr.eq.0) then
               call gtstds('REALIN',1,invalr,scaler,zeror,titler)

            else if(istr.ne.1) then
               call wrerr('TOOBAD')
               goto 999

            endif

*
* NEXT THE IMAGINARY IMAGE
*
            call gt2dir('IMAGIN',102,.true.,npixi,nlini,ipini,isti)

            if(isti.eq.0) then
               call gtstds('IMAGIN',1,invali,scalei,zeroi,titlei)

            else if(isti.ne.1) then
               call wrerr('TOOBAD')
               goto 999

            endif

*
* IF NO REAL OR IMAGINARY INPUT WAS GIVEN GET POINTER TO POWER AND
* PHASE INPUT IMAGES AND IMAGE DESCRIPTORS . FIRST THE POWER IMAGE:
*
            pandp=.false.

            if(istr.eq.1.and.isti.eq.1) then
               pandp=.true.

               call gt2dir('POWERIN',102,.true.,npixr,nlinr,ipinr,istr)

               if(istr.eq.0) then
                  call gtstds('POWERIN',1,invalr,scaler,zeror,titler)

               else if(istr.ne.1) then
                  call wrerr('TOOBAD')
                  goto 999

               endif

*
* NEXT THE PHASE IMAGE
*
               call gt2dir('PHASEIN',102,.true.,npixi,nlini,ipini,isti)

               if(isti.eq.0) then
                  call gtstds('PHASEIN',1,invali,scalei,zeroi,titlei)

               else if(isti.ne.1) then
                  call wrerr('TOOBAD')
                  goto 999

               endif

            endif

*
* IF GIVEN, CHECK THEY ARE THE SAME SIZE
*
            if(istr.eq.0.or.isti.eq.0) then
               if(isti.eq.0) then
                  npix=npixi
                  nlin=nlini
               else if(istr.eq.0) then
                  npix=npixr
                  nlin=nlinr
               else
                  if(npixr.ne.npixi.or.nlinr.ne.nlini) then
                     call wrerr('NOTSAME')
                     goto 999
                  else
                     npix=npixr
                     nlin=nlinr
                  endif
               endif
            endif
         endif

*
* IF NO INPUTS GIVEN QUIT
*
         if(isth.eq.1.and.istr.eq.1.and.isti.eq.1) goto 999

*
* CALCULATE SIZE OF OUTPUT IMAGES WHICH CAN BE COPED WITH BY NAG ROUTINE
*
         call imsize(npix,dim(1),ierr)
         if(ierr.eq.0) call imsize(nlin,dim(2),ierr)
         if(ierr.ne.0) goto 999

         if(ilevel.ge.2.and.(npix.ne.dim(1).or.nlin.ne.dim(2))) then
            write(prbuf,10) dim(1),dim(2)
            call lbgone(prbuf(43:))
            call lbgone(prbuf(33:))
            call wruser(' ',ierr)
            call wruser('    The transform routine cannot cope with'//
     :                  ' the given image size.',ierr)
            call wruser(prbuf,ierr)
            call wruser(' ',ierr)
         endif

*
* GET WORK SPACE
*
         npnts=dim(1)*dim(2)

         call gtwork('H','REAL',npnts,iph,ierr)

         if(isth.eq.1) then
            if(ierr.eq.0) call gtwork('I','REAL',npnts,ipi,ierr)
            if(ierr.eq.0) call gtwork('R','REAL',npnts,ipr,ierr)
         endif

         if(ierr.eq.0) call gtwork('W1','REAL',npnts,ipw1,ierr)
         if(ierr.ne.0) goto 999

*
* PREPARE THE INPUT IMAGES.
* COPY INPUT IMAGES INTO REAL WORK ARRAYS, SCALING THE
* DATA AND REPLACING BLANK PIXELS BY THE FILL VALUE. THE IMAGES ARE
* ALSO PADDED OUT TO THE CALCULATED SIZE WITH PIXELS CONTAINING THE
* FILL VALUES. IF EITHER IMAGE IN A PAIR WERE NOT GIVEN, THE
* CORRESPONDING REAL IMAGE IS FILLED WITH ZEROS. THE DATA VALUES
* HAVE THE NORMALISATION REMOVED WHICH WAS INTRODUCED IN THE FORWARD
* TRANSFORM.
*
         norm=1.0/sqrt(real(npnts))

         if(isth.eq.0) then

            if(ilevel.eq.3) call wruser('Preparing input',ierr)
            call prepim(%val(ipinh),npix,nlin,invalh,scaleh*norm,
     :                  zeroh*norm,%val(ipw1),dim(1),dim(2),fill,0)

         else if(istr.eq.0.or.isti.eq.0) then

            if(.not.pandp) then

               if(ilevel.eq.3) call wruser('Preparing Real input',ierr)
               call prepim(%val(ipinr),npix,nlin,invalr,scaler*norm,
     :                     zeror*norm,%val(ipr),dim(1),dim(2),fill,istr)

               if(ilevel.eq.3) call wruser('Preparing Imaginary input',
     :                                      ierr)
               call prepim(%val(ipini),npix,nlin,invali,scalei*norm,
     :                     zeroi*norm,%val(ipi),dim(1),dim(2),fill,isti)

*
* IF POWER AND PHASE INPUTS WERE GIVEN CONVERT TO REAL AND IMAGINARY
* NOTE, NORMALISATION IS SQUARED FOR POWER AND IS UNITY FOR PHASE.
*
            else

               if(ilevel.eq.3) call wruser('Preparing Power input',ierr)
               call prepim(%val(ipinr),npix,nlin,invalr,scaler*norm,
     :                    zeror*norm,%val(ipr),dim(1),dim(2),fill,istr)

               if(ilevel.eq.3) call wruser('Preparing Phase input',ierr)
               call prepim(%val(ipini),npix,nlin,invali,scalei,
     :                     zeroi,%val(ipi),dim(1),dim(2),fill,isti)

               if(ilevel.eq.3) call wruser('Calculating complex data',
     :                                      ierr)
               call realim(%val(ipr),%val(ipi),dim(1),dim(2))

            endif

*
* PRODUCE A HERMITIAN IMAGE FROM THE REAL AND IMAGINARY IMAGES
*
            if(ilevel.eq.3) call wruser('Creating Hermitian image',ierr)
            call hermit(%val(ipr),%val(ipi),dim(1),dim(2),%val(ipw1),
     :                  ilevel)
         endif

*
* DO THE TRANSFORM
*
         if(ilevel.eq.3) call wruser('Calling main routine',ierr)

         call fftrev(%val(iph),dim(1),dim(2),%val(ipw1),
     :               ilevel,ierr)

         if(ierr.eq.0) then
            if(ilevel.eq.3) call wruser('Back from main routine',ierr)

*
* PRODUCE REAL OUTPUT IMAGE.
*
            call gt2diW('OUTPUT',102,.false.,dim(1),dim(2),ipo,ierr)
            if(ierr.ne.0) then
               call wrerr('TOOBAD')
               goto 999
            endif

*
* IF REQUIRED, SWAP THE QUADRANTS ROUND
*
            if(quart.eq.1) then

               if(ilevel.eq.3) call wruser('Quartering output',ierr)
               call quads(%val(ipw1),%val(iph),dim(1),dim(2),npix,nlin,
     :                    -quart)

               if(ilevel.eq.3) call wruser('Producing output',ierr)
               call i2prod(%val(iph),dim(1),dim(2),0.0,scale,zero,
     :                     %val(ipo),0)

            else

               if(ilevel.eq.3) call wruser('Producing output',ierr)
               call i2prod(%val(ipw1),dim(1),dim(2),0.0,scale,zero,
     :                     %val(ipo),0)

            endif

*
* ADD DESCRIPTORS TO THE FINAL OUTPUT IMAGE
*
            if(ilevel.eq.3) call wruser('Adding descriptors',ierr)

            if(ipinh.ne.0) then
               call cydscr('HERMIN','OUTPUT',ierr)
               title=titleh

            else if(ipinr.ne.0) then
               if(pandp) then
                  call cydscr('POWERIN','OUTPUT',ierr)
               else
                  call cydscr('REALIN','OUTPUT',ierr)
               endif
               title=titler

            else if(ipini.ne.0) then
               if(pandp) then
                  call cydscr('PHASEIN','OUTPUT',ierr)
               else
                  call cydscr('IMAGIN','OUTPUT',ierr)
               endif
               title=titlei

            endif

            call ptdscr('OUTPUT','NAXIS1','INTEGER',dim(1),rval,cval,
     :                  ierr)
            call ptdscr('OUTPUT','NAXIS2','INTEGER',dim(2),rval,cval,
     :                  ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',-100000,rval,cval,
     :                  ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,scale,cval,
     :                  ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,zero,cval,ierr)
            call rdkeyc('TITLE',.true.,1,title,ival,ierr)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,
     :                   ierr)
         endif
      endif

*
* RELEASE ALL DATA AREAS AND FINISH
*
  999 call frdata(' ',ierr)

      end
