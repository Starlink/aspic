      subroutine memcf3(pname,null,file,rinval,bdfnam,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Gets a 2D image from the user and stores it in a specified
*	internal image file. NB, the image must have FITS
*	descriptors equivalent to those of the output image being 
*	generated.
*          The parameter value is not cancelled by this routine, the
*	calling routine should do that if necessary.
*
*SOURCE
*       MEMCF3.FOR in MEMCRDD.TLB
*       
*ARGUMENTS       
*   INPUT:
*	pname	character	INTERIM parameter name to use
*	null	logical		If true, then user may give no file
*	file(B0_nps,B0_nls) real The internal file in which to store
*				the image.
*	rinval	real		The value with which to replace invalid 
*				pixels
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*	bdfnam	character	Name of BDF containing image
*       ierr    integer         Exit status: 0 - success
*				             1 - null value given
*					    100- Too many bad images given
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,gt2dir,i2torl,gtinam,xytord
*       EDRS:
*              gtdscr,wrerr
*
*STARLINK PARAMETERS
*	'pname'		Argument "pname" contains name of parameter used
*			to get the image
*	F3ERR1(error)   Accessed if the descriptors of the given image
*			don't agree with those of the output image.
*	TOOBAD(error) 	Accessed if too many bad input images given.
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/1/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... FITS DESCRIPTORS OF OUTPUT IMAGE
      include '(B0_COM)'

* ... MEMSYS3 INFO
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      logical	null
      character	pname*(*),bdfnam*(*)
      integer	ierr
      real	rinval,file(B0_nps,B0_nls)

*
* DECLARE LOCAL VARIABLES
*
      real	bscale	! Scale factor for input image
      real	bzero	! Offset for input image
      integer	c	! Corner index (1 to 3)
      character	cval*1	! Dummy character argument
      real	ddec	! Difference in corner DEC values in arcmins
      real	dec1	! DEC value in degs of output image corner
      real	dec2	! DEC value in degs of input image corner
      integer	desc	! Descriptor index
      character desnam(7)*6 ! FITS descriptor names
      real	dra	! Difference in corner RA values in arcmins
      real	infits(7)! FITS descriptors from input image
      integer	inval	! Flag for invalid pixels in the input image
      integer	ipin	! Pointer to the input image
      integer	ival	! Dummy integer argument
      integer	istat	! Temporary status value
      real	lin(3)	! Line numbers of three image corners
      integer	nbad	! No. of bad images given by user
      integer	nlin	! No. of lines in the input image
      integer	npix	! No. of pixels per line in the input image
      logical	ok	! True if input image has same FITS descriptors
			! as the output image.
      real	pix(3)	! Pixel numbers of three image corners
      character	prbuf*80! Buffer for screen output
      real	ra1	! RA of corner of output image (in degrees)
      real	ra2	! RA of corner of input image (in degrees)
      real	relerr	! Relative error between input and output 
			! descriptor values
      real	rval	! Dummy real argument

*
* DEFINE FITS DESCRIPTOR NAMES
*
      data desnam/'CRVAL1','CRVAL2','CRPIX1','CRPIX2','CDELT1','CDELT2',
     :            'CROTA1'/

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* GET THE IMAGE FROM THE USER
*
      nbad=0
  10  call gt2dir(pname,102,null,npix,nlin,ipin,ierr)
      if(ierr.eq.0) then

*
* CHECK THE IMAGE SIZE IS CORRECT
*
         ok=.true.

         if(npix.ne.B0_nps) then
            ok=.false.
            write(prbuf,20) npix,B0_nps
  20        format(' *** NAXIS1 is: ',I6,', should be: ',I6)
            call wruser(prbuf,istat)
         endif

         if(nlin.ne.B0_nls) then
            ok=.false.
            write(prbuf,30) nlin,B0_nls
  30        format(' *** NAXIS2 is: ',I6,', should be: ',I6)
            call wruser(prbuf,istat)
         endif

*
* IF A GOOD IMAGE WAS OBTAINED OK, THEN GET ITS FITS AND EDRS DESCRIPTORS
*
         if(ok) then
            call gtdscr(pname,'BSCALE','REAL',ival,bscale,cval,istat)
            call gtdscr(pname,'BZERO','REAL',ival,bzero,cval,istat)
            call gtdscr(pname,'INVAL','INTEGER',inval,rval,cval,istat)
            call gtdscr(pname,'CRVAL1','REAL',ival,infits(1),cval,istat)
            call gtdscr(pname,'CRVAL2','REAL',ival,infits(2),cval,istat)
            call gtdscr(pname,'CRPIX1','REAL',ival,infits(3),cval,istat)
            call gtdscr(pname,'CRPIX2','REAL',ival,infits(4),cval,istat)
            call gtdscr(pname,'CDELT1','REAL',ival,infits(5),cval,istat)
            call gtdscr(pname,'CDELT2','REAL',ival,infits(6),cval,istat)
            call gtdscr(pname,'CROTA1','REAL',ival,infits(7),cval,istat)

*
* CHECK THAT THE RA AND DEC OF THREE CORNERS AGREE TO WITHIN HALF A 
* PIXEL SIZE
*
            pix(1)=1
            pix(2)=1
            pix(3)=B0_nps
            lin(1)=1
            lin(2)=B0_nls
            lin(3)=1

            do c=1,3
               if(ok) then

                  call xytord(pix(c),lin(c),B0_fit(1),B0_fit(2),
     :                        B0_fit(3),B0_fit(4),B0_fit(5),B0_fit(6),
     :                        B0_fit(7),ra1,dec1)

                  call xytord(pix(c),lin(c),infits(1),infits(2),
     :                        infits(3),infits(4),infits(5),infits(6),
     :                        infits(7),ra2,dec2)

                  dra=60.0*abs(ra1-ra2)*cosd(0.5*(dec1+dec2))
                  ddec=60.0*abs(dec1-dec2)               
                  if(dra.gt.0.5*ZZ_psz.or.ddec.gt.0.5*ZZ_psz) ok=.false.

               endif
            enddo
         endif

*
* IF THEY DONT AGREE GIVE A MESSAGE AND GO ROUND FOR ANOTHER INPUT
* UPTO A MAXIMUM OF PR_BAD TIMES
*
         if(.not.ok) then
            ierr=100
            call wrerr('F3ERR1')
            if(nbad.lt.PR_bad) then
               nbad=nbad+1
               call cnpar(pname,istat)
               goto 10
            else
               call wrerr('TOOBAD')
            endif

*
* IF THE DESCRIPTORS DO AGREE, COPY THE IMAGE INTO THE SPECIFIED FILE
* AND GET THE NAME OF THE BDF
*
         else
            ierr=0
            call i2torl(%val(ipin),npix,nlin,bscale,bzero,inval,
     :                  rinval,file)
            call gtinam(pname,bdfnam,istat)
            if(istat.ne.0) bdfnam='The given image'
         endif

      endif

*
* FINISH
*
  999 continue

      end
