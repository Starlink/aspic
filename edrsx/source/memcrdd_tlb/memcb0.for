      subroutine memcb0(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Set up FITS descriptos describing the output image frame.
*
*SOURCE
*       MEMCB0.FOR in MEMCRDD.TLB
*
*METHOD
*	Set up default descriptors such that all the data is encompassed
*	with a suitable sized margin. Give the user a chance to override
*	these defaults. Finally, ensure that the NAG FFT routines can
*	cope with the image size. If not, increase the image size to 
*	the next allowable value. The FITS descriptors are:
*
*	fits(1) - CRVAL1 - RA of reference point (in degrees)
*	fits(2) - CRVAL2 - DEC of reference point (in degrees)
*	fits(3) - CRPIX1 - Pixel no. of reference point
*	fits(4) - CRPIX2 - Line no. of reference point
*	fits(5) - CDELT1 - Size of a pixel in RA direction (degrees of arc)
*	fits(6) - CDELT2 - Size of a pixel in DEC direction (degrees of arc)
*	fits(7) - CROTA1 - Clockwise angle from north to -ve image Y axis 
*			   in degrees (assuming  east to north is clockwise)
*       
*ARGUMENTS       
*   INPUTS:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A7_COM/,/ZZ_COM/
*   WRITE:
*	/B0_COM/
*		B0_fit - FITS descriptors of output image
*		B0_nps - No. of pixels per line in output image
*		B0_nls - No. of lines in output image
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              imsize,gtradc,wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcb4
*       EDRS:
*              lbgone,getpar
*              
*STARLINK PARAMETERS
*	BOXSIZE		Size of final (square) image in arcmins
*	RA_HRS		Hours field of user specified RA
*	RA_MINS		Minutes field of user specified RA
*	RA_SECS		Seconds field of user specified RA
*	DEC_DEG		Degrees field of user specified DEC
*	DEC_MIN		Minutes field of user specified DEC
*	DEC_SEC		Seconds field of user specified DEC
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/9/89
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

* ... SIZE OF BLANK "SAFETY MARGIN" AROUND THE OUTPUT  IMAGE
      include '(A7_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(B0_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real	boxsiz  ! Image size actually used
      real	deccen  ! DEC of image centre (in degreees)
      real	defbox	! Default image size in arcmins
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      integer	nlused	! No. of image lines actually used
      integer	npused	! No. of pixels per image line actually used
      character	prbuf*80! Buffer for screen output
      real	racen	! RA of image centre (in hours)

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* SET UP SOME DEFAULT FITS PARAMETERS DESCRIBING THE OUTPUT IMAGE SUCH 
* THAT IT ENCOMPASSES ALL THE DATA IN THE INPUT CRDD FILES (LEAVING A 
* SUITABLE MARGIN).
*
      call memcb4(ZZ_cro,B0_nps,B0_nls,B0_fit,ierr)
      if(ierr.ne.0) goto 999

*
* SEE IF USER WISHES TO DEFINE AN ALTERNATIVE OUTPUT FRAME. NB, THE
* BOX SIZE SPECIFIED BY THE USER DOES NOT INCLUDE THE BLANK MARGIN
*
      defbox=min(B0_nps,B0_nls)*ZZ_psz-2.0*A7_mar

      boxsiz=defbox
      call getpar('BOXSIZE','REAL',1,4.0*ZZ_psz,1.0E6,.true.,ival,
     :             boxsiz,istat)

*
* IF SO, GET THE RA AND DEC OF THE CENTRE OF THE OUTPUT FRAME REQUIRED, 
* AND UPDATE THE FITS DESCRIPTORS IF SUCCESFULL
*
      if(boxsiz.ne.defbox) then
         racen=B0_fit(1)/15.0
         deccen=B0_fit(2)
         call gtradc(' ',racen,deccen,.true.,.false.,istat)
         if(istat.le.1) then
            B0_nps=int((boxsiz+2.0*A7_mar)/ZZ_psz)+1
            B0_nls=B0_nps
            B0_fit(1)=15.0*racen
            B0_fit(2)=deccen
            B0_fit(3)=B0_nps/2.0
            B0_fit(4)=B0_nls/2.0
         endif
      endif

*
* THE NAG FFT ROUTINES CAN ONLY COPE WITH CERTAIN IMAGE SIZES. INCREASE
* THE SIZE OF THE OUTPUT FRAME IF REQUIRED, TO THE NEXT LEGAL SIZE.
*
      call imsize(B0_nps,npused,ierr)
      if(ierr.ne.0) goto 999

      call imsize(B0_nls,nlused,ierr)
      if(ierr.ne.0) goto 999
      
*
* TELL THE USER THE IMAGE SIZE
*
      if(ZZ_ilv.ge.2) then
         write(prbuf,30) B0_nps,B0_nls,npused,nlused
  30     format('  Output frame size increased from (',I4,',',I4,
     :          ') to (',I4,',',I4,')' )
         call lbgone(prbuf(58:))
         call lbgone(prbuf(53:))
         call lbgone(prbuf(43:))
         call lbgone(prbuf(38:))
         call wruser(prbuf,istat)
         call wruser(' ',istat)
      endif

*
* MODIFY THE FITS DESCRIPTORS TO TAKE ACCOUNT OF THE NEW FRAME SIZE
*
      B0_fit(3)=int(B0_fit(3)+(npused-B0_nps)/2.0)
      B0_fit(4)=int(B0_fit(4)+(nlused-B0_nls)/2.0)
      B0_nps=npused
      B0_nls=nlused

*
* FINISH
*
  999 continue

      end
