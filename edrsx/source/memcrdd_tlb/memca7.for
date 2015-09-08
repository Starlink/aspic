      subroutine memca7(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Aquires a set of PSF images from the user.
*
*SOURCE
*       MEMCA7.FOR in MEMCRDD.TLB
*
*METHOD
*	At the moment, this routine just calls utility routine "GETPSF"
*	to get a standard PSF stack as produced by program PSFSTACK. In
*	the future it may be possible to save run-time by giving the 
*	PSF data in the final form required by OPUS and TROPUS, by 
*	reading in a file written by a previous run of MEMCRDD.
*	The width of the blank margin to put fround the data in the
*	final image frame is also calculated as 0.75 of the maximum 
*	(diagonal) dimension of any PSF in arcmins.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B2_COM/,/ZZ_COM/
*   WRITE:
*	/A7_COM/,
*		A7_psf		Pointers to the individual PSFs
*		A7_nps		Total no. of pixels in a PSF line
*		A7_nls          Total no. of lines in a PSF
*		A7_nds		No. of detector PSFs in the stack
*		A7_npx		No. of used pixels in a PSF line
*		A7_nln		No. of used lines in a PSF
*		A7_inv		Flag for invalid PSF pixels
*		A7_sca		Scale factor for PSFs
*		A7_zer		Zero offset for PSFs
*		A7_tr		PSF pixel to focal plane transformation
*		A7_mar		Half the maximum dimensions of the PSFs
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              getpsf,wruser
*       EDRS:
*              wrerr,lbgone
*              
*STARLINK PARAMETERS
*	BANDiPSF	PSF stack for IRAS band i (i=1-4)
*	A7ERR1(error)   Accessed if PSF stack is for wrong band
*	A7ERR2(error)	Accessed if PSF stack contains wrong no. of PSFs
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/9/89
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
* INCLUDE FOCAL PLANE INFO, ETC
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING ...

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(A7_COM)'

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	det		! Detector index
      real	disqu		! Max. squared diagonal size
      integer	iband		! IRAS band of given PSF stack
      integer	istat		! Temporary status value
      integer	nlin		! No. of used lines in current PSF
      integer	npix		! No. of used pixels in current PSF
      character	pname*8		! Parameter name for getting PSF stack
      character	prbuf*80	! Buffer for screen output
      real	z1,y1		! Focal plane coords of bottom left 
				! corner of current PSF (arcmins)
      real	z2,y2		! Focal plane coords of top right
				! corner of current PSF (arcmins)

*
* CHECK INHERITED STATUS VALUE
*
      if(ierr.ne.0) goto 999

*
* CREATE THE BAND-SPECIFIC PARAMETER NAME
*
      write(pname,10) B2_bnd
  10  format('BAND',I1,'PSF')

*
* GET PSF STACK AND DESCRIPTORS
*
      call getpsf(pname,IR_dts,A7_psf,A7_nps,A7_nls,A7_nds,A7_npx,
     :            A7_nln,A7_inv,A7_sca,A7_zer,A7_tr,iband,ierr)
      if(ierr.ne.0) goto 999

*
* CHECK THAT PSF STACK IS FOR THE RIGHT BAND, AND CONTAINS THE RIGHT
* NUMBER OF PSFS
*
      if(iband.ne.B2_bnd) then
         call wrerr('A7ERR1')
         ierr=1
      else if(A7_nds.ne.DT_bns(B2_bnd)) then
         call wrerr('A7ERR2')
         ierr=1
      endif
      if(ierr.ne.0) goto 999

*
* CALCULATE THE SIZE OF THE MARGIN FOR THE FINAL IMAGE FRAME (=0.75 THE
* MAXIMUM DIAGONAL OF ALL PSFS)
*
      disqu=0.0
      do det=1,A7_nds

         nlin=A7_nln(det)
         npix=A7_npx(det)

         z1=A7_tr(1,det) + A7_tr(2,det) + A7_tr(3,det)
         y1=A7_tr(4,det) + A7_tr(5,det) + A7_tr(6,det)

         z2=A7_tr(1,det) + npix*A7_tr(2,det) + nlin*A7_tr(3,det)
         y2=A7_tr(4,det) + npix*A7_tr(5,det) + nlin*A7_tr(6,det)

         disqu=max(disqu,(z1-z2)**2+(y1-y2)**2)

      enddo

      A7_mar=0.75*sqrt(disqu)

*
* IF REQUIRED, TELL USER THE WIDTH OF THE MARGIN
*
      if(ZZ_ilv.ge.3) then
         write(prbuf,20) A7_mar
  20     format('  Blank margin round final image has width ',F5.2,
     :          ' arcmins')
         call lbgone(prbuf(44:))
         call wruser(' ',istat)
         call wruser(prbuf,istat)
      endif

*
* FINISH
*
 999  continue

      end
