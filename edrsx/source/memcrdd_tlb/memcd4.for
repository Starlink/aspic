      subroutine memcd4(crddf,trfpsk,nsamp,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Sets up pointing information needed by MEMCC4. NB, this need
*	only be done for SURVEY data. AO data uses a different method.
*
*SOURCE
*       MEMCD4.FOR in MEMCRDD.TLB
*
*METHOD
*	For each boresight position, a linear transformation from focal
*	plane coords (in arcmins) to pixel coords in the final sky image
*	is calculated. The 6 coefficients needed to describe these
*	transformations are returned in the array "trfpsk". If no valid
*       transformation can be found for a sample, then all 6 
*       coefficients are set to zero. This ensures that all detectors
*       will have central pixel coordinates of (0,0) and thu sbe 
*       excluded from the deconvolution.
*
*	The transformations themselves are calculated from a cubic 
*	spline fit to the boresight pointing data stored with survey 
*	CRDD files, and the FITS descriptors of the output file.
*       
*ARGUMENTS       
*   INPUT:
*	crddf	integer		CRDD file index
*	nsamp	integer		The no. of different boresight positions
*				( = no. of samples per detector)
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	trfpsk(6,nsamp) real	The required transformations.
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/B2_COM/,/ZZ_COM/
*   READ/WRITE:
*	/BORE_SP/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*            boresp,splntr
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/10/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS, DETECTOR DATA, AND CRDD DESCRIPTORS.
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
      include 'UTILITIES(DS_COM)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... FITS DESCRIPTORS OF FINAL OUTPUT IMAGE
      include '(B0_COM)'

* ... INFO ABOUT THE INPUT CRDD FILES
      include '(B2_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	nsamp,crddf,ierr
      real	trfpsk(6,nsamp)

*
* DECLARE LOCAL VARIABLES
*
      integer	samp	! Current sample number
      integer   frame   ! Frame holding the CRDD file descriptors.
      real      hitime  ! Time of last boresight sample.
      real      lotime  ! Time of first boresight sample.
      real      time    ! Sample time relative to first sample.

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF NOT DEALING WITH SURVEY DATA RETURN IMMEDIATELY
*
      if(ZZ_typ.eq.'SURVEY') then

*
* SET UP THE COMMON BLOCK /BORE_SP/ HOLDING CUBIC SPLINE FITS TO
* THE BORESIGHT POINTING PARAMETERS FOR THE GIVEN CRDD FILE
*
         call boresp(B2_frm(crddf),ierr)
         if(ierr.ne.0) goto 999

*
* LOOP ROUND ALL THE SAMPLES FROM THIS CRDD FILE
*
         do samp=1,nsamp

*
* IF THE SAMPLE TIME IS OUTSIDE THE RANGE OF THE BORESIGHT SAMPLES
* THEN THE SAMPLE IS NOT USED.
*
            time=real(samp-1)/real(DT_srt(B2_bnd))
            frame=B2_frm(crddf)
            lotime=DS_but(1,frame)
            hitime=DS_but(DS_bsa(frame),frame)

            if(time.lt.lotime.or.time.gt.hitime) then
               trfpsk(1,samp)=0.0
               trfpsk(2,samp)=0.0
               trfpsk(3,samp)=0.0
               trfpsk(4,samp)=0.0
               trfpsk(5,samp)=0.0
               trfpsk(6,samp)=0.0

*
* IF THE SAMPLE IS WITHIN THE ALLOWED TIME RANGE, CALL SPLNTR TO SET 
* UP THE TRANSFORMATION FOR THIS SAMPLE.
*
            else
               call splntr(trfpsk(1,samp),samp,frame,B0_fit,B2_bnd,ierr)
               if(ierr.ne.0) goto 999
            endif

*
* NEXT SAMPLE
*
         enddo

      endif

*
* FINISH
*
  999 continue

      end
