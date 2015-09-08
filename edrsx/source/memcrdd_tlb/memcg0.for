      subroutine memcg0(k,j,crdreq)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculate an image from a given data set using only data
*	from the specified CRDD file.
*
*SOURCE
*       MEMCG0.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	j	integer		Internal file no. of the output image
*	k	integer		Internal file no. of the input data set
*	crdreq  integer		Index of required CRDD file
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	       wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              g0work(source attached)
*	EDRS:
*	       lbgone
*
*VAX SPECIFICS
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 24/4/90
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

* ... POINTERS TO MEMCRDD INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	j,k,crdreq

*
* DECLARE LOCAL VARIABLES
*
      integer	istat	! Local error status
      character	prbuf*80! Buffer for screen output

*
* IF REQUIRED TELL THE USER WHAT IS GOING ON
*
      if(ZZ_ilv.ge.5) then
         write(prbuf,10) ME_ntr+1
  10     format('    Entering MEMCG0 (transform no. ',I10,' )')
         call lbgone(prbuf(36:))
         call wruser(prbuf,istat)
      endif

*
* CALL G0WORK TO DO THE WORK
*
      call g0work(ME_st(ME_kb(j)),ME_st(ME_kb(k)),ME_st(B6_wk1),
     :            ME_st(B6_wk2),ME_st(B6_wk3),crdreq)

*
* FINISH
*
      end




      subroutine g0work(sky,data,skyfft,datfft,work,crdreq)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Applies the transposed response matrix to a data set to get
*	an image, using only data from a given CRDD file.
*
*METHOD
*       As MEMCD2 except the SDC values for each sample are examined
*	to see if the sample comes from the right CRDD file before
*	including it in the map.
*       
*ARGUMENTS       
*   INPUT:
*	data	real	Input data set
*	datfft	real	Image work space used to hold FFT of projected data
*	skyfft	real	Image work space used to hold FFT of final sky
*	work	real	Image work space used by FFT routines
*	crdreq  integer	Index for required CRDD file
*   OUTPUTS:
*       sky	real	Output sky image
*
*COMMON USAGE
*   READ:
*	/A8_COM/,/B0_COM/,/B5_COM/,/B6_COM/,/ME_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcc6,memcc7,memcc9,memcd0
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
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

* ... NUMBER OF SAMPLE GROUPS
      include '(A8_COM)'

* ... OUTPUT IMAGE FITS DESCRIPTORS
      include '(B0_COM)'

* ... START AND END OF EACH GROUP
      include '(B5_COM)'

* ... POINTERS TO MEMCRDD FILES STORED IN ME_ST
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      real	sky(B0_nps,B0_nls),data(ME_mk),skyfft(B0_nls,B0_nps),
     :          datfft(B0_nls,B0_nps),work(B0_nps,B0_nls)
      integer	crdreq

*
* DECLARE LOCAL VARIABLES
*
      real	c1	! Contribution from bottom left pixel
      real	c2	! Contribution from bottom right pixel
      real	c3	! Contribution from top left pixel
      real	c4	! Contribution from top right pixel
      integer	crddf	! CRDD file index of current sample
      real	datval	! Data value
      integer	group	! Current sample group number
      integer	istat	! Temporary status value
      integer	line	! Line no. within sky image
      integer	pixel	! Pixel no. within sky image
      integer	sample	! Sample index within data set
      integer	sdc	! SDC identifier (see MEMCB5)

*
* SET SUM OF FFT OF SMOOTHED PROJECTED DATA IMAGES TO ZERO
*
      do pixel=1,B0_nps
         do line=1,B0_nls
            skyfft(line,pixel)=0.0
         enddo
      enddo

*
* LOOP ROUND EACH DETECTOR GROUP
*
      do group=1,A8_ngp

*
* IF THIS GROUP IS EMPTY, PASS ON
*
         if(b5_lgs(group).eq.0) goto 10

*
* SET IMAGE HOLDING PROJECTED DATA VALUES TO ZERO
*
         do line=1,B0_nls
            do pixel=1,B0_nps
               sky(pixel,line)=0.0
            enddo
         enddo

*
* PROJECT THE DATA SAMPLES INTO AN IMAGE USING THE SAME COEFFICIENTS
* AS WERE USED TO GENERATE THE SAMPLES IN ROUTINE OP1
*
         do sample=B5_fgs(group),B5_lgs(group)

*
* CHECK THIS SAMPLE COMES FROM THE REQUIRED CRDD FILE
*
            sdc=ME_sti(B6_sdc+sample-1)
            crddf=mod(sdc/16,B5_cfa)+1

            if(crddf.eq.crdreq) then

*
* CALCULATE THE CONTRIBUTIONS TO THE BI-LINEARLY INTERPOLATED VALUE 
* FROM THE FOUR SKY PIXELS NEAREST TO THE THE DATA SAMPLE CENTRE 
*
               call memcc9(sample,c1,c2,c3,c4,pixel,line)

*
* MULTIPLY THE DATA VALUE BY THE SOLID ANGLE
*
               datval=data(sample)*ME_st(B6_sol+sample-1)

*
* ADD THE CONTRIBUTION TO EACH PIXEL FROM THE CURRENT DATA SAMPLE
* INTO THE TOTAL IMAGE
*
               sky(pixel,line)=sky(pixel,line)+c1*datval
               sky(pixel+1,line)=sky(pixel+1,line)+c2*datval
               sky(pixel,line+1)=sky(pixel,line+1)+c3*datval
               sky(pixel+1,line+1)=sky(pixel+1,line+1)+c4*datval

            endif
*
* GO ROUND FOR NEXT DATA SAMPLE
*
         enddo

*
* TAKE INVERSE FFT OF PROJECTED DATA IMAGE
*
         call memcc6(PR_inv,sky,datfft,work,B0_nps,B0_nls,istat)
         if(istat.ne.0) stop '*** Execution aborted within MEMCG0'

*
* MULTIPLY THE TRANSFORM BY THE PSF TRANSFORM, PUTTING THE RESULTS
* BACK IN THE ARRAY "DATFFT"
*
         call memcd0(datfft,ME_st(B6_psf(group)),datfft,B0_nps,B0_nls)

*
* ADD THE FFT OF THE SMOOTHED PROJECTED DATA IMAGE TO THE SUM OF SUCH
* IMAGES
*
         do pixel=1,B0_nps
            do line=1,B0_nls
               skyfft(line,pixel)=skyfft(line,pixel)+datfft(line,pixel)
            enddo
         enddo

*
* DO NEXT GROUP
*
  10     continue

      enddo

*
* TAKE FORWARD FFT TO GET FINAL IMAGE
*
      call memcc7(PR_fwd,skyfft,sky,work,B0_nps,B0_nls,istat)
      if(istat.ne.0) stop '*** Execution aborted within MEMCG0'

*
* FINISH
*
      end

