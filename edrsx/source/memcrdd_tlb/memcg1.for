      subroutine memcg1(j,k,crdreq)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculate a data set from a given image, but only the data 
*	samples coming from a given crdd file.
*
*SOURCE
*       MEMCG1.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	j	integer		Internal file no. of the input image
*	k	integer		Internal file no. of the output data set
*	crdreq	integer		Index of required crdd file
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	       wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              g1work
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
  10     format('    Entering MEMCG1(transform no. ',I10,' )')
         call lbgone(prbuf(36:))
         call wruser(prbuf,istat)
      endif

*
* CALL G1WORK TO DO THE WORK
*
      call g1worK(ME_st(ME_kb(j)),ME_st(ME_kb(k)),ME_st(B6_wk1),
     :            ME_st(B6_wk2),ME_st(B6_wk3),ME_st(B6_wk4),crdreq)

*
* FINISH
*
      end



      subroutine g1work(sky,data,skyfft,prdfft,smooth,work,crdreq)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Generates a set of data from a given image using the IRAS
*	PSFs. Only data for the required CRDD file is generated.
*
*METHOD
*	Like MEMCd1, except a check is made on each samples SDC
*	identifier before generating a sample value.
*
*ARGUMENTS       
*   INPUT:
*	sky	real	Image holding input sky
*	skyfft  real	Image work space to hold FFT of sky
*	prdfft  real	Image work space to hold FFT of smoothed sky
*	smooth	real	Image work space to hold smoothed sky
*	work	real	Image work space used in FFT routines
*       crdreq	integer	Required CRDD file index
*   OUTPUTS:
*	data	real	Output data set
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
     :          smooth(B0_nps,B0_nls),prdfft(B0_nls,B0_nps),
     :          work(B0_nps,B0_nls)
      integer	crdreq

*
* DECLARE LOCAL VARIABLES
*
      real	c1	! Contribution from bottom left pixel
      real	c2	! Contribution from bottom right pixel
      real	c3	! Contribution from top left pixel
      real	c4	! Contribution from top right pixel
      integer	crddf	! Samples crdd file index
      real	datval	! Data value in Jy/st
      integer	group	! Current sample group number
      integer	istat	! Temporary status value
      integer	line	! Line no. within sky image
      integer	pixel	! Pixel no. within sky image
      integer	sample	! Sample index within data set
      integer	sdc	! Samples SDC identifer (see memcb5)

*
* TAKE FORWARD FFT OF INPUT SKY IMAGE
*
      call memcc6(PR_fwd,sky,skyfft,work,B0_nps,B0_nls,istat)
      if(istat.ne.0) stop '*** Execution aborted within MEMCG1'

*
* LOOP ROUND EACH DETECTOR GROUP
*
      do group=1,A8_ngp

*
* IF THIS GROUP IS EMPTY. PASS ON
*
         if(B5_lgs(group).eq.0) goto 10

*
* MULTIPLY THE SKY TRANSFORM BY THE PSF TRANSFORM, 
*
         call memcd0(skyfft,ME_st(B6_psf(group)),prdfft,B0_nps,B0_nls)

*
* TAKE INVERSE FFT TO GET SMOOTHED SKY IMAGE
*
         call memcc7(PR_inv,prdfft,smooth,work,B0_nps,B0_nls,istat)
         if(istat.ne.0) stop '*** Execution aborted within MEMCG1'

*
* SAMPLE THE SMOOTHED IMAGE AT THE POSITION OF EACH DATA SAMPLE CENTRE
*
         do sample=B5_fgs(group),B5_lgs(group)

*
* CHECK THIS SAMPLE IS FROM THE REQUIRED CRDD FILE
*
            sdc=ME_sti(b6_sdc+sample-1)
            crddf=mod(sdc/16,B5_cfa)+1

            if(crddf.eq.crdreq) then

*
* CALCULATE THE CONTRIBUTIONS TO THE BI-LINEARLY INTERPOLATED VALUE 
* FROM THE FOUR SKY PIXELS NEAREST TO THE THE DATA SAMPLE CENTRE
*
               call memcc9(sample,c1,c2,c3,c4,pixel,line)

*
* CALCULATE THE INTERPOLATED VALUE OF THE SMOOTHED IMAGE AND STORE IN
* THE OUTPUT DATA ARRAY
*
               datval = c1*smooth(pixel,line)
     :                 +c2*smooth(pixel+1,line)
     :                 +c3*smooth(pixel,line+1)
     :                 +c4*smooth(pixel+1,line+1)

*
* MULTIPLY THE SAMPLE VALUE BY THE SOLID ANGLE OF THE DETECTOR TO GET 
* THE DATA VALUE IN JY
*
               data(sample)=datval*ME_st(B6_sol+sample-1)

            endif

*
* GO ROUND FOR DATA SAMPLE
*
         enddo

*
* DO NEXT DETECTOR GROUP
*
  10     continue

      enddo

*
* FINISH
*
      end
