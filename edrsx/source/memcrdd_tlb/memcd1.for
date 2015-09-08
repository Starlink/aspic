      subroutine memcd1(sky,data,skyfft,prdfft,smooth,work)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Generates a set of data from a given image using the IRAS
*	PSFs
*
*SOURCE
*       MEMCD1.FOR in MEMCRDD.TLB
*
*METHOD
*	This version uses Fourier transforms to perform the 
*	convolutions. The data samples are split into groups,
*	each group having a single PSF. However, the solid angle
*	for each individual detector is retained, i.e. just the
*	PSF shapes are averaged, not the total sensitivity.
*
*       Pseudo code:
*            Take FFT of trial sky
*            For each sample group, do...
*               Multiply trial sky FFT by FFT of this groups PSF
*               Take inverse FFT of the product (=smoothed image)
*               For each sample within the current group, do...
*                  Sample the smoothed sky at the position of this 
*                       data sample using bi-linear interpolation
*               Next sample
*            Next group
*       
*ARGUMENTS       
*   INPUT:
*	sky	real	Image holding input sky
*	skyfft  real	Image work space to hold FFT of sky
*	prdfft  real	Image work space to hold FFT of smoothed sky
*	smooth	real	Image work space to hold smoothed sky
*	work	real	Image work space used in FFT routines
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
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/89
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

*
* DECLARE LOCAL VARIABLES
*
      real	c1	! Contribution from bottom left pixel
      real	c2	! Contribution from bottom right pixel
      real	c3	! Contribution from top left pixel
      real	c4	! Contribution from top right pixel
      real	datval	! Data value in Jy/st
      integer	group	! Current sample group number
      integer	istat	! Temporary status value
      integer	line	! Line no. within sky image
      integer	pixel	! Pixel no. within sky image
      integer	sample	! Sample index within data set

*
* TAKE FORWARD FFT OF INPUT SKY IMAGE
*
      call memcc6(PR_fwd,sky,skyfft,work,B0_nps,B0_nls,istat)
      if(istat.ne.0) stop '*** Execution aborted within OPUS'

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
         if(istat.ne.0) stop '*** Execution aborted within OPUS'

*
* SAMPLE THE SMOOTHED IMAGE AT THE POSITION OF EACH DATA SAMPLE CENTRE
*
         do sample=B5_fgs(group),B5_lgs(group)

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
     :              +c2*smooth(pixel+1,line)
     :              +c3*smooth(pixel,line+1)
     :              +c4*smooth(pixel+1,line+1)

*
* MULTIPLY THE SAMPLE VALUE BY THE SOLID ANGLE OF THE DETECTOR TO GET 
* THE DATA VALUE IN JY
*
            data(sample)=datval*ME_st(B6_sol+sample-1)

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
