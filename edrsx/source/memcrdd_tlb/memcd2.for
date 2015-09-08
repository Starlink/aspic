      subroutine memcd2(sky,data,skyfft,datfft,work)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Applies the transposed response matrix to a data set to get
*	an image.
*
*SOURCE
*       MEMCD2.FOR in MEMCRDD.TLB
*
*METHOD
*	This version uses Fourier transforms to perform the 
*	convolutions. The data samples are split into groups,
*	each group having a single PSF. However, the solid angle
*	for each individual detector is retained, i.e. just the
*	PSF shapes are averaged, not the total sensitivity.
*
*       Pseudo code:
*            Initialise image A to all zeros
*            For each sample group, do...
*               Initialise image B to all zeros
*               For each sample within the current group, do...
*                  Project the sample value into image B
*               Next sample
*               Take inverse Fourier transform of image B
*               Multiply by the transform of the PSF for this group
*               Add the product to image A
*            Next group
*            Take forward Fourier transform of image A to get result
*       
*ARGUMENTS       
*   INPUT:
*	data	real	Input data set
*	datfft	real	Image work space used to hold FFT of projected data
*	skyfft	real	Image work space used to hold FFT of final sky
*	work	real	Image work space used by FFT routines
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
     :          datfft(B0_nls,B0_nps),work(B0_nps,B0_nls)

*
* DECLARE LOCAL VARIABLES
*
      real	c1	! Contribution from bottom left pixel
      real	c2	! Contribution from bottom right pixel
      real	c3	! Contribution from top left pixel
      real	c4	! Contribution from top right pixel
      real	datval	! Data value
      integer	group	! Current sample group number
      integer	istat	! Temporary status value
      integer	line	! Line no. within sky image
      integer	pixel	! Pixel no. within sky image
      integer	sample	! Sample index within data set

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

*
* GO ROUND FOR NEXT DATA SAMPLE
*
         enddo

*
* TAKE INVERSE FFT OF PROJECTED DATA IMAGE
*
         call memcc6(PR_inv,sky,datfft,work,B0_nps,B0_nls,istat)
         if(istat.ne.0) stop '*** Execution aborted within TROPUS'

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
      if(istat.ne.0) stop '*** Execution aborted within TROPUS'

*
* FINISH
*
      end

