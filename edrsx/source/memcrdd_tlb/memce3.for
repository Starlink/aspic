      subroutine memce3(datin,datout,sky,skyfft,datfft,work,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Projects the given Jy/ST data into an image using PSFs, then 
*	samples the image to produce the output data set (in Jy/st).
*
*SOURCE
*       MEMCE3.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	datin	real	Input data set (in Jy/St)
*	datfft	real	Image work space used to hold FFT of projected data
*	skyfft	real	Image work space used to hold FFT of final sky
*	work	real	Image work space used by FFT routines
*	ierr	integer	Inherited status
*   OUTPUTS:
*	datout	real	Output data set (in Jy/St)
*       sky	real	Smoothed image
*	ierr	integer	Output status: 0 for success
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
*       D.S. Berry (MAVAD::DSB) 19/12/89
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
      integer	ierr
      real	sky(B0_nps,B0_nls),datin(ME_mk),datout(ME_mk),
     :          skyfft(B0_nls,B0_nps),datfft(B0_nls,B0_nps),
     :          work(B0_nps,B0_nls)

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
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

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
* STORE THE DATA VALUE
*
            datval=datin(sample)

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
         call memcc6(PR_inv,sky,datfft,work,B0_nps,B0_nls,ierr)
         if(ierr.ne.0) goto 999

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
      call memcc7(PR_fwd,skyfft,sky,work,B0_nps,B0_nls,ierr)

*
* SAMPLE THE SMOOTHED IMAGE AT THE POSITION OF EACH DATA SAMPLE CENTRE
*
      do sample=1,ME_mk
         call memcc9(sample,c1,c2,c3,c4,pixel,line)
         datout(sample) = c1*sky(pixel,line)
     :                   +c2*sky(pixel+1,line)
     :                   +c3*sky(pixel,line+1)
     :                   +c4*sky(pixel+1,line+1)
      enddo

*
* FINISH
*
  999 continue

      end

